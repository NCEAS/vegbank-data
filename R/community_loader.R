library(tidyverse)
library(here)
library(stringr)
library(googlesheets4)
source("R/build_loader_table.R")


# load in CDFW data -----------------------------------------------------------

load_files <- function(in_dir) {
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  # files needed for CommunityClassifications
  plot_files <- dir(sub_folders, full.names = TRUE) %>%
    grep(pattern = "RAPlots.csv", value = TRUE)
  
  classification_files <- dir(sub_folders, full.names = TRUE) %>%
    grep(pattern = "RAClassification.csv", value = TRUE)
  
  project_files <- dir(sub_folders, full.names = TRUE) %>%
    grep(pattern = "RAProjects.csv", value = TRUE)
  
  # read + combine
  plots_df_list <- lapply(
    plot_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE,
    col_types = cols(`PlotOther5` = col_character())
  )
  plots <- do.call(bind_rows, plots_df_list)
  
  classification_df_list <- lapply(
    classification_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE
  )
  classification <- do.call(bind_rows, classification_df_list)
  
  project_df_list <- lapply(
    project_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE
  )
  projects <- do.call(bind_rows, project_df_list)
  
  # create blank Loader Table dataframe -----------------------------------------------------
  community_template_fields <- build_loader_table(
    sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
    sheet = "CommunityClassifications",
    source_df = classification
  )
  
  community_LT <- community_template_fields$template
  
  plots <<- plots
  classification <<- classification
  projects <<- projects
  community_LT <<- community_LT
  
  return(classification)
}

normalize_projects_classification <- function(projects) {
  
  text_map <- c(
    inspectionText = "surveys were keyed using",
    multivariateAnalysisText = paste(
      'See "Vegetation Map and Classification of Fish',
      'These data were analyzed using multivariate cluster analysis, performed',
      'See: Classification of the Vegetation Alliances',
      'These data were analyzed using a number of statistical methods, chiefly an',
      'CNPS analyzed the species cover data using PC-Ord and R software',
      sep = "|"
    )
  )
  
  projects_proj <- projects %>%
    mutate(
      inspectionText = if_else(
        coalesce(str_detect(ClassificationDescription, text_map["inspectionText"]), FALSE),
        ClassificationDescription,
        NA_character_
      ),
      multivariateAnalysisText = if_else(
        coalesce(str_detect(ClassificationDescription, text_map["multivariateAnalysisText"]), FALSE),
        ClassificationDescription,
        NA_character_
      )
    ) %>%
    group_by(ProjectCode) %>%
    summarise(
      expert_system = first(ClassificationTool),
      inspection = first(inspectionText),
      multivariate_analysis = first(multivariateAnalysisText),
      .groups = "drop"
    )
  
  # any ClassificationDescription that didn't match either bucket?
  unmatched <- projects %>%
    filter(!is.na(ClassificationDescription), str_squish(ClassificationDescription) != "") %>%
    mutate(
      matched_any = coalesce(str_detect(ClassificationDescription, text_map["inspectionText"]), FALSE) |
        coalesce(str_detect(ClassificationDescription, text_map["multivariateAnalysisText"]), FALSE)
    ) %>%
    filter(!matched_any) %>%
    distinct(ClassificationDescription) %>%
    pull(ClassificationDescription)
  
  if (length(unmatched) > 0) {
    cli::cli_alert_warning(
      "Some Project ClassificationDescription values did not match inspection or multivariate patterns ({length(unmatched)} unique values)."
    )
    cli::cli_text("Sample (up to 10):")
    cli::cli_text(paste0("- ", head(unmatched, 10)))
  }
  
  return(projects_proj)
}

# plots (class_confidence)

normalize_class_confidence <- function(plots) {
  
  plots_conf <- plots %>%
    transmute(
      SurveyID,
      class_confidence = case_when(
        Confidence_ID %in% c("Not recorded") ~ NA_character_,
        is.na(Confidence_ID) ~ NA_character_,
        Confidence_ID == "H" ~ "High",
        Confidence_ID == "M" ~ "Medium",
        Confidence_ID == "L" ~ "Low",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(SurveyID) %>%
    summarise(
      class_confidence = first(class_confidence),
      .groups = "drop"
    )
  
  allowed <- c("H", "M", "L", "Not recorded", "<Null>", "", NA)
  weird_vals <- setdiff(unique(plots$Confidence_ID), allowed)
  
  if (length(weird_vals) > 0) {
    cli::cli_alert_warning(
      "Confidence_ID has unexpected values not in {H,M,L} (and not recorded/null) ({length(weird_vals)} unique values)."
    )
    cli::cli_text(paste0("- ", weird_vals))
  }
  
  return(plots_conf)
}