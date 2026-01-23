library(tidyverse)
library(here)
library(stringr)
library(googlesheets4)
library(cli)
library(glue)
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
    cli_alert_warning("Some Project ClassificationDescription values did not match patterns ({length(unmatched)} unique).")
    cli_text("Sample (up to 3):")
    cli_text(paste0("- ", str_trunc(head(unmatched, 3), 120)))
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
    cli_alert_warning(
      "Confidence_ID has unexpected values not in H/M/L (and not recorded/null) ({length(weird_vals)} unique values)."
    )
    cli_text(paste0("- ", weird_vals))
  }
  
  return(plots_conf)
}

load_reference_tables <- function(in_dir){
  # Community concepts from VegBank (should be loaded already)
  # TODO: replace this with something that doesn't rely on a local file. we should either include the vegbank code that generates the file, or write that file out to the curation dir
  cc_all_path <- here("data", "cc_all.csv")
  if (!file.exists(cc_all_path)) {
    stop("Missing data/cc_all.csv. Generate it (or copy it) before running.")
  }
  cc_all <- read_csv(cc_all_path, show_col_types = FALSE)
  
  cc_current <- cc_all %>%
    filter(current_accepted == TRUE)
  
  # CA code map (Google Sheet)
  cacode_sheet_url <- "https://docs.google.com/spreadsheets/d/1LsDQL3NxRjJ32eyRuVPyjtQgq8cqcyhjAIZiw9MYg-0/edit?gid=755803824#gid=755803824"
  
  cacode_map_raw <- read_sheet(cacode_sheet_url, sheet = 1)
  
  cacode_map <- cacode_map_raw %>%
    mutate(
      CaCode_norm = str_squish(str_to_lower(CaCode)),
      NVC_norm = str_squish(str_to_lower(as.character(`2009/NVC_Code`)))
    ) %>%
    filter(!is.na(CaCode_norm), CaCode_norm != "", !is.na(NVC_norm), NVC_norm != "")
  
  # warn on duplicate CaCodes
  dup <- cacode_map %>%
    count(CaCode_norm) %>%
    filter(n > 1)
  
  if (nrow(dup) > 0) {
    cli_alert_warning("cacode_map has duplicate CaCode values ({nrow(dup)} codes). Mapping may be ambiguous.")
    cli_text("Sample duplicates:")
    cli_text(paste0("- ", head(dup$CaCode_norm, 10)))
  }
  
  # cc lookup table
  cc_lookup <- cc_current %>%
    mutate(comm_code_norm = str_squish(str_to_lower(as.character(comm_code)))) %>%
    filter(!is.na(comm_code_norm), comm_code_norm != "") %>%
    distinct(comm_code_norm, .keep_all = TRUE) %>%
    select(cc_code, comm_code_norm)
  
  list(
    cacode_map = cacode_map,
    cc_lookup = cc_lookup
  )
}

assign_vb_cc_code <- function(classification, cacode_map, cc_lookup){
  
  cacode_map_1to1 <- cacode_map %>%
    group_by(CaCode_norm) %>%
    summarise(
      NVC_norm = first(NVC_norm),
      n_map = n(),
      .groups = "drop"
    )
  
  # warn if any CaCodes map to multiple NVC codes
  multi <- cacode_map %>%
    count(CaCode_norm, name = "n_map") %>%
    filter(n_map > 1)
  
  if (nrow(multi) > 0) {
    cli_alert_warning(
      "Some CaCode values map to multiple NVC codes in cacode_map ({nrow(multi)} CaCodes). Using the first NVC code for now to avoid row duplication."
    )
    cli_text(paste0("- ", head(multi$CaCode_norm, 10)))
  }
  
  classification_norm <- classification %>%
    mutate(CaCode_norm = str_squish(str_to_lower(CaCode))) %>%
    left_join(cacode_map_1to1 %>% select(CaCode_norm, NVC_norm), by = "CaCode_norm") %>%
    mutate(comm_code_norm = NVC_norm) %>%
    left_join(cc_lookup, by = "comm_code_norm") %>%
    mutate(vb_cc_code = cc_code)
  
  # warn if lots of NAs
  na_ct <- sum(is.na(classification_norm$vb_cc_code))
  if (na_ct > 0) {
    cli_alert_warning("vb_cc_code is NA for {na_ct} rows (out of {nrow(classification_norm)}).")
  }
  
  classification_norm
}

join_classifications <- function(classification_with_cc, plots_conf, projects_proj){
  out <- classification_with_cc %>%
    left_join(plots_conf, by = "SurveyID") %>%
    left_join(projects_proj, by = "ProjectCode")

  if (nrow(out) != nrow(classification_with_cc)) {
    stop(glue(
      "Row count changed after joins: {nrow(classification_with_cc)} -> {nrow(out)}. ",
      "This indicates duplicates in join keys (SurveyID or ProjectCode)."
    ))
  }
  
  out
}

community_loader <- function(in_dir){
  
  classification <- load_files(in_dir)
  
  projects_proj <- normalize_projects_classification(projects)
  plots_conf <- normalize_class_confidence(plots)
  
  refs <- load_reference_tables(in_dir)
  classification_with_cc <- assign_vb_cc_code(
    classification = classification,
    cacode_map = refs$cacode_map,
    cc_lookup = refs$cc_lookup
  )
  
  class_cc_proj <- join_classifications(
    classification_with_cc = classification_with_cc,
    plots_conf = plots_conf,
    projects_proj = projects_proj
  )
  
  # TODO: possibly improve messaging here?
  stopifnot(nrow(class_cc_proj) == nrow(community_LT))
  stopifnot(all(names(c("expert_system","inspection","multivariate_analysis","class_confidence","vb_cc_code")) %in% names(class_cc_proj)))
  
  # Assigning columns to loader table -------------------------------------------
  community_LT$expert_system <- class_cc_proj$expert_system
  community_LT$inspection <- class_cc_proj$inspection
  community_LT$multivariate_analysis <- class_cc_proj$multivariate_analysis
  community_LT$class_confidence <- class_cc_proj$class_confidence
  community_LT$vb_cc_code <- class_cc_proj$vb_cc_code
  
  # save filled in loader table
  out_path <- file.path(out_dir, "communityClassificationsLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(community_LT, out_path)
}