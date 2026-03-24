library(tidyverse)
library(stringr)
library(cli)
library(here)

# load in CDFW data -----------------------------------------------------------

#' Reads and combines RAPlots.csv, RAClassification.csv, and RAProjects.csv
#' files from VegBank
#' 
#' @param in_dir Directory of VegBank data to read from. Can be a full or relative to working directory. 
#' 
#'   A full file path is the complete location of a folder on your computer,
#'   starting from the top level (e.g., "C:/Users/yourname/Documents/VegBankData"
#'   or "/Users/yourname/Documents/VegBankData").
#'   
#'   A relative file path is the location of the folder relative to your current
#'   R project or working directory. For example, if your data folder is inside
#'   your project, you might use "VegBank/data".
#'   
#' 
#' @return Named list with three elements:
#'   \describe{
#'     \item{plots}{Combined RAPlots data}
#'     \item{classification}{Combined RAClassification data}
#'     \item{projects}{Combined RAProjects data (deduplicated by ProjectCode)}
#'   }
load_community_files <- function(in_dir) {
  
  # make path project-root relative (will still use full file paths if provided)
  in_dir <- here::here(in_dir)
  
  # files needed for CommunityClassifications
  plot_files <- dir(in_dir, full.names = TRUE, recursive = TRUE) %>%
    grep(pattern = "RAPlots.csv", value = TRUE)
  
  classification_files <- dir(in_dir, full.names = TRUE, recursive = TRUE) %>%
    grep(pattern = "RAClassification.csv", value = TRUE)
  
  project_files <- dir(in_dir, full.names = TRUE, recursive = TRUE) %>% 
    grep(pattern = "RAProjects.csv", value = TRUE)
  
  
  missing_files <- c()
  if (length(plot_files) == 0) missing_files <- c(missing_files, "RAPlots.csv")
  if (length(classification_files) == 0) missing_files <- c(missing_files, "RAClassification.csv")
  if (length(project_files) == 0) missing_files <- c(missing_files, "RAProjects.csv")
  
  if (length(missing_files) > 0) {
    stop("Required files not found in directory ", in_dir, ": ", paste(missing_files, collapse = ", "))
  }
  
  # read + combine
  plots_df_list <- lapply(
    plot_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE,
    col_types = cols(`PalmJoshua` = col_character(),
                     `DesertRip` = col_character()),
    guess_max = 20000
  )
  plots <- do.call(bind_rows, plots_df_list)
  
  classification_df_list <- lapply(
    classification_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE,
    guess_max = 20000
  )
  classification <- do.call(bind_rows, classification_df_list)
  
  # read in projects file
  
  projects_df_list <- lapply(project_files, read_csv, progress = FALSE, show_col_types = FALSE)
  
  projects <- do.call(bind_rows, projects_df_list)
  
  out <- list("plots" = plots, "classification" = classification, "projects" = projects)
  
  return(out)
}

#' Maps project codes to standardized classification method flags (inspection,
#' multivariate analysis, table analysis) and consolidates classification
#' notes
#' 
#' @param projects Data frame containing project data with classification
#'                 information
#' @param in_dir Directory of VegBank data to read from. Can be a full or relative to working directory.
#' 
#' @return Data frame with one row per ProjectCode containing normalized
#'         classification method flags and notes
#'         
#' @details
#' Combines ClassificationDescription and ClassificationTool fields into a
#' single class_notes field, removing empty entries and trailing punctuation
normalize_projects_classification <- function(projects) {
  
  method_lookup <- read.csv(here("data/lookup-tables/classification-methods-20260318.csv")) %>% 
    select(-ClassificationDescription, -ClassificationTool)
  
  projects_proj <- projects %>% 
    left_join(method_lookup, by = join_by(ProjectCode)) %>% 
    mutate(inspection = if_else(grepl("inspection", technique), TRUE, FALSE),
           multivariate_analysis = if_else(grepl("multiVariateAnalysis", technique), TRUE, FALSE),
           table_analysis = if_else(grepl("tableAnalysis", technique), TRUE, FALSE)) %>% 
    mutate(across(c(inspection, multivariate_analysis, table_analysis), ~ if_else(is.na(technique), NA, .))) %>% 
    mutate(across(c(ClassificationDescription, ClassificationTool), ~ if_else(is.na(.x), "", .x))) %>% 
    mutate(class_notes = paste("Project: ", ProjectCode, " . Classification notes: ", ClassificationDescription, ClassificationTool, sep = " ")) %>% 
    mutate(class_notes = if_else(class_notes == ", ", NA, class_notes)) %>% 
    mutate(class_notes = gsub("^(,\\s*)|(,\\s*)$", "", class_notes)) %>% 
    group_by(ProjectCode) %>% 
    summarise(
      across(c(inspection, multivariate_analysis, table_analysis, class_notes), first),
      .groups = "drop"
    )
  
  # any ClassificationDescription that didn't match either bucket?
  unmatched <- projects_proj %>%
    filter(is.na(inspection) | is.na(table_analysis) | is.na(multivariate_analysis)) %>% 
    filter(!is.na(class_notes)) %>% 
    filter(!grepl("was not performed|Not formally classified", class_notes)) %>% 
    distinct(ProjectCode) %>%
    pull(ProjectCode)
  
  if (length(unmatched) > 0) {
    cli_alert_warning("Some Projects did not contain classification methods in the lookup table: ({length(unmatched)} unique).")
    cli_text("Sample (up to 10):")
    cli_text(paste0(head(unmatched, 10), collapse = ", "))
  }
  
  return(projects_proj)
}

# plots (class_confidence)
#' Standardizes confidence ratings from plots into three categories:
#' High, Medium, Low. 
#' 
#' @param plots Data frame containing plot data with Confidence_ID field
#' 
#' @return Data frame with two columns: SurveyID and class_confidence
normalize_class_confidence <- function(plots) {
  conf_raw <- plots$Confidence_ID %>%
    as.character() %>%
    str_squish()
  
  conf_raw[conf_raw %in% c(
    "", "NA", "N/A", "<Null>",
    "Not recorded", "not recorded",
    "Not present", "not present",
    "not collected", NA
  )] <- NA_character_
  
  conf_pct <- suppressWarnings(
    as.numeric(str_remove(conf_raw, "%$"))
  )
  
  plots_conf <- plots %>%
    transmute(
      SurveyID,
      class_confidence = case_when(
        is.na(conf_raw) ~ NA_character_,
        
        conf_raw == "H" ~ "High",
        conf_raw == "M" ~ "Medium",
        conf_raw == "L" ~ "Low",
        
        conf_raw %in% c("High", "Medium", "Low") ~ conf_raw,
        
        !is.na(conf_pct) & conf_pct >= 75 ~ "High",
        !is.na(conf_pct) & conf_pct < 75  ~ "Medium",
        
        TRUE ~ NA_character_
      )
    )
  
  recognized <- is.na(conf_raw) |
    conf_raw %in% c("H","M","L","High","Medium","Low") |
    !is.na(conf_pct)
  
  weird_vals <- unique(conf_raw[!recognized])
  weird_vals <- weird_vals[!is.na(weird_vals)]
  
  if (length(weird_vals) > 0) {
    cli_alert_warning("Confidence_ID has unexpected/unparseable values ({length(weird_vals)} unique).")
    cli_text(paste0("- ", head(weird_vals, 20)))
  }
  
  return(plots_conf)
}

#' Queries the VegBank API to retrieve all community concept records with 
#' caching and adaptive paging
#' 
#' @param renew_cache If TRUE, re-downloads from API. If FALSE, uses cached data
#'                    if available
#'                    
#' @return Data frame containing all VegBank community concepts with standardized
#'         column types
#'         
#' @details
#' **Caching:**
#' \itemize{
#'   \item Cache location: `rappdirs::user_cache_dir("vegbank")/cc_all.csv`
#' }
#' **Column Type Standardization:**
#' Ensures consistent types across pages for fields like comm_description,
#' status codes, parent codes, and dates
get_vb_cc <- function(renew_cache = FALSE){
  
  cache_dir  <- rappdirs::user_cache_dir("vegbank")
  cache_file <- file.path(cache_dir, "cc_all.csv")
  
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  obj <- if (file.exists(cache_file) & !renew_cache) {
    cc_all <- read_csv(cache_file, progress = FALSE, show_col_types = FALSE, guess_max = 20000)
  } else {
    cli::cli_alert_info("Downloading vb community concept data.")
    cc_all <- vb_get_community_concepts(limit = 1000000)
      
    write_csv(cc_all, cache_file, progress = FALSE)
  }
  return(cc_all)
}

#' Loads VegBank community concepts and California vegetation code crosswalk
#' tables needed for matching CDFW classifications to VegBank
#' 
#' @param in_dir Directory of VegBank data to read from. Can be a full or relative to working directory.
#' @param renew_cache If TRUE, refreshes cached VegBank API reference data
#' 
#' @return Named list with two elements:
#'  \describe{
#'    \item{cacode_map}{Crosswalk mapping CaCodes to NVC codes}
#'    \item{cc_lookup}{Lookup table mapping NVC codes to VegBank cc_codes}
#'  }
#'  
#' @note This function downloads VegBank community concepts via API on first run
load_reference_tables <- function(in_dir, renew_cache = FALSE){
  
  in_dir <- here::here(in_dir)
  
  # Community concepts from VegBank
  cc_all <- suppressMessages(get_vb_cc(renew_cache = renew_cache))
  
  cc_current <- cc_all %>%
    filter(concept_rf_label %in% c('NVC 2004', 'USNVC 2016', 'USNVC 3.0'))
  
  cc_current$concept_rf_label <- factor(cc_current$concept_rf_label, levels = c('NVC 2004', 'USNVC 2016', 'USNVC 3.0'))
  
  nvc_lookup <- cc_current %>% 
    group_by(comm_code) %>% 
    slice_max(concept_rf_label) %>% 
    filter(!is.na(comm_code)) %>% 
    ungroup() %>%
    mutate(comm_code_norm = str_squish(str_to_lower(as.character(comm_code)))) %>%
    filter(!is.na(comm_code_norm), comm_code_norm != "") %>%
    distinct(comm_code_norm, .keep_all = TRUE) %>%
    select(cc_code, comm_code_norm)
  
  mcv_lookup <- cc_all %>% 
    filter(concept_rf_label %in% c("MCV - CDFW CNPS", "MCV2"))
  
  mcv_lookup$concept_rf_label <- factor(mcv_lookup$concept_rf_label, levels = c("MCV - CDFW CNPS", "MCV2"))
  
  mcv_lookup <- mcv_lookup %>% 
    group_by(comm_code) %>% 
    slice_max(concept_rf_label) %>% 
    ungroup() %>%
    select(cc_code, comm_code) %>% 
    filter(!is.na(comm_code))
  
  # CA code map
  cacode_sheet_path <- here('data/lookup-tables/VegBank_CrosswalkHierarchyMCV.csv')
  
  cacode_map_raw <- read_csv(cacode_sheet_path, progress = FALSE, show_col_types = FALSE)
  
  cacode_map <- cacode_map_raw %>%
    mutate(
      CaCode_norm = str_squish(str_to_lower(CaCode)),
      NVC_norm = str_squish(str_to_lower(as.character(`2009/NVC_Code`)))
    ) %>%
    filter(!is.na(CaCode_norm), CaCode_norm != "", !is.na(NVC_norm), NVC_norm != "")
  
  list(
    cacode_map = cacode_map,
    nvc_lookup = nvc_lookup,
    mcv_lookup = mcv_lookup
  )
}

#' Matches CDFW CaCodes to VegBank community concept codes by getting CaCode, 
#' NVC code, and then cc_code
#' 
#' @param classification Data frame containing classification data with CaCode
#'                       field
#' @param cacode_map Data frame mapping CaCodes to NVC codes
#' @param nvc_lookup Data frame mapping NVC codes to cc_code
#' 
#' @return Data frame with vb_cc_code field added
#' 
#' @details
#' **Matching Process:**
#' \enumerate{
#'   \item Normalize CaCode (lowercase, trimmed)
#'   \item Join to cacode_map to get NVC code
#'   \item Join to cc_lookup to get VegBank cc_code
#' }
#' **Duplicate Handling:**
#' When a CaCode maps to multiple NVC codes, only the first NVC code is used
#' to prevent row duplication.
assign_vb_cc_code <- function(classification, cacode_map, nvc_lookup, mcv_lookup){
  
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
    left_join(cacode_map_1to1 %>% select(CaCode_norm, NVC_norm), by = "CaCode_norm") 
  # Try NVC first
  classification_norm_nvc <- classification_norm %>% 
    inner_join(nvc_lookup, by = c("NVC_norm" = "comm_code_norm"))
  
  # MCV only for rows that didn't match NVC
  classification_norm_mcv <- classification_norm %>% 
    anti_join(classification_norm_nvc, by = names(classification_norm)) %>%  # exclude NVC matches
    inner_join(mcv_lookup, by = c("CaCode" = "comm_code"))
  
  # Combine them (no duplicates now)
  class_vbs <- bind_rows(classification_norm_nvc, classification_norm_mcv)
  
  class_final <- left_join(classification, class_vbs) %>% 
    rename(vb_cc_code = cc_code) %>% 
    filter(str_detect(CaCode, "^\\d{2}\\.\\d{3}\\.\\d{2}$"))
  
  # warn if lots of NAs
  na_ct <- sum(is.na(class_final$vb_cc_code))
  if (na_ct > 0) {
    cli_alert_warning("vb_cc_code is NA for {na_ct} rows (out of {nrow(class_final)}).")
  }
  
  #t <- filter(class_final, is.na(vb_cc_code))
  
  class_final
}

#' Combines classification records with confidence ratings and project
#' classification methods, validating that no duplicate rows are created
#' 
#' @param classification_with_cc Data frame with classification data and cc_code
#' @param plots_conf Data frame with SurveyID and class_confidence
#' @param projects_proj Data frame with ProjectCode and classification methods
#' 
#' @return Combined data frame with all classification information
#' 
#' @details
#' A left join is conducted with plots_conf by SurveyID. Then, another left join
#' is conducted with projects_proj by ProjectCode
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

#' Main function that orchestrates the complete community classification
#' processing pipeline from raw CSV files to VegBank-compatible loader tables
#' 
#' @param in_dir Directory of VegBank data to read from. Can be a full or relative to working directory.
#' @param out_dir Directory of data to write to. Can be a full or relative to working directory.
#' @param renew_cache If TRUE, refreshes cached VegBank API reference data
#' 
#' @return None. Writes loader table communityClassificationsLT.csv to `out_dir`
#' 
#' @details
#' This function executes a comprehensive classification processing pipeline
#' from data loading, mapping project codes, confidence standardization,
#' matches community concepts, data integration, and loader table generation.
community_loader <- function(in_dir, out_dir, renew_cache = FALSE){
  
  in_dir  <- here::here(in_dir)
  out_dir <- here::here(out_dir)
  
  out <- load_community_files(in_dir)
  
  list2env(out, envir = environment())
  
  projects_proj <- normalize_projects_classification(projects)
  plots_conf <- normalize_class_confidence(plots)
  
  refs <- load_reference_tables(in_dir, renew_cache = renew_cache)
  
  classification_with_cc <- assign_vb_cc_code(
    classification = classification,
    cacode_map = refs$cacode_map,
    nvc_lookup = refs$nvc_lookup,
    mcv_lookup = refs$mcv_lookup
  ) 
  # TODO: drop GXXX values in CaCode
  # TODO: drop NAs and n/a's in CaCode
  # they don't get inserted anyway but might as well be explicit about it
  
  no_match <- classification_with_cc %>% 
    filter(is.na(vb_cc_code)) %>% 
    select(CaCode, Alliance, Association) %>% 
    filter(!is.na(CaCode)) %>% 
    distinct()
  
  class_cc_proj <- join_classifications(
    classification_with_cc = classification_with_cc,
    plots_conf = plots_conf,
    projects_proj = projects_proj
  )
  
  class_cc_proj <- class_cc_proj %>% 
    mutate(multivariate_analysis = if_else(!(multivariate_analysis %in% c("TRUE", "FALSE")), NA, multivariate_analysis))
  
  # TODO: possibly improve messaging here?
  #stopifnot(nrow(class_cc_proj) == nrow(community_LT))
  stopifnot(all(names(c("class_notes","inspection","multivariate_analysis","class_confidence","vb_cc_code")) %in% names(class_cc_proj)))
  
  community_LT <- class_cc_proj %>%
    mutate(user_cl_code = row_number()) %>%
    mutate(SurveyID = toupper(SurveyID)) %>% 
    select(
      user_ob_code = SurveyID,
      user_cl_code,
      class_notes,
      inspection,
      multivariate_analysis,
      class_confidence,
      vb_cc_code
    )
  
  # save filled in loader table
  out_path <- file.path(out_dir, "communityClassificationsLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(community_LT, out_path)
}
 
