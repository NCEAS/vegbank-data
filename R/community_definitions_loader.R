library(tidyverse)
library(stringr)
library(cli)
library(readr)
library(lubridate)
library(here)

#' Reads Manual of California Vegetation (MCV) 2019 alliance and association
#' files and combines them into a single data set
#' 
#' @param in_dir Directory of VegBank data to read from
#' 
#' @return Data frame combining alliance and association data with unified
#'         'name' column
#'         
#' @details
#' **File Requirements:**
#' \itemize{
#'   \item MCV2019_Alliance.csv - Alliance-level vegetation classifications
#'   \item MCV2019_Association.csv - Association-level vegetation classifications
#' }
load_community_def_files <- function(in_dir) {
  
  alliance_path    <- here('data/lookup-tables/MCV-alliance.csv')
  association_path <- here('data/lookup-tables/MCV-association.csv')
  
  if (!file.exists(alliance_path)) {
    cli_abort("Missing file: {alliance_path}")
  }
  if (!file.exists(association_path)) {
    cli_abort("Missing file: {association_path}")
  }
  
  alliance <- read_csv(alliance_path, show_col_types = FALSE, progress = FALSE) %>% 
    mutate(DateAdded = mdy(DateAdded))
  association <- read_csv(association_path, show_col_types = FALSE, progress = FALSE) %>% 
    mutate(DateAddedToMCV = dmy(DateAddedToMCV))
  
  mcv <- bind_rows(alliance, association) %>%
    mutate(CaCode = coalesce(AllianceCaCode, AssociationCaCode)) %>%
    mutate(date = coalesce(DateAdded, DateAddedToMCV)) %>% 
    select(
      CaCode,
      name = ScientificName,
      MCVLevel = ClassifLevel,
      date
    ) %>% 
    filter(!is.na(CaCode))
  
  return(mcv)
}

#' Extracts unique California vegetation codes (CaCodes) from
#' RAClassification.csv files, filtering to properly formatted codes
#' 
#' @param in_dir Directory of VegBank data to read from
#' 
#' @return Character vector of unique, properly formatted CaCodes found in
#'         classification data
#'         
#' @details
#' **CaCode Format:**
#' Only codes matching the pattern `##.###.##` are retained (e.g., "21.100.00").
load_cdfw_cacodes <- function(in_dir, renew_cache) {
  
  out <- load_community_files(in_dir)
  list2env(out, envir = environment())
  
  refs <- load_reference_tables(in_dir, renew_cache)
  
  classification_with_cc <- assign_vb_cc_code(
    classification = classification,
    cacode_map = refs$cacode_map,
    nvc_lookup = refs$nvc_lookup,
    mcv_lookup = refs$mcv_lookup
  ) 
  
  if (!("CaCode" %in% names(classification))) {
    cli_abort("Expected column `CaCode` not found in RAClassification.csv data.")
  }
  
  cacodes <- classification_with_cc %>%
    filter(is.na(vb_cc_code)) %>% 
    mutate(CaCode = str_squish(as.character(CaCode))) %>%
    filter(!is.na(CaCode), CaCode != "") %>%
    filter(str_detect(CaCode, "^\\d{2}\\.\\d{3}\\.\\d{2}$")) %>% 
    distinct() %>%
    pull(CaCode)
  
  if (length(cacodes) == 0) {
    cli_abort("No non-empty CaCode values found in RAClassification.csv data.")
  }
  
  return(cacodes)
}

#' Standardizes MCVLevel field values to VegBank-compatible community levels
#' (alliance, association)
#' 
#' @param mcv Data frame containing MCV data with MCVLevel field
#' 
#' @return Data frame with added comm_level field containing standardized values
# comm_level
normalize_comm_level <- function(mcv) {
  
  if (!("MCVLevel" %in% names(mcv))) {
    cli_abort("Expected column `MCVLevel` not found in MCV data.")
  }
  if (!("NVCAboveAlliance" %in% names(mcv))) {
    cli_abort("Expected column `NVCAboveAlliance` not found in MCV data.")
  }
  
  mcv <- mcv %>%
    mutate(
      mcv_level_raw = as.character(MCVLevel) %>% str_squish() %>% str_to_lower(),
      nvc_above_raw = as.character(NVCAboveAlliance) %>% str_squish() %>% str_to_lower(),
      comm_level = case_when(
        str_detect(mcv_level_raw, "association") ~ "association",
        str_detect(mcv_level_raw, "alliance") & str_detect(nvc_above_raw, "macrogroup") ~ "macrogroup",
        str_detect(mcv_level_raw, "alliance") & str_detect(nvc_above_raw, "group") ~ "group",
        str_detect(mcv_level_raw, "alliance") ~ "alliance",
        TRUE ~ NA_character_
      )
    )
  
  bad_levels <- mcv %>%
    filter((!is.na(MCVLevel) & MCVLevel != "") | (!is.na(NVCAboveAlliance) & NVCAboveAlliance != "")) %>%
    filter(is.na(comm_level)) %>%
    distinct(MCVLevel, NVCAboveAlliance)
  
  if (nrow(bad_levels) > 0) {
    cli_alert_warning("Some MCV/NVC level combinations were not recognized (comm_level set to NA):")
    cli_ul(apply(bad_levels, 1, paste, collapse = " | "))
  }
  
  missing_levels <- which(is.na(mcv$MCVLevel) | mcv$MCVLevel == "")
  if (length(missing_levels) > 0) {
    cli::cli_alert_warning(
      "Some rows are missing an MCV level ({length(missing_levels)} rows)."
    )
  }
  
  return(mcv)
}

#' Reduces MCV reference data to only vegetation types (CaCode) that actually
#' appear in CDFW classification data
#' 
#' @param mcv Data frame containing complete MCV reference data
#' @param cacodes_in_data CaCodes from RAClassifications.csv
#' 
#' @return Filtered data frame containing only MCV rows with CaCodes present in
#'         classification data
#' 
#' @details
#' Prevents creation of community concept records for vegetation types not
#' observed in the CDFW data set, reducing unnecessary database entries
filter_mcv_to_classification <- function(mcv, cacodes_in_data) {
  
  before_n <- nrow(mcv)
  
  cacodes_in_data <- str_squish(as.character(cacodes_in_data))
  
  mcv_f <- mcv %>%
    mutate(CaCode = str_squish(as.character(CaCode))) %>%
    filter(CaCode %in% cacodes_in_data) %>% 
    mutate(date = coalesce(date, as.Date("2019-01-01")))
  
  after_n <- nrow(mcv_f)
  
  if (after_n == 0) {
    cli_abort("After filtering to CaCodes in RAClassification, 0 rows remain in MCV data. Check that CaCode formats match.")
  }
  
  cli_alert_info("Filtered MCV rows: {before_n} -> {after_n} (kept CaCodes present in RAClassification).")
  
  return(mcv_f)
}

#' Constructs the CommunityConcepts loader table from MCV data with standardized
#' VegBank fields and metadata
#' 
#' @param mcv Data frame containing MCV data with CaCode, name, and comm_level
#' 
#' @return Data frame with CommunityConcepts loader table structure
#' 
#' @details
#' **Output Fields:**
#' \describe{
#'   \item{user_cc_code}{CaCode - unique community concept identifier}
#'   \item{name}{Scientific name (alliance or association name)}
#'   \item{user_rf_code}{"MCV 2019" for all rows}
#'   \item{user_status_rf_code}{"MCV 2019" for all rows}
#'   \item{comm_concept_status}{"accepted" for all rows}
#'   \item{user_parent_cc_code}{NA (no hierarchy in this dataset)}
#'   \item{comm_level}{"alliance" or "association"}
#'   \item{start_date}{date record was added to the MCV}
#'   \item{user_status_py_code}{"CDFW CNPS" for all rows}
#' }
build_community_concepts <- function(mcv) {
  
  community_concepts <- mcv %>%
    mutate(
      user_cc_code        = CaCode,
      name                = name,     
      user_rf_code        = "MCV - CDFW CNPS",
      user_status_rf_code = "MCV - CDFW CNPS",
      comm_concept_status = "accepted",
      user_parent_cc_code = NA_character_,
      comm_level          = comm_level,
      start_date          = date,
      user_status_py_code   = "CDFW CNPS"
    ) %>%
    distinct(user_cc_code, .keep_all = TRUE) %>% 
    select(user_cc_code, name, user_rf_code, user_status_rf_code, comm_concept_status, user_parent_cc_code, comm_level, start_date, user_status_py_code)
  
  bad_codes <- which(is.na(community_concepts$user_cc_code) | community_concepts$user_cc_code == "")
  if (length(bad_codes) > 0) {
    cli_abort("CommunityConcepts has missing/blank CaCode values. Check input MCV files.")
  }
  
  return(community_concepts)
}

#' Creates the CommunityNames loader table with two records per community
#' concept: one scientific name and one code-based name
#' 
#' @param comm_concepts Data frame containing CommunityConcepts with
#'                      user_cc_code and name fields
#'                      
#' @return Data frame with CommunityNames loader table structure containing
#'         exactly 2 rows per community concept
#'         
#' @details
#' **Dual Naming Structure:**
#' Each community concept receives two name records:
#'
#' *Row 1 - Scientific Name:*
#' \describe{
#'   \item{user_cc_code}{CaCode}
#'   \item{name_type}{"Scientific"}
#'   \item{name}{Alliance or association scientific name}
#'   \item{name_status}{"Standard"}
#'   \item{usage_start}{date}
#'   \item{vb_usage_py_code}{"py.512"}
#' }
#'
#' *Row 2 - Code Name:*
#' \describe{
#'   \item{user_cc_code}{CaCode}
#'   \item{name_type}{"Code"}
#'   \item{name}{CaCode itself (e.g., "21.100.00")}
#'   \item{name_status}{"Standard"}
#'   \item{usage_start}{date}
#'   \item{vb_usage_py_code}{"py.512"}
#' }
#'
#' **Example:**
#' For CaCode "21.100.00" with name "Abronia latifolia – Ambrosia chamissonis":
#' \itemize{
#'   \item Row 1: name_type="Scientific", name="Abronia latifolia – Ambrosia chamissonis"
#'   \item Row 2: name_type="Code", name="21.100.00"
#' }
build_community_names <- function(comm_concepts) {
  
  required_cols <- c("user_cc_code", "name")
  missing_cols <- setdiff(required_cols, names(comm_concepts))
  if (length(missing_cols) > 0) {
    cli_abort("comm_concepts is missing required columns: {paste(missing_cols, collapse = ', ')}")
  }
  
  # row 1: Scientific name
  comm_names_scientific <- comm_concepts %>%
    mutate(
      user_cc_code     = user_cc_code,
      name_type        = "Scientific",
      name             = name,
      name_status      = "Standard",
      usage_start      = start_date,
      user_usage_py_code = "CDFW CNPS"
    ) %>% 
    select(user_cc_code, name_type, name, name_status, usage_start, user_usage_py_code)
  
  # row 2: Code
  comm_names_code <- comm_concepts %>%
    mutate(
      user_cc_code     = user_cc_code,
      name_type        = "Code",
      name             = user_cc_code,
      name_status      = "Standard",
      usage_start      = start_date,
      user_usage_py_code = "CDFW CNPS"
    ) %>% 
    select(user_cc_code, name_type, name, name_status, usage_start, user_usage_py_code)
  
  comm_names <- bind_rows(comm_names_scientific, comm_names_code)
  
  if (any(is.na(comm_names$user_cc_code) | comm_names$user_cc_code == "")) {
    cli_abort("CommunityNames has missing/blank user_cc_code values.")
  }
  
  missing_scientific <- comm_names_scientific %>%
    filter(is.na(name) | name == "") %>%
    distinct(user_cc_code) %>%
    pull(user_cc_code)
  
  if (length(missing_scientific) > 0) {
    cli_alert_warning(
      "Some CommunityNames scientific rows have missing names ({length(missing_scientific)} concepts)."
    )
    cli_ul(head(missing_scientific, 10))
  }
  
  expected <- 2 * nrow(comm_concepts)
  if (nrow(comm_names) != expected) {
    cli_alert_warning("CommunityNames row count is not 2x CommunityConcepts ({nrow(comm_names)} vs {expected}).")
  }
  
  return(comm_names)
}

#' Main function that creates VegBank community concept and community name
#' loader tables from Manual of California Vegetation (MCV) 2019 reference
#' data, filtered to vegetation types present in CDFW classification data
#' 
#' @param in_dir Path to the input directory containing RAClassifications.csv
#'               files and MCV2019_Alliance.csv and MCV2019_Association.csv
#' @param out_dir Directory of data to write to
#' 
#' @return None. Writes communityConceptsLT.csv and communityNamesLT.csv
#' 
#' @details
#' This function executes a community definition processing pipeline where it
#' first loads CDFW classification data, loads MCV reference data, normalizes
#' community levels, filters to observed vegetation types, builds community
#' concepts, and builds community names.
community_definitions_loader <- function(in_dir, out_dir, renew_cache = FALSE){
  cacodes <- load_cdfw_cacodes(in_dir, renew_cache)
  
  mcv <- load_community_def_files(in_dir)
  mcv <- normalize_comm_level(mcv)
  mcv <- filter_mcv_to_classification(mcv, cacodes)

  comm_concepts <- build_community_concepts(mcv)
  comm_names <- build_community_names(comm_concepts)
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  out_path_concepts <- file.path(out_dir, "communityConceptsLT.csv")
  out_path_names    <- file.path(out_dir, "communityNamesLT.csv")
  
  cli_alert_success("Writing two output files to:")
  cli_ul(c(out_path_concepts, out_path_names))
  
  write_csv(comm_concepts, out_path_concepts)
  write_csv(comm_names, out_path_names)
}

