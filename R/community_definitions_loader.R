library(tidyverse)
library(stringr)
library(cli)
library(glue)
library(readr)
source("R/build_loader_table.R")
# example data for CommunityConcepts table

#$user_cc_code        <chr> "21.100.00" (CaCode column)
#$ name                <chr> ""Abronia latifolia – Ambrosia chamissonis" (Alliance or Association column depending on file)
#$ user_rf_code        <chr> MVC 2019" (same for all rows) 
#$ user_status_rf_code <chr> "MVC 2019" (same for all rows) 
#$ comm_concept_status <chr> "accepted" (same for all rows)
#$ user_parent_cc_code <chr> NA
#$ comm_level          <chr> "association" (normalized from the MVC level column)
#$ start_date          <date> 2019-01-01 same for all rows
#$ vb_status_py_code   <chr> "py.512" (same for all rows)


load_community_def_files <- function(in_dir) {
  
  alliance_path    <- file.path(in_dir, "lookup-tables/MCV2019_Alliance.csv")
  association_path <- file.path(in_dir, "lookup-tables/MCV2019_Association.csv")
  
  if (!file.exists(alliance_path)) {
    cli_abort("Missing file: {alliance_path}")
  }
  if (!file.exists(association_path)) {
    cli_abort("Missing file: {association_path}")
  }
  
  alliance <- read_csv(alliance_path, show_col_types = FALSE, progress = FALSE)
  association <- read_csv(association_path, show_col_types = FALSE, progress = FALSE)
  
  mcv <- bind_rows(alliance, association) %>%
    mutate(name = coalesce(Alliance, Association)) %>%
    select(-Alliance, -Association)
  
  return(mcv)
}

load_classification_cacodes <- function(in_dir) {
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  classification_files <- dir(sub_folders, full.names = TRUE) %>%
    grep(pattern = "RAClassification.csv", value = TRUE)
  
  if (length(classification_files) == 0) {
    cli_abort("No RAClassification.csv files found under {in_dir}.")
  }
  
  class_df_list <- lapply(
    classification_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE,
    guess_max = 20000
  )
  classification <- bind_rows(class_df_list)
  
  if (!("CaCode" %in% names(classification))) {
    cli_abort("Expected column `CaCode` not found in RAClassification.csv data.")
  }
  
  cacodes <- classification %>%
    transmute(CaCode = str_squish(as.character(CaCode))) %>%
    filter(!is.na(CaCode), CaCode != "") %>%
    distinct() %>%
    pull(CaCode)
  
  if (length(cacodes) == 0) {
    cli_abort("No non-empty CaCode values found in RAClassification.csv data.")
  }
  
  return(cacodes)
}

# comm_level
normalize_comm_level <- function(mcv) {
  
  if (!("MCVLevel" %in% names(mcv))) {
    cli_abort("Expected column `MCVLevel` not found in MCV data.")
  }
  
  mcv <- mcv %>%
    mutate(
      mvc_level_raw = as.character(MCVLevel) %>% str_squish() %>% str_to_lower(),
      comm_level = case_when(
        str_detect(mvc_level_raw, "association") ~ "association",
        str_detect(mvc_level_raw, "alliance") ~ "alliance",
        TRUE ~ NA_character_
      )
    )
  
  bad_levels <- mcv %>%
    filter(!is.na(MCVLevel), is.na(comm_level)) %>%
    distinct(MCVLevel) %>%
    pull(MCVLevel)
  
  if (length(bad_levels) > 0) {
    cli_alert_warning("Some MVC level values were not recognized (comm_level set to NA):")
    cli_ul(bad_levels)
  }
  
  missing_names <- which(is.na(mcv$MCVLevel) | mcv$MCVLevel == "")
  if (length(missing_names) > 0) {
    cli::cli_alert_warning(
      "Some rows are missing an Alliance/Association level ({length(missing_names)} rows)."
    )
  }
  
  return(mcv)
}

filter_mcv_to_classification <- function(mcv, cacodes_in_data) {
  
  before_n <- nrow(mcv)
  
  cacodes_in_data <- str_squish(as.character(cacodes_in_data))
  
  mcv_f <- mcv %>%
    mutate(CaCode = str_squish(as.character(CaCode))) %>%
    filter(CaCode %in% cacodes_in_data)
  
  after_n <- nrow(mcv_f)
  
  if (after_n == 0) {
    cli_abort("After filtering to CaCodes in RAClassification, 0 rows remain in MCV data. Check that CaCode formats match.")
  }
  
  cli_alert_info("Filtered MCV rows: {before_n} -> {after_n} (kept CaCodes present in RAClassification).")
  
  return(mcv_f)
}

build_community_concepts <- function(mcv) {
  
  community_concepts <- mcv %>%
    transmute(
      user_cc_code        = CaCode,
      name                = name,     
      user_rf_code        = "MVC 2019",
      user_status_rf_code = "MVC 2019",
      comm_concept_status = "accepted",
      user_parent_cc_code = NA_character_,
      comm_level          = comm_level,
      start_date          = as.Date("2019-01-01"),
      vb_status_py_code   = "py.512"
    ) %>%
    distinct(user_cc_code, .keep_all = TRUE)
  
  bad_codes <- which(is.na(community_concepts$user_cc_code) | community_concepts$user_cc_code == "")
  if (length(bad_codes) > 0) {
    cli_abort("CommunityConcepts has missing/blank CaCode values. Check input MCV files.")
  }
  
  return(community_concepts)
}

# TODO: build community names table matching this description:

# example data for CommunityNames table
# for each row in the comm_concepts data frame, there are two rows in the comm_names table

# row 1: scientific name
#$ user_cc_code     <chr> "21.100.00" (CaCode column)
#$ name_type        <chr> "Scientific" (same for all rows)
#$ name             <chr> "Abronia latifolia – Ambrosia chamissonis" (name column)
#$ name_status      <chr> "Standard" (same for all rows)
#$ usage_start      <date> 2019-01-01 same for all rows
#$ vb_usage_py_code <chr> "py.512" (same for all rows)

# row 2: code
#$ user_cc_code     <chr> "21.100.00" (CaCode column)
#$ name_type        <chr> "Code" (same for all rows)
#$ name             <chr> "21.100.00" (CaCode column)
#$ name_status      <chr> "Standard" (same for all rows)
#$ usage_start      <date> 2019-01-01 same for all rows
#$ vb_usage_py_code <chr> "py.512" (same for all rows)

build_community_names <- function(comm_concepts) {
  
  required_cols <- c("user_cc_code", "name")
  missing_cols <- setdiff(required_cols, names(comm_concepts))
  if (length(missing_cols) > 0) {
    cli_abort("comm_concepts is missing required columns: {paste(missing_cols, collapse = ', ')}")
  }
  
  # row 1: Scientific name
  comm_names_scientific <- comm_concepts %>%
    transmute(
      user_cc_code     = user_cc_code,
      name_type        = "Scientific",
      name             = name,
      name_status      = "Standard",
      usage_start      = as.Date("2019-01-01"),
      vb_usage_py_code = "py.512"
    )
  
  # row 2: Code
  comm_names_code <- comm_concepts %>%
    transmute(
      user_cc_code     = user_cc_code,
      name_type        = "Code",
      name             = user_cc_code,
      name_status      = "Standard",
      usage_start      = as.Date("2019-01-01"),
      vb_usage_py_code = "py.512"
    )
  
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

community_definitions_loader <- function(in_dir, out_dir){
  cacodes <- load_classification_cacodes(in_dir)
  
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
  
  invisible(list(comm_concepts = comm_concepts, comm_names = comm_names))
}

in_dir <- '/var/data/curation/vegbank/'
out_dir <- 'data/loader-tables'

community_definitions_loader(in_dir, out_dir)

