library(tidyverse)
library(stringr)
library(cli)
library(glue)
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

# example data for CommunityNames table
# for each row in the table above, there are two rows in the communityNames table

# row 1: scientific name
#$ user_cc_code     <chr> "21.100.00" (CaCode column)
#$ name_type        <chr> "Scientific" (same for all rows)
#$ name             <chr> "Abronia latifolia – Ambrosia chamissonis" (Alliance or Association column depending on file)
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

load_community_def_files <- function(in_dir) {
  
  alliance_path    <- file.path(in_dir, "data/MCV2019_Alliance.csv")
  association_path <- file.path(in_dir, "data/MCV2019_Association.csv")
  
  if (!file.exists(alliance_path)) {
    cli_abort("Missing file: {alliance_path}")
  }
  if (!file.exists(association_path)) {
    cli_abort("Missing file: {association_path}")
  }
  
  alliance <- read_csv(alliance_path, show_col_types = FALSE, progress = FALSE)
  association <- read_csv(association_path, show_col_types = FALSE, progress = FALSE)
  
  mcv <- bind_rows(alliance, association)
  
  ret <- list(
    alliance = alliance,
    association = association,
    mcv = mcv
  )
  
  return(ret)
}

# comm_level
normalize_comm_level <- function(mcv) {
  
  if (!("MVC level" %in% names(mcv))) {
    cli_abort("Expected column `MVC level` not found in MCV data.")
  }
  
  mcv <- mcv %>%
    mutate(
      mvc_level_raw = as.character(`MVC level`) %>% str_squish() %>% str_to_lower(),
      comm_level = case_when(
        str_detect(mvc_level_raw, "association") ~ "association",
        str_detect(mvc_level_raw, "alliance") ~ "alliance",
        TRUE ~ NA_character_
      )
    )
  
  bad_levels <- mcv %>%
    filter(!is.na(`MVC level`), is.na(comm_level)) %>%
    distinct(`MVC level`) %>%
    pull(`MVC level`)
  
  if (length(bad_levels) > 0) {
    cli_alert_warning("Some MVC level values were not recognized (comm_level set to NA):")
    cli_ul(bad_levels)
  }
  
  return(mcv)
}

# concept_name
normalize_concept_name <- function(mcv) {
  
  if (!("CaCode" %in% names(mcv))) {
    cli_abort("Expected column `CaCode` not found in MCV data.")
  }
  
  # We expect Alliance and/or Association columns to exist across the two files.
  # Prefer Alliance when present, otherwise use Association.
  if (!("Alliance" %in% names(mcv))) mcv$Alliance <- NA_character_
  if (!("Association" %in% names(mcv))) mcv$Association <- NA_character_
  
  mcv <- mcv %>%
    mutate(
      CaCode = as.character(CaCode) %>% str_squish(),
      Alliance = as.character(Alliance) %>% str_squish(),
      Association = as.character(Association) %>% str_squish(),
      concept_name = coalesce(
        na_if(Alliance, ""),
        na_if(Association, "")
      )
    )
  
  missing_names <- which(is.na(mcv$concept_name) | mcv$concept_name == "")
  if (length(missing_names) > 0) {
    cli::cli_alert_warning(
      "Some rows are missing Alliance/Association names ({length(missing_names)} rows). `concept_name` set to NA."
    )
  }
  
  return(mcv)
}

build_community_concepts <- function(mcv) {
  
  community_concepts <- mcv %>%
    transmute(
      user_cc_code        = CaCode,           # CaCode
      name                = concept_name,     # Alliance or Association
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

community_definitions_loader <- function(in_dir, out_dir){
  alliance <- read.csv(file.path(in_dir, "lookup-tables/MCV2019_Alliance.csv"))
  association <- read.csv(file.path(in_dir, "lookup-tables/MCV2019_Association.csv"))
  # this is the data we need to model to the form above
  mcv <- bind_rows(alliance, association)
}

