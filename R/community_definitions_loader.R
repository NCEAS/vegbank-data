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
    mutate(name = if_else(is.na(Alliance), Association, Alliance)) %>% 
    select(-Alliance, -Association)
  
  return(mcv)
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

community_definitions_loader <- function(in_dir, out_dir){
  mcv <- load_community_def_files(in_dir)
  mcv <- normalize_comm_level(mcv)
  comm_concepts <- build_community_concepts(mcv)
}

