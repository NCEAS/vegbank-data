library(tidyverse)
library(stringr)
library(vctrs)

#' Extract party and contributor information from RAProjects files
#' and writes to a vegbank loader table.
#'
#' @param in_dir Directory of vegbank data to read from
#' @param out_dir Directory of data to write to
#'
#' @return None. Writes loader tables partyLT.csv and contributorLT.csv to `out_dir`.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Reshapes wide-format contact data (DataContactName1, DataContactName2, etc.)
#'         into long format with one row per contact person
#'   \item Validates email addresses using regex pattern matching
#'   \item Splits contact names into given name, middle name (if present), and surname
#'   \item Generates unique user party codes (ca_001, ca_002, etc.)
#'   \item Maps CDFW role descriptions to standardized VegBank role codes (ar.*)
#'   \item Queries VegBank API to match parties with existing VegBank records
#'   \item Creates party and contributor loader tables with appropriate foreign keys
#' }
party_contributor_loader <- function(in_dir, out_dir){
  
  in_dir  <- here::here(in_dir)
  out_dir <- here::here(out_dir)
  
  # read in projects file
  project_files <- dir(in_dir, full.names = TRUE) %>% 
    grep(pattern = "RAProjects.csv", value = TRUE)
  
  projects_df_list <- lapply(project_files, read_csv, progress = FALSE, show_col_types = FALSE)
  
  projects <- do.call(bind_rows, projects_df_list)
  
  
  
  
  # Changing to RAProjects to long format to have one row per contact
  # Remove unnecessary variables (can add back later if needed)
  projects_long <- projects %>%
    pivot_longer(
      cols = matches("^DataContact(Name|Email|Organization|Role)\\d*$"),
      names_to   = c(".value", "slot"),
      names_pattern = "^DataContact(Name|Email|Organization|Role)(\\d*)$"
    ) %>%
    filter(!is.na(Name) & str_squish(Name) != "") %>%
    transmute(
      ProjectCode,
      ContactName  = str_squish(Name),
      ContactEmail = str_squish(Email),
      ContactOrg = str_squish(Organization),
      ContactRole = str_squish(Role)
    )
  
  # DataContactName should be a first and last name
  # Making sure each value is two words
  name_count <- projects_long %>%
    mutate(word_count = str_count(ContactName, "\\S+"))
  
  # Email should be a valid email address
  
  email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
  invalid_emails <- projects_long %>%
    filter(!str_detect(ContactEmail, email_pattern))
  
  if (nrow(invalid_emails) > 0){
    cli::cli_alert_warning("Some email addresses appear to be invalid: {.emph {paste(invalid_emails$ContactEmail, collapse = ', ')}}")
  }
  
  ### given_name (Party) + surname (Party) ###
  # Separating DataContactName into `FirstName` and `LastName`.
  # Currently no middle names, but can optionally separate middle name as well.
  # If no middle name, it is set to NA
  
  projects <- projects_long %>%
    tidyr::extract(
      ContactName,
      into  = c("FirstName", "MiddleName", "LastName"),
      regex = "^\\s*([^\\s]+)\\s+(?:([^\\s]+)\\s+)?(.+?)\\s*$",
      remove = FALSE
    ) %>%
    mutate(
      MiddleName = na_if(MiddleName, ""),
      across(c(FirstName, MiddleName, LastName), str_squish)
    ) %>% 
    mutate(ContactEmail = tolower(ContactEmail))
  
  roles <- tibble::tribble(
    ~ar_code, ~role_name,
    "ar.16",  "Author",
    "ar.17",  "Contact",
    "ar.18",  "PI",
    "ar.19",  "Data Manager",
    "ar.34",  "Classifier",
    "ar.36",  "Plot author",
    "ar.38",  "Co-PI",
    "ar.39",  "Computer (automated)",
    "ar.40",  "Consultant",
    "ar.43",  "Field assistant",
    "ar.44",  "Guide",
    "ar.45",  "Land owner",
    "ar.46",  "Not specified",
    "ar.47",  "Not specified/Unknown",
    "ar.48",  "Passive observer",
    "ar.50",  "Plot contributor",
    "ar.51",  "Publication author",
    "ar.53",  "Research advisor",
    "ar.54",  "System manager",
    "ar.55",  "Taxonomist",
    "ar.56",  "Data aggregator"
  )
  
  ### role (Contributor) ###
  # Map DataContactRole values to ar.* codes
  # Done manually for each value
  role_lookup <- read.csv(here('data/lookup-tables/cdfw-roles-2026-03-18.csv'))
  
  projects <- projects %>%
    mutate(ContactRole = tolower(trimws(ContactRole))) %>% 
    left_join(role_lookup, by = "ContactRole") %>% 
    left_join(roles, by = c("role_name"))
  
  if (any(is.na(projects$ar_code))){
    uncat <- projects$ContactRole[which(is.na(projects$ar_code))]
    cli::cli_alert_warning("The following project roles were not transformed into vegbank codes:")
    cli::cli_h3("Uncategorized roles")
    cli::cli_ul(unique(uncat))
  }
  
  ### contributor_type (Contributor) ###
  # Since we are in RAProjects, each value will be "Project" for now
  # Can potentially merge with something else to add values "Observation" or "Class"
  # But, there is not contributor data for these
  
  projects <- projects %>%
    mutate(
      contributor_type = "Project"
    )
  
  # TODO: clean this up more?
  people <- projects %>% 
    select(FirstName, MiddleName, LastName, ContactEmail, ContactOrg) %>% 
    distinct() %>% 
    mutate(
      full_name = pmap_chr(
        list(FirstName, MiddleName, LastName),
        ~ str_trim(str_to_lower(paste(na.omit(c(...)), collapse = " ")))
      )
    )
  
  # get existing vb parties by name
  party_vegbank <- as.data.frame(vegbankr::vb_get_parties(limit = 5000))
  
  vegbank_names <- party_vegbank %>%
    mutate(
      full_name = pmap_chr(
        list(given_name, middle_name, surname),
        ~ str_trim(str_to_lower(paste(na.omit(c(...)), collapse = " ")))
      )
    )
  
  # Check for matching names across data sets 
  matching_names <- intersect(people$full_name, vegbank_names$full_name)
  
  # Keep 63 entries; keep party_LT; keep df1
  # Check if the person matches from party_LT to party_vegbank
  # If there is a match between people names from party_LT and party_vegbank,
  # then save py_code as vb_py_code. Otherwise, leave it as NA
  
  # df2 subset of distinct vegbank parties (vegbank party)
  veg_subset <- vegbank_names %>% 
    select(full_name, py_code) %>% 
    distinct(full_name, .keep_all = TRUE)
  
  # Left join to get vb_py_code
  people_j <- people %>% 
    left_join(veg_subset, by = "full_name") %>% 
    select(-full_name) %>% 
    mutate(user_py_code = sprintf("ca_pt_%03d", seq_len(n()))) %>% 
    mutate(user_py_code = if_else(!is.na(py_code), NA, user_py_code))
  
  if (nrow(people_j) > nrow(people)){
    warning("Joining vegbank party identifiers resulted in duplicated rows. This could indicate an issue with the vegbank party codes, the input data, or both.")
  }
  
  party_LT <- people_j %>%
    select(
      py_code,
      user_py_code,
      given_name = FirstName,
      middle_name = MiddleName,
      surname = LastName,
      organization_name = ContactOrg,
      email = ContactEmail
    ) %>% 
    rbind(data.frame(py_code = NA_character_,
                     user_py_code = "cdfw_general",
                     given_name = NA_character_,
                     middle_name = NA_character_,
                     surname = NA_character_,
                     organization_name = "Vegcamp, California Department of Fish and Wildlife",
                     email = "vegcamp@wildlife.ca.gov"))
  
  
  ctrib <- left_join(projects, people_j, by = join_by(FirstName, MiddleName, LastName, ContactEmail, ContactOrg)) %>% 
    select(ProjectCode, ar_code, py_code, user_py_code) %>% 
    mutate(contributor_type = "Project") %>% 
    mutate(user_cr_code = sprintf("ca_cr_%03d", seq_len(n())))
  
  pjs <- unique(projects$ProjectCode)
  
  contributor_LT <- ctrib %>%
    select(
      user_cr_code,
      vb_py_code = py_code,
      user_py_code,
      vb_ar_code = ar_code,
      contributor_type,
      record_identifier = ProjectCode
    ) %>% 
    rbind(data.frame(user_cr_code = paste0("cdfw_general_", pjs),
                     vb_py_code = rep(NA_character_, length(pjs)),
                     user_py_code = rep("cdfw_general", length(pjs)),
                     vb_ar_code = rep("ar.46", length(pjs)),
                     contributor_type = rep("Project", length(pjs)),
                     record_identifier = pjs))
  
  # don't include parties that are already in VB
  party_LT <- party_LT %>% 
    filter(is.na(py_code)) %>% 
    select(-py_code)
  
  out_path_party <- file.path(out_dir, "partyLT.csv")
  out_path_contributor <- file.path(out_dir, 'contributorLT.csv')
  cli::cli_alert_success("Writing two output files to:")
  cli::cli_ul(c(out_path_party, out_path_contributor))
  
  write_csv(party_LT, out_path_party)
  write_csv(contributor_LT, out_path_contributor)
  
}
