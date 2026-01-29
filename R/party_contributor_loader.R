library(tidyverse)
library(stringr)
library(vctrs)
library(glue)
source("R/build_loader_table.R")

# load in CDFW data -------------------------------------------------------

party_contributor_loader <- function(in_dir, out_dir){
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  # read in RAProjects
  project_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = "RAProjects.csv", value = TRUE)
  
  cli::cli_alert_info(paste("Processing", length(project_files), "people/contributor tables from:"))
  cli::cli_ul(project_files)
  
  projects_df_list <- lapply(project_files, read_csv, progress = FALSE, show_col_types = FALSE)
  projects <- do.call(bind_rows, projects_df_list)
  
  # creating loader table ---------------------------------------------------
  party_template_fields <- build_loader_table(
    sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
    sheet = "Party",
    source_df = projects
  )
  
  contributor_template_fields <- build_loader_table(
    sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
    sheet = "Contributor",
    source_df = projects
  )
  
  party_LT <- party_template_fields$template
  contributor_LT <- contributor_template_fields$template
  
  # Tidying Data ------------------------------------------------------
  
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
  
  if (any(name_count$word_count > 2)){
    names <- name_count$ContactName[which(name_count$word_count > 2)]
    #warning(glue("Some ContactName entries contain more than two words. The loader table does not accept middle names. The following names may need special attention to ensure correct parsing: {names}"))
  }
  
  # Email should be a valid email address
  
  email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
  invalid_emails <- projects_long %>%
    filter(!str_detect(ContactEmail, email_pattern))
  
  if (nrow(invalid_emails) > 0){
    cli::cli_alert_warning("Some email addresses appear to be invalid: {.emph {paste(invalid_emails$ContactEmail, collapse = ', ')}}")
  }
  
  # Adjusting number of rows in party_LT to account for pivot
  needed_n <- nrow(projects_long)
  have_n <- nrow(party_LT)
  need <- needed_n - have_n
  
  if (need > 0) {
    # create `need` NA rows with the SAME types as party_LT
    blank_rows <- as_tibble(
      map(party_LT, ~ vec_init(.x, n = need))
    )
    party_LT <- bind_rows(party_LT, blank_rows)
  }
  
  if (need > 0) {
    # create `need` NA rows with the SAME types as contributor_LT
    blank_rows <- as_tibble(
      map(contributor_LT, ~ vec_init(.x, n = need))
    )
    contributor_LT <- bind_rows(contributor_LT, blank_rows)
  }
  
  ### given_name (Party) + surname (Party) ###
  # Separating DataContactName into `FirstName` and `LastName`.
  # Currently no middle names, but can optionally separate middle name as well.
  # If no middle name, it is set to NA
  
  projects <- projects_long %>%
    extract(
      ContactName,
      into  = c("FirstName", "MiddleName", "LastName"),
      regex = "^\\s*([^\\s]+)\\s+(?:([^\\s]+)\\s+)?(.+?)\\s*$",
      remove = FALSE
    ) %>%
    mutate(
      MiddleName = na_if(MiddleName, ""),
      across(c(FirstName, MiddleName, LastName), str_squish)
    )
  
  ### user_py_code (Party) ###
  # Create a unique code for each individual (ca_***)
  projects <- projects %>%
    mutate(user_py_code = sprintf("ca_%03d", seq_len(n()))) 
  
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
  role_lookup <- read.csv(paste0(in_dir, "/lookup-tables/cdfw-roles.csv")) %>% 
    rename(ContactRole = found, role_name = allowed)
  

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
  
  ### vb_py_code (Contributor) ###
  # If the person matches, turn user_py_code into vb_py_code
  
  # Turn get_all_parties() output into a dataframe
  suppressMessages(vegbankr::vb_set_base_url("https://api-dev.vegbank.org"))
  party_vegbank <- as.data.frame(vegbankr::vb_get_parties(limit = 5000))
  
  # Create a "full_name" key in both data frames
  project_names <- projects %>% 
    mutate(
      full_name = pmap_chr(
        list(FirstName, MiddleName, LastName),
        ~ str_trim(str_to_lower(paste(na.omit(c(...)), collapse = " ")))
      )
    )
  
  vegbank_names <- party_vegbank %>%
    mutate(
      full_name = pmap_chr(
        list(given_name, middle_name, surname),
        ~ str_trim(str_to_lower(paste(na.omit(c(...)), collapse = " ")))
      )
    )
  
  # Check for matching names across data sets 
  matching_names <- intersect(project_names$full_name, vegbank_names$full_name)
  
  # Keep 63 entries; keep party_LT; keep df1
  # Check if the person matches from party_LT to party_vegbank
  # If there is a match between people names from party_LT and party_vegbank,
  # then save py_code as vb_py_code. Otherwise, leave it as NA
  
  # df2 subset of distinct vegbank parties (vegbank party)
  veg_subset <- vegbank_names %>% 
    select(full_name, py_code) %>% 
    distinct(full_name, .keep_all = TRUE)
  
  # Left join to get vb_py_code
  project_names_j <- project_names %>% 
    left_join(veg_subset, by = "full_name") %>% 
    mutate(user_cr_code = sprintf("ca_cr_%03d", seq_len(n())))
  
  if (nrow(project_names_j) > nrow(project_names)){
    warning("Joining vegbank party identifiers resulted in duplicated rows. This could indicate an issue with the vegbank party codes, the input data, or both.")
  }
  
  
  # Assigning columns to loader table ---------------------------------------
  party_LT$user_py_code <- projects$user_py_code
  party_LT$surname <- projects$LastName
  party_LT$organization_name <- projects$ContactOrg
  party_LT$given_name <- projects$FirstName
  party_LT$email <- projects$ContactEmail
  party_LT$middle_name <- projects$MiddleName
  
  contributor_LT$user_cr_code <- project_names_j$user_cr_code
  contributor_LT$vb_py_code <- project_names_j$py_code
  contributor_LT$user_py_code <- project_names_j$user_py_code
  contributor_LT$vb_ar_code <- project_names_j$ar_code
  contributor_LT$contributor_type <- project_names_j$contributor_type
  contributor_LT$record_identifier <- project_names_j$ProjectCode
  
  # saved filled in loader table --------------------------------------------
  out_path_party <- file.path(out_dir, "partyLT.csv")
  out_path_contributor <- file.path(out_dir, 'contributorLT.csv')
  cli::cli_alert_success("Writing two output files to:")
  cli::cli_ul(c(out_path_party, out_path_contributor))
  
  write_csv(party_LT, out_path_party)
  write_csv(contributor_LT, out_path_contributor)
  
}
