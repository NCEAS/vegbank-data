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
  
  message(paste("Processing", length(project_files), "people/contributor tables from:", paste(project_files, collapse = ", ")))
  
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
    warning(glue("Some ContactName entries contain more than two words. The loader table does not accept middle names. The following names may need special attention to ensure correct parsing: {names}"))
  }
  
  # Email should be a valid email address
  
  email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
  invalid_emails <- projects_long %>%
    filter(!str_detect(ContactEmail, email_pattern))
  
  if (nrow(invalid_emails) > 0){
    warning(glue("Some email addresses appear to be invalid: {paste(invalid_emails$ContactEmail, collapse = ', ')}"))
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
  
  
  ### role (Contributor) ###
  # Map DataContactRole values to ar.* codes
  # Done manually for each value
  role_map <- c(
    "project lead" = "ar.18",
    "regional biologist who helped with field effort" = "ar.43",
    "regional biologist who met with us on reviving project" = "ar.40",
    "Classification lead for the project." = "ar.34",
    "NPS lead and data collector" = "ar.36",
    "California Native Plant Program" = "ar.19",
    "planning" = "ar.46",
    "provided student workers" = "ar.40",
    "field lead" = "ar.43",
    "Project Lead" = "ar.18",
    "Lead Biologist Interpreter" = "ar.18",
    "Senior Environmental Scientist" = "ar.18",
    "Environmental Scientist/ Vegetation Ecologist" = "ar.36",
    "Associate Wildlife Biologist, Land Program - North, Eastern Sierra - Inland Deserts Region" = "ar.36",
    "CNPS Vegetation Program" = "ar.19",
    "Program Lead" = "ar.18",
    "Project Manager" = "ar.18",
    "Managed field crew and database entry for the project" = "ar.19",
    "classification" = "ar.34",
    "data management" = "ar.19",
    "VegCAMP Lead Biologist" = "ar.18",
    "Vegetation Ecologist" = "ar.36",
    "GIS analyst and lead photointerpreter" = "ar.54",
    "VegCAMP lead" = "ar.18",
    "surveyor" = "ar.36",
    "Field coordinator and data collector" = "ar.36",
    "Senior Scientist" = "ar.18",
    "Obtained data for CNPS" = "ar.56",
    "primary surveyor" = "ar.36",
    "Collected additional data for the project and managed data entry for their surveys" = "ar.19",
    "Environmental Scientist / Vegetation Ecologist" = "ar.36",
    "Project manager, including mainting sub-contract with Prunske-Chattam for surveying" = "ar.18",
    "vegetation program lead" = "ar.18",
    "Senior Biologist during the time of the project. CDFW performed RAs and releves, and classified them" = "ar.34",
    "GIC used these surveys and classification of them to map the area." = "ar.56",
    "Lead biologist for VegCAMP" = "ar.18",
    "Prinicpal contact for AECOM" = "ar.17",
    "classifier" = "ar.34",
    "field staff and mapper" = "ar.43"
  )
  
  projects <- projects %>%
    mutate(
      RoleCode = recode(ContactRole, !!!role_map),
      RoleCode = if_else(is.na(RoleCode) | str_squish(ContactRole) == "", "ar.46", RoleCode)
    )
  
  if (!all(grepl("^ar\\.", projects$RoleCode))){
    uncat <- grep("^ar\\.", projects$RoleCode, value = TRUE, invert = TRUE)
    warning(glue("The following project roles were not transformed into vegbank ar.* codes: {paste(uncat, collapse = ', ')}"))
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
  SuppressMessages(vegbankr::vb_set_base_url("https://api-dev.vegbank.org"))
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
    left_join(veg_subset, by = "full_name")
  
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
  
  contributor_LT$vb_py_code <- project_names_j$py_code
  contributor_LT$user_py_code <- project_names_j$user_py_code
  contributor_LT$role <- project_names_j$RoleCode
  contributor_LT$contributor_type <- project_names_j$contributor_type
  contributor_LT$recordIdentifier <- project_names_j$ProjectCode
  
  # saved filled in loader table --------------------------------------------
  out_path_party <- file.path(out_dir, "partyLT.csv")
  out_path_contributor <- file.path(out_dir, 'contributorLT.csv')
  message(paste("Writing two output files to:", out_path_party, out_path_contributor))
  
  write_csv(party_LT, out_path_party)
  write_csv(contributor_LT, out_path_contributor)
  
}
