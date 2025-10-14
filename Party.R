library(tidyverse)
library(here)
library(stringr)
source("Build_Loader_Table.R")

# Personal Notes for Party (Will Delete):
# user_py_code: unique 'ca_***' identifier
# surname: RAProjects' DataContactName* fields (second half of split)
# given_name: RAProjects' DataContactName* fields (first half of split)
# middle_name: no mapping yet
# organization_name: RAProjects' DataContactOrganization
# email: RAProject's DataContactEmail
# orcid: no mapping yet
# ror: no mapping yet

# Personal Notes for Contributor (Will Delete):
# vb_py_code: no mapping yet
# user_py_code: unique 'ca_***' identifier (link to Party)
# role: RAProjects' DataContactRole (manually map to VegBank's 'ar.*')
# contributor_type: either 'observation', 'project', or 'class'
# recordIdentifier: maps to pj_code or ob_code

# load in CDFW data -------------------------------------------------------

# RAProjects
csv_path <- here("data", "RAProjects.csv")
projects  <- read_csv(csv_path, show_col_types = FALSE)

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

# Checking values ---------------------------------------------------------

# DataContactName* (RAProjects) - given_name (Party) + surname (Party)
# DataContactName* values Should be separated into `FirstName` and `LastName`.
# Change RAProjects to long format to have one row per contact.
unique(projects$DataContactName)
unique(projects$DataContactName2)
unique(projects$DataContactName3)
class(projects$DataContactName) # character
class(projects$DataContactName2) # character
class(projects$DataContactName3)# character

# DataContactEmail* (RAProjects) - email (Party)
# Values should be valid email addresses.
# Change RAProjects to long format to have one row per contact.
unique(projects$DataContactEmail)
unique(projects$DataContactEmail2)
unique(projects$DataContactEmail3)
class(projects$DataContactEmail) # character
class(projects$DataContactEmail2) # character
class(projects$DataContactEmail3) # character

# DataContactRole* (RAProjects) - role (Contributor)
# Values must be matched and changed to VegBank ar_codes.
# (Most likely done manually)
unique(projects$DataContactRole)
unique(projects$DataContactRole2)
unique(projects$DataContactRole3)
class(projects$DataContactRole) # character
class(projects$DataContactRole2) # character
class(projects$DataContactRole3) # character


# Tidying Data -------------------------------------------------------

# DataContactName should be a first and last name
# Making sure each value is two words
projects %>%
  mutate(word_count = str_count(DataContactName, "\\S+")) %>%
  count(word_count)

# Email should be a valid email address

email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"

invalid_emails <- projects %>%
  filter(!str_detect(DataContactEmail, email_pattern) | is.na(DataContactEmail))

unique(invalid_emails$DataContactEmail)

# All email and name values are valid

# Changing to RAProjects to long format to have one row per contact

projects_long <- projects %>%
  pivot_longer(
    cols = matches("^DataContact(Name|Email|Organization|Role)\\d*$"),
    names_to   = c(".value", "slot"),
    names_pattern = "^DataContact(Name|Email|Organization|Role)(\\d*)$"
  ) %>%
  filter(!is.na(Name) & str_squish(Name) != "") %>%
  transmute(
    ContactName  = str_squish(Name),
    ContactEmail = str_squish(Email),
    ContactOrg = str_squish(Organization),
    ContactRole = str_squish(Role)
  )

# Adjusting number of rows in party_LT to account for pivot.
n_new <- 30  # number of rows to add

party_LT <- bind_rows(
  party_LT,
  as_tibble(map(party_LT, ~ rep(NA, n_new)))
)

### given_name (Party) + surname (Party) ###
# Separating DataContactName into `FirstName` and `LastName`.

projects <- projects_long %>%
  separate(ContactName, into = c("FirstName", "LastName"), sep = " ", extra = "merge")

### user_py_code (Party) ###
# Create a unique code for each individual (ca_***)
projects <- projects %>%
  mutate(user_py_code = sprintf("ca_%03d", seq_len(n())))


### role (Contributor) ###
# Map DataContactRole values to ar.* codes
unique(projects$ContactRole)
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
    RoleCode = recode(ContactRole, !!!role_map)
  )

# Assigning columns to loader table ---------------------------------------
party_LT$user_py_code <- projects$user_py_code
party_LT$surname = projects$LastName
party_LT$organization_name = projects$ContactOrg
party_LTgiven_name = projects$FirstName
party_LT$email = projects$ContactEmail


# py_code, middle_name, party_type, org_position, orcid, and ror not present in CDFW data.
