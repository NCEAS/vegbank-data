library(tidyverse)
library(here)
library(stringr)
source("Build_Loader_Table.R")

# Personal Notes (Will Delete):

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
    cols = matches("^DataContact(Name|Email|Organization)\\d*$"),
    names_to   = c(".value", "slot"),
    names_pattern = "^DataContact(Name|Email|Organization)(\\d*)$"
  ) %>%
  filter(!is.na(Name) & str_squish(Name) != "") %>%
  transmute(
    ContactName  = str_squish(Name),
    ContactEmail = str_squish(Email),
    ContactOrg = str_squish(Organization)
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
projects

# Assigning columns to loader table ---------------------------------------
party_LT$user_py_code <- projects$user_py_code
party_LT$surname = projects$LastName
party_LT$organization_name = projects$ContactOrg
party_LTgiven_name = projects$FirstName
party_LT$email = projects$ContactEmail

# py_code, middle_name, party_type, org_position, orcid, and ror not present in CDFW data.
