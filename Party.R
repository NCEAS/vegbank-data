library(tidyverse)
library(here)
library(stringr)

# load in CDFW data -------------------------------------------------------
# RAProjects
csv_path <- here("data", "RAProjects.csv")
projects  <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------
# create a header csv with only variable names
csv_path <- here("loader_tables", "Party_Header.csv")
template <- read_csv(csv_path, show_col_types = FALSE)

# Checking values ---------------------------------------------------------

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

# Tidying Data -------------------------------------------------------
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

# Separating DataContactName into `FirstName` and `LastName`.

project_name_split <- projects_long %>%
  separate(ContactName, into = c("FirstName", "LastName"), sep = " ", extra = "merge")

# Assigning columns to loader table ---------------------------------------
party_LT <- bind_rows(
  template,
  tibble(surname = project_name_split$LastName,
         organization_name = project_name_split$ContactOrg,
         given_name = project_name_split$FirstName,
         email = project_name_split$ContactEmail)
)

# py_code, middle_name, party_type, org_position, orcid, and ror not present in CDFW data.
