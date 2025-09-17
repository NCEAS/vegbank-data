library(tidyverse)
library(here)
library(stringr)

# load in CDFW data -------------------------------------------------------
# RAProjects
csv_path <- here("data", "RAProjects.csv")
projects  <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------
# getting number of rows
nrow <-  nrow(projects)
# create blank data frame (probably better way to do this)
party_LT <- data.frame(py_code = rep(NA, nrow),
                      surname = rep(NA, nrow),
                      organization_name = rep(NA, nrow),
                      given_name = rep(NA, nrow),
                      middle_name = rep(NA, nrow),
                      party_type = rep(NA, nrow),
                      org_position = rep(NA, nrow),
                      email = rep(NA, nrow),
                      orcid = rep(NA, nrow),
                      ror = rep(NA, nrow))

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
# Separating DataContactName into `FirstName` and `LastName`.

project_name_split <- projects %>%
  separate(DataContactName, into = c("FirstName", "LastName"), sep = " ", extra = "merge")

# Assigning columns to loader table ---------------------------------------
party_LT$py_code <- 'NA'
party_LT$surname <- project_name_split$LastName
party_LT$organization_name <- project_name_split$DataContactOrganization
party_LT$given_name <- project_name_split$FirstName
party_LT$middle_name <- 'NA'
party_LT$party_type <- 'NA'
party_LT$org_position <- 'NA'
party_LT$email <- project_name_split$DataContactEmail
party_LT$orcid <- 'NA'
party_LT$ror <- 'NA'

# py_code, middle_name, party_type, org_position, orcid, and ror not present in CDFW data.
