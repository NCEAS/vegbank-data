library(tidyverse)
library(here)
library(stringr)
library(vctrs)
source("Build_Loader_Table.R")

# Personal Notes for Party (Will Delete):
# user_py_code: unique 'ca_***' identifier
# surname: RAProjects' DataContactName* fields (second half of split)
# given_name: RAProjects' DataContactName* fields (first half of split)
# middle_name: no mapping yet
# organization_name: RAProjects' DataContactOrganization
# email: RAProject's DataContactEmail
# orcid: no mapping
# ror: no mapping

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


# DataContactOrganization* (RAProjects) ~ organization_name (Party)
unique(projects$DataContactOrganization)
unique(projects$DataContactOrganization2)
unique(projects$DataContactOrganization3)
class(projects$DataContactOrganization) # character
class(projects$DataContactOrganization2) # character
class(projects$DataContactOrganization3) # character

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

projects
### user_py_code (Party) ###
# Create a unique code for each individual (ca_***)
projects <- projects %>%
  mutate(user_py_code = sprintf("ca_%03d", seq_len(n())))


### role (Contributor) ###
# Map DataContactRole values to ar.* codes
# Done manually for each value
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
    RoleCode = recode(ContactRole, !!!role_map),
    RoleCode = if_else(is.na(RoleCode) | str_squish(ContactRole) == "", "ar.46", RoleCode)
  )

### contributor_type (Contributor) ###
# Since we are in RAProjects, each value will be "Project" for now
# Can potentially merge with something else to add values "Observation" or "Class"
# But, there is not contributor data for these

projects <- projects %>%
  mutate(
    contributor_type = "Project"
  )

# Assigning columns to loader table ---------------------------------------
party_LT$user_py_code <- projects$user_py_code
party_LT$surname <- projects$LastName
party_LT$organization_name <- projects$ContactOrg
party_LT$given_name <- projects$FirstName
party_LT$email <- projects$ContactEmail
party_LT$middle_name <- projects$MiddleName

contributor_LT$user_py_code <- projects$user_py_code
contributor_LT$role <- projects$RoleCode
contributor_LT$contributor_type <- projects$contributor_type
contributor_LT$recordIdentifier <- projects$ProjectCode

# -------------------------------------------------------------------------

### vb_py_code (Contributor) ###
# If the person matches, turn user_py_code into vb_py_code

# Turn get_all_parties() output into a dataframe
vegbankr::set_vb_base_url("https://api-dev.vegbank.org")
party_vegbank <- as.data.frame(vegbankr::get_all_parties())
# Note: I am getting a warning message suddenly now for saying there is
# In canonicalize_names(vb_data):
# Unmatched names: given_name, middle_name, organization_name, contact_instructions
# I'm going to ignore this for now

# Create a "full_name" key in both data frames
df1 <- party_LT %>% 
  mutate(
    full_name = pmap_chr(
      list(given_name, middle_name, surname),
      ~ str_trim(str_to_lower(paste(na.omit(c(...)), collapse = " ")))
    )
  )

df2 <- party_vegbank %>% 
  mutate(
    full_name = pmap_chr(
      list(given_name, middle_name, surname),
      ~ str_trim(str_to_lower(paste(na.omit(c(...)), collapse = " ")))
    )
  )

# Create named vector from df1 for fast lookup
py_code_lookup <- setNames(df1$user_py_code, df1$full_name)

# Count how many times each name appears
name_counts <- df1 %>% 
  count(full_name, name = "name_count")
name_counts2 <- df2 %>% 
  count(full_name, name = "name_count")

# Join name_counts to df1
df1 <- df1 %>%
  left_join(name_counts, by = "full_name")
# Join name_counts2 to df2
df2 <- df2 %>% 
  left_join(name_counts2, by = "full_name")

# Transform user_py_code format to patch py_code format
# ca_055 -> py.055
# Update after Maggie confirms
# Leading zeroes?
df1 <- df1 %>%
  mutate(user_py_code = paste0("py.", sprintf("%03d", 
                                              as.integer(str_remove(user_py_code, 
                                                                    "^ca_")))))

# Pick the first user_py_code per full_name key for df1
vb_code_lookup <- df1 %>%
  group_by(full_name) %>%
  summarise(vb_py_code = first(user_py_code), .groups = "drop")

# Pick the first user_py_code per full_name key for df2
vb_code_lookup2 <- df2 %>% 
  group_by(full_name) %>% 
  summarise(vb_py_code = first(py_code), .groups = "drop")

# Question: py_code is in a different format than user_py_code. Which format?
# Assuming py_code is in py.4317 format

# Combine full_name
combined_df <- bind_rows(df1, df2)

# After figuring out the py_code format, change df1 to combined_df
# Group by full_name and combine code
combined_unique <- df1 %>% 
  group_by(full_name) %>% 
  summarise(
    given_name = first(na.omit(given_name)),
    middle_name = first(na.omit(middle_name)),
    surname = first(na.omit(surname)),
    vb_py_code = first(na.omit(user_py_code)),
    .groups = "drop"
  )

# Join the consistent vb_py_code to df3
df3 <- df1 %>%
   left_join(vb_code_lookup, by = "full_name")
# df3 <- df2 %>% 
#   left_join(vb_code_lookup2, by = "full_name")

# Join combined_df to full data
# You can uncomment after we figure out py_code format
# vb_lookup <- combined_df %>% 
#   group_by(full_name) %>% 
#   summarise(vb_py_code = first(na.omit(user_py_code)), .groups = "drop")
# final_df <- combined_df %>% 
#   left_join(vb_lookup, by = "full_name")
# 
# # Only one row per person from join to full data
# final_df <- final_df %>% 
#   distinct(full_name, .keep_all = TRUE)

# Match and assign
df3 <- df3 %>%
  mutate(vb_py_code = py_code_lookup[full_name])
