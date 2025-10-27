library(tidyverse)
library(here)
source("Build_Loader_Table.R")

# load in CDFW data -----------------------------------------------------------

# set folder where data is saved
folder <- 'data'

# RAProjects
projects <- read_csv(here(folder, 'RAProjects.csv'))

# no CA lookup table

# creating loader table -------------------------------------------------------

project_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "Project",
  source_df = projects
)

project_LT <- project_template_fields$template

# checking values -------------------------------------------------------------

# ProjectCode (RAProjects) - user_pj_code (Projects)
# ProjectCode should be a 4 digit character code
unique(projects$ProjectCode)
class(projects$ProjectCode) # character

# ProjectStartDate (RAProjects) + ProjectEndDate (RAProjects) - start_date (Projects) + stop_date (Projects)
# Start Date and End Date should have the time removed from them
unique(projects$ProjectStartDate)
unique(projects$ProjectEndDate)
class(projects$ProjectStartDate) # character
class(projects$ProjectEndDate) # character

# tidying CDFW data -----------------------------------------------------------

### user_pj_code (Projects) ###
# Confirming values of the correct form (4 characters)
# Some values are not exactly 4 digits, but are still character codes

invalid_codes <- projects %>%
  mutate(ProjectCode = str_squish(ProjectCode)) %>%
  filter(!str_detect(ProjectCode, "^[A-Z0-9]{4}$"))

# View all unique invalids
unique(invalid_codes$ProjectCode)

### start_date (Project) + stop_date (Project) ###
# Converting Start Date and End Time to just Dates
# Start Date
projects$ProjectStartDate <- as_date(mdy_hms(projects$ProjectStartDate))
projects$ProjectStartDate

# End Date
projects$ProjectEndDate <- as_date(mdy_hms(projects$ProjectEndDate))
projects$ProjectEndDate
                                            
# assigning columns to loader table -------------------------------------------

project_LT$user_pj_code <- projects$ProjectCode
project_LT$project_name <- projects$ProjectName
project_LT$project_description <- projects$ProjectDescription
project_LT$start_date <- projects$ProjectStartDate
project_LT$stop_date <- projects$ProjectEndDate

# save filled in loader table
write_csv(project_LT, here('loader_tables', 'ProjectLT.csv'))
