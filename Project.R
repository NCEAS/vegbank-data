library(tidyverse)
library(here)
source("Build_Loader_Table.R")

# load in CDFW data -----------------------------------------------------------

# set folder where data is stored
folder <- 'data'

# read in RAProjects
projects <- read_csv(here(folder, 'RAProjects.csv'))

# creating loader table -------------------------------------------------------

project_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "Project",
  source_df = projects
)

project_LT <- project_template_fields$template

# checking values -------------------------------------------------------------

# ProjectStartDate and ProjectEndDate in RAProjects.csv has time
# Start_date and stop_date in the Projects loader table is date only
head(projects$ProjectStartDate)
head(projects$ProjectEndDate)

# tidying CDFW data -----------------------------------------------------------

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
