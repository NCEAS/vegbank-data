library(tidyverse)
library(here)

# load in CDFW data -----------------------------------------------------------

# set folder where data is saved
folder <- 'data'

# RAProjects
projects <- read_csv(here(folder, 'RAProjects.csv'))

# no CA lookup table

# creating loader table -------------------------------------------------------

# getting number of rows
nrow <- nrow(projects)

# create blank data frame
project_LT <- data.frame(pj_code = rep(NA, nrow),
                         project_name  = rep(NA, nrow),
                         project_description = rep(NA, nrow),
                         start_date = rep(NA, nrow),
                         stop_date = rep(NA, nrow))

# checking values -------------------------------------------------------------

# Project Code needs to remove extra characters to convert them all into 4
# characters.
unique(projects$ProjectCode)
unique(projects$ProjectCode)[nchar(unique(projects$ProjectCode)) > 4]

# Start Date and End Date should have the time removed from them
unique(projects$ProjectStartDate)
unique(projects$ProjectEndDate)
class(projects$ProjectStartDate) # character
class(projects$ProjectEndDate) # character

# tidying CDFW data -----------------------------------------------------------

# Converting Project Code into 4 characters
projects_merged <- projects %>% 
  mutate(ProjectCode = substr(ProjectCode, 1, 4))

# Converting Start Date and End Time to just Dates
# Start Date
projects_merged$ProjectStartDate <- as_date(mdy_hms(projects$ProjectStartDate))
projects_merged$ProjectStartDate
projects$ProjectStartDate

# End Date
projects_merged$ProjectEndDate <- as_date(mdy_hms(projects$ProjectEndDate))
projects_merged$ProjectEndDate
projects$ProjectEndDate
                                            
# assigning columns to loader table -------------------------------------------

project_LT$pj_code <- projects_merged$ProjectCode
project_LT$project_name <- projects$ProjectName
project_LT$project_description <- projects$ProjectDescription
project_LT$start_date <- projects_merged$ProjectStartDate
project_LT$stop_date <- projects_merged$ProjectEndDate

# save filled in loader table
write_csv(project_LT, here('loader_tables', 'ProjectLT.csv'))
