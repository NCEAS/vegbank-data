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
proj_LT <- data.frame(pj_code = rep(NA, nrow),
                         project_name = rep(NA, nrow),
                         project_description = rep(NA, nrow),
                         start_date = rep(NA, nrow),
                         stop_date = rep(NA, nrow))

# Checking values ---------------------------------------------------------

# pj_code should be a 4 digit character code.
# some values are not exactly 4 digits, but are still character codes

invalid_codes <- projects %>%
  mutate(ProjectCode = str_squish(ProjectCode)) %>%
  filter(!str_detect(ProjectCode, "^[A-Z0-9]{4}$"))

# View all unique invalids
unique(invalid_codes$ProjectCode)

# start date and end date should take the form "MM/DD/YY hh:mm:ss"
# all values take the correct form.

ProjectStartDate_chr = str_squish(as.character(projects$ProjectStartDate))

date_pattern <- "^(0[1-9]|1[0-2])/([0-2][0-9]|3[0-1])/\\d{2} ([0-1][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"

invalid_dates <- projects %>%
  filter(!str_detect(ProjectStartDate, date_pattern))

unique(invalid_dates$ProjectStartDate)
unique(invalid_dates$ProjectEndDate)

# Tidying CDFW data -------------------------------------------------------
# Converting date format from MM/DD/YYYY hh:mm:ss to MM/DD/YYYY
projects <- projects %>%
  mutate(
    ProjectStartDate = format(mdy_hms(ProjectStartDate), "%m/%d/%Y"),
    ProjectEndDate = format(mdy_hms(ProjectEndDate), "%m/%d/%Y")
  )

# Assigning columns to loader table ---------------------------------------
proj_LT$pj_code <- projects$ProjectCode
proj_LT$project_name <- projects$ProjectName
proj_LT$project_description <- projects$ProjectDescription
proj_LT$start_date <- projects$ProjectStartDate
proj_LT$stop_date <- projects$ProjectEndDate


