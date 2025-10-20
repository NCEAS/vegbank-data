library(tidyverse)
library(here)
source("Build_Loader_Table.R")

# load in CDFW data -------------------------------------------------------

# set folder where data is saved
folder <- 'data'

# RAImpacts
impacts <- read_csv(here(folder, 'RAImpacts.csv')) 

# loading CA lookup table
impacts_lookup <- read_csv(here(folder, 'LImpacts.csv'))

# creating loader table ---------------------------------------------------

disturb_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "DisturbanceData",
  source_df = impacts
)

disturb_LT <- disturb_template_fields$template

# Checking values ---------------------------------------------------------

# Intensity should be 1, 2, or 3
# how will we handle values: 0, 10, 999, and 52?
unique(impacts$Intensity)

# Impact codes: are there any present in CDFW data that are not included in lookup table?
which(!impacts$CodeImpact %in% impacts_lookup$CodeImp)
impacts[!impacts$CodeImpact %in% impacts_lookup$CodeImp, ]


# Tidying CDFW data -------------------------------------------------------

impacts_merged <- impacts %>% 
  # joining with lookup table by impact code
  left_join(impacts_lookup, by = c("CodeImpact" = "CodeImp")) %>% 
  # changing numeric code
  mutate(Intensity = case_when(
    Intensity == 1 ~ "Low",
    Intensity == 2 ~ "Moderate",
    Intensity == 3 ~ "High"
  ))


# Assigning columns to loader table ---------------------------------------
disturb_LT$ob_code <- impacts_merged$SurveyID
disturb_LT$disturbance_type <- impacts_merged$`Impact type`
disturb_LT$disturbance_comment <- impacts_merged$Other
disturb_LT$disturbance_intensity <- impacts_merged$Intensity

# disturbance age and extent not present in CDFW data:
disturb_LT$disturbance_age <- 'NA'
disturb_LT$disturbance_extent <- 'NA'

# save filled in loader table
write_csv(disturb_LT, here('loader_tables','DisturbanceDataLT.csv'))
