library(tidyverse)
library(here)
library(stringr)

# load in CDFW data -------------------------------------------------------
# RAPlants
csv_path <- here("data", "RAPlants.csv")
plants <- read_csv(csv_path, show_col_types = FALSE)

# loading CA lookup tables
csv_path <- here("data", "USDA_PLANTS.csv")
plants_lookup <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------

strata_cover_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "StrataCoverData",
  source_df = plants
)

strata_cover_LT <- strata_cover_template_fields$template

# Checking values ---------------------------------------------------------

# making sure values that should be from SpeciesName are found in the USDA plants table.
# Some values are not found directly in the table.

plants %>%
  filter(!SpeciesName %in% plants_lookup$ScientificName) %>%
  distinct(SpeciesName)

# making sure values from Stratum fit the CA Description
unique(plants$Stratum)
# It looks like there are some values that do not fit the CA Description:
# GitHub Issue #15

# Assigning columns to loader table ---------------------------------------

strata_cover_LT$user_sr_code <- plants$Stratum
strata_cover_LT$authorPlantName <- plants$SpeciesName
strata_cover_LT$cover <- plants$Species_cover


# All variables besides authorPlantName, cover, and user_sre_code were not matched and are left as 'NA'
