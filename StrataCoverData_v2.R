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
# create a header csv with only variable names
csv_path <- here("loader_tables", "StrataCoverData_Header.csv")
template <- read_csv(csv_path, show_col_types = FALSE)

# Checking values ---------------------------------------------------------

# making sure values that should be from SpeciesName are found in the USDA plants table.
# Some values are not found directly in the table.

plants %>%
  filter(!SpeciesName %in% plants_lookup$ScientificName) %>%
  distinct(SpeciesName)

# Assigning columns to loader table ---------------------------------------

# Changing column types to support to data
template <- template %>%
  mutate(
    cover = as.numeric(cover)
  )

StrataCoverData_LT <- bind_rows(
  template,
  tibble(authorPlantName = plants$SpeciesName,
         cover = plants$Species_cover
  )
)

# All variables besides authorPlantName and cover were not matched and are left as 'NA'
