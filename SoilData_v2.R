library(tidyverse)
library(here)
library(stringr)

# load in CDFW data -------------------------------------------------------
# RAPlots
csv_path <- here("data", "RAPlots.csv")
plots <- read_csv(csv_path, show_col_types = FALSE)

# loading CA lookup tables
csv_path <- here("data", "LSoil.csv")
soil_lookup <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------
# create a header csv with only variable names
csv_path <- here("loader_tables", "SoilData_Header.csv")
template <- read_csv(csv_path, show_col_types = FALSE)

# Checking values ---------------------------------------------------------

# making sure values that should be from L* tables are found in L* tables
# all values are valid.

# Soil
plots %>%
  filter(!Soil_text %in% soil_lookup$SoilTexture) %>%
  distinct(Soil_text)

# Assigning column to loader table ---------------------------------------

SoilData_LT <- bind_rows(
  template,
  tibble(soilDescription = plots$Soil_text
         )
)

SoilData_LT$soilDescription

# For now there is only one variable that mapped to the Soil Data Loader table (Soil_text from RAPlots)