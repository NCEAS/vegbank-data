library(tidyverse)
library(here)
library(stringr)
source("Build_Loader_Table.R")

# load in CDFW data -------------------------------------------------------
# RAPlots
csv_path <- here("data", "RAPlots.csv")
plots <- read_csv(csv_path, show_col_types = FALSE)

# loading CA lookup tables
csv_path <- here("data", "LSoil.csv")
soil_lookup <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------

soil_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "SoilData",
  source_df = plots
)

soil_LT <- soil_template_fields$template

# Checking values ---------------------------------------------------------

# making sure values that should be from L* tables are found in L* tables
# all values are valid.

# Soil
plots %>%
  filter(!Soil_text %in% soil_lookup$SoilTexture) %>%
  distinct(Soil_text)

# Assigning column to loader table ---------------------------------------

soil_LT$soilDescription = plots$Soil_text

# For now there is only one variable that mapped to the Soil Data Loader table (Soil_text from RAPlots)
