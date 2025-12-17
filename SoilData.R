library(tidyverse)
library(here)
library(stringr)
source("R/build_loader_table.R")

# load in CDFW data -------------------------------------------------------
# RAPlots
csv_path <- here("data", "RAPlots.csv")
plots <- read_csv(csv_path, col_types = cols(.default = col_guess(), 
                                             `PlotOther5` = col_character()))

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

# Soil_text (RAPlots) - soilDescription (SoilData)
# making sure values that should be from L* tables are found in L* tables
unique(plots$Soil_text)
class(plots$Soil_text) # character

# tidying CDFW data -----------------------------------------------------------

### soilDescription (SoilData) ###
# Soil_text (RAPlots)
# all values are valid.
plots %>%
  filter(!Soil_text %in% soil_lookup$SoilTexture) %>%
  distinct(Soil_text)

# Assigning column to loader table ---------------------------------------

soil_LT$soilTexture = plots$Soil_text

# For now there is only one variable that mapped to the Soil Data Loader table (Soil_text from RAPlots)

# save filled in loader table
write_csv(soil_LT, here('loader_tables', 'SoilDataLT.csv'))
