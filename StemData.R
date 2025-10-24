library(tidyverse)
library(here)
library(sf)
source('Build_Loader_Table.R')

# Problem: No Data in AltTreeDBH.csv

# load in CDFW data -----------------------------------------------------------

# set folder where data is saved
folder <- 'data'

# AltTreeDBH
alt_tree <- read_csv(here(folder, 'AltTreeDBH.csv'))

# creating loader table -------------------------------------------------------
stem_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "StemData",
  source_df = alt_tree
)

stem_LT <- stem_template_fields$template

# checking values -------------------------------------------------------------

# tidying CDFW data -----------------------------------------------------------

# assigning column to loader table --------------------------------------------

# save filled in loader table