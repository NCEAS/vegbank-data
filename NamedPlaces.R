library(tidyverse)
library(here)
library(sf)
source('Build_Loader_Table.R')

# Problem: SurveyPoints is empty

# load in CDFW data -----------------------------------------------------------

# set folder where data is saved
folder <- 'data'

# SurveyPoints
surveypoints <- read_csv(here(folder, 'SurveyPoints.csv'))

# creating loader table -------------------------------------------------------
named_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "NamedPlaces",
  source_df = surveypoints
)

named_LT <- named_template_fields$template

# checking values -------------------------------------------------------------

# tidying CDFW data -----------------------------------------------------------

# assigning column to loader table --------------------------------------------

# save filled in loader table