library(tidyverse)
library(here)
library(stringr)
source("R/build_loader_table.R")

# load in CDFW data -----------------------------------------------------------

# AltPlots
csv_path <- here("data", "AltPlots.csv")
altplots <- read_csv(csv_path, show_col_types = FALSE)

# RAPlots
csv_path <- here("data", "RAPlots.csv")
raplots <- read_csv(csv_path, show_col_types = FALSE)

# tidying data ----------------------------------------------------------------

# link AltPlots records to RAPlots by SurveyID
alt_ra_plots <- raplots %>% 
  left_join(altplots, by = "SurveyID")

# viewing PlotObservations loader table
plotsLT <- read_csv("data/loader-tables/plotsLT.csv")

# load current PlotObservations loader table's mappings
plotsLT_path <- file.path("data/loader-tables/plotsLT.csv")

# if (file.exists(plotsLT_path)) {
#   plotsLT <- read_csv(plotsLT_path, show_col_types = FALSE)
#   
#   cli::cli_h2("Current PlotObservations fields from plots_loader.R:")
#   cli::cli_ul(names(plotsLT)[!is.na(plotsLT[1,])])  # Fields that have values
#   
# } else {
#   cli::cli_alert_warning("plotsLT.csv not found at {plots_LT_path}")
# }

# testing ---------------------------------------------------------------------
cli::cli_h2("AltPlots Columns:")
cli::cli_ul(names(altplots))

alt_cols_with_data <- altplots %>% 
  select(where(~!all(is.na(.x)))) %>% 
  names()

cli::cli_h2("AltPlots columns with non-NA values:")
cli::cli_ul(alt_cols_with_data)

mapped_alt_columns <- c("SurveyID")

# get all columns from AltPlots
all_alt_columns <- names(altplots)

# find unmapped columns
unmapped_columns <- setdiff(all_alt_columns, mapped_alt_columns)

cli::cli_h2("Unmapped AltPlots Columns ({length(unmapped_columns)} total):")
cli::cli_ul(unmapped_columns)

# show which unmapped columns actually have data
unmapped_with_data <- altplots %>% 
  select(all_of(unmapped_columns)) %>% 
  select(where(~!all(is.na(.x)))) %>% 
  names()

cli::cli_h2("Unmapped columns that contain non-NA values ({length(unmapped_with_data)}):")
cli::cli_ul(unmapped_with_data)
