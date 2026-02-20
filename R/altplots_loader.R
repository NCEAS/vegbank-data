library(tidyverse)
library(here)
library(stringr)

load_altplots_files <- function(in_dir, out_dir) {
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>% 
    grep(pattern = "VegBankProject", value = TRUE)
  
  # Read in AltPlots
  altplot_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = "AltPlots.csv", value = TRUE)
  
  
  altplots_df_list <- lapply(altplot_files, read_csv, progress = FALSE,
                             show_col_types = FALSE)
  
  # Addressing character and double combo error
  altplots_df_list <- lapply(altplots_df_list, function(df) {
    if ("Representative" %in% names(df)) {
      df$Representative <- as.character(df$Representative) 
    }
    return(df)
  })
  
  altplots <- do.call(bind_rows, altplots_df_list)
  
  # Read in RAPlots
  raplot_files <- dir(sub_folders, full.names = TRUE) %>%
    grep(pattern = "RAPlots.csv", value = TRUE)
  
  raplots_df_list <- lapply(raplot_files, read_csv, progress = FALSE,
                            show_col_types = FALSE)
  
  # Addressing character and double combo error
  raplots_df_list <- lapply(raplots_df_list, function(df) {
    if ("DesertRip" %in% names(df)) {
      df$DesertRip <- as.character(df$DesertRip)
    }
    if ("PlotOther4" %in% names(df)) {
      df$PlotOther4 <- as.character(df$PlotOther4)
    }
    return(df)
  })
  
  raplots <- do.call(bind_rows, raplots_df_list)
  
  altplots <- altplots
  raplots <- raplots
  
  out <- list("altplots" = altplots, "raplots" = raplots)
  
  return(out)
}

# load in CDFW data -----------------------------------------------------------

# AltPlots
# TODO: read from out_dir instead of your local path here. see any of the other loader functions for examples. this is true for all read calls below
# csv_path <- here("data", "AltPlots.csv")
# altplots <- read_csv(csv_path, show_col_types = FALSE)

# RAPlots
# csv_path <- here("data", "RAPlots.csv")
# raplots <- read_csv(csv_path, show_col_types = FALSE)

# tidying data ----------------------------------------------------------------

# link AltPlots records to RAPlots by SurveyID
alt_ra_plots <- raplots %>% 
  left_join(altplots, by = "SurveyID")

# viewing PlotObservations loader table
plotsLT <- read_csv("data/loader-tables/plotsLT.csv")

# load current PlotObservations loader table's mappings
plotsLT_path <- file.path("data/loader-tables/plotsLT.csv")

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
