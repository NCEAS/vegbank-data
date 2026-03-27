library(tidyverse)
library(stringr)
# load in CDFW data -------------------------------------------------------
# RAPlots

#' Extracts soil texture descriptions from RAPlots.csv files, validates values
#' against a lookup table, and generates a soil loader table
#' 
#' @param in_dir Directory of VegBank data to read from. Can be a full file path or relative to working directory. 
#' @param out_dir Directory of data to write to. Can be a full file path or relative to working directory. 
#' 
#' @return None. Writes loader table soilLT.csv to `out_dir`
#' 
#' @details
#' This function performs data loading, data cleaning and checks soil texture
#' descriptions against the lookup table.
soil_loader <- function(in_dir, out_dir){
  
  in_dir  <- here::here(in_dir)
  out_dir <- here::here(out_dir)
  
  plot_files <- dir(in_dir, full.names = TRUE, recursive = TRUE) %>% 
    grep(pattern = "RAPlots.csv", value = TRUE)
  
  cli::cli_alert_info(paste("Processing", length(plot_files), "soil tables from:"))
  cli::cli_ul(plot_files)
  
  plots_df_list <- lapply(plot_files, 
                          read_csv,
                          progress = FALSE,
                          show_col_types = FALSE,
                          col_types = cols(`PalmJoshua` = col_character(),
                                           `DesertRip` = col_character()),
                          guess_max = 20000)
  plots <- do.call(bind_rows, plots_df_list)
  
  
  # clean up strings
  plots_clean <- plots %>% 
    select(SurveyID, Soil_text) %>% 
    mutate(Soil_text = gsub("\\s+", " ", Soil_text)) %>%  # replace multi-spaces with just one space
    mutate(Soil_text = case_when(
      Soil_text == "Not Recorded" ~ NA,
      Soil_text == "<Null>" ~ NA
    )) # replace null looking values with actual NAs
  
  soil_LT <- plots %>% 
    select(user_ob_code = SurveyID,
           description = Soil_text) %>% 
    mutate(horizon = "A") %>% 
    mutate(user_so_code = paste0("SO_", row_number())) %>% 
    drop_na() %>% 
    convert_df_to_utf8()
  
  # save filled in loader table
  out_path <- file.path(out_dir, "soilLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(soil_LT, out_path)
  
}
