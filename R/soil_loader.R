library(tidyverse)
library(stringr)
# load in CDFW data -------------------------------------------------------
# RAPlots

soil_loader <- function(in_dir, out_dir){
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  plot_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = "RAPlots.csv", value = TRUE)
  
  cli::cli_alert_info(paste("Processing", length(plot_files), "soil tables from:"))
  cli::cli_ul(plot_files)
  
  plot_df_list <- lapply(plot_files, function(file) {
    df <- read_csv(file, progress = FALSE, show_col_types = FALSE)
    # column typing
    if ("DesertRip" %in% names(df)) {
      df$DesertRip <- as.character(df$DesertRip)
    }
    if ("PlotOther4" %in% names(df)) {
      df$PlotOther4 <- as.character(df$PlotOther4)
    }
    return(df)
  })
  
  plots <- do.call(bind_rows, plot_df_list)
  
  soil_lookup <- read_csv(file.path(in_dir, "lookup-tables/LSoil.csv"), progress = FALSE, show_col_types = FALSE)
  
  
  # clean up strings
  plots_clean <- plots %>% 
    select(SurveyID, Soil_text) %>% 
    mutate(Soil_text = gsub("\\s+", " ", Soil_text)) %>%  # replace multi-spaces with just one space
    mutate(Soil_text = case_when(
      Soil_text == "Not Recorded" ~ NA,
      Soil_text == "<Null>" ~ NA
    )) # replace null looking values with actual NAs

  no_lookup <- plots_clean %>%
    anti_join(soil_lookup, by = c("Soil_text" = "SoilTexture")) %>% 
    distinct(Soil_text) %>% 
    filter(!is.na(Soil_text))
  
  if (nrow(no_lookup) > 0){
    cli::cli_alert_warning("The following soil descriptions are missing from the lookup table:") # which we don't even do anything with anyway...but maybe we will eventually
    cli::cli_ul(no_lookup$Soil_text)
  }
  

  soil_LT <- plots %>% 
    select(user_ob_code = SurveyID,
           texture = Soil_text)
  
  # save filled in loader table
  out_path <- file.path(out_dir, "soilLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(soil_LT, out_path)
  
}
