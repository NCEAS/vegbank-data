library(tidyverse)
source("R/build_loader_table.R")

# load in CDFW data -------------------------------------------------------

disturbance_loader <- function(in_dir, out_dir){
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  # read in RAImpacts
  impact_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = "RAImpacts.csv", value = TRUE)
  
  cli::cli_alert_info(paste("Processing", length(impact_files), "disturbance tables from:"))
  cli::cli_ul(impact_files)
  
  impacts_df_list <- lapply(impact_files, read_csv, progress = FALSE, show_col_types = FALSE)
  impacts <- do.call(bind_rows, impacts_df_list)

  
  impacts_lookup <- read_csv(file.path(in_dir, "lookup-tables/LImpacts.csv"), progress = FALSE, show_col_types = FALSE)
  

  # creating loader table ---------------------------------------------------
  
  disturb_template_fields <- build_loader_table(
    sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
    sheet = "DisturbanceData",
    source_df = impacts
  )
  
  disturb_LT <- disturb_template_fields$template
  
  # Checking values ---------------------------------------------------------
  
  # Intensity should be 1, 2, or 3
  # how will we handle values: 0, 10, 999, and 52?
  intensity_vals <- unique(impacts$Intensity)
  
  if (any(!(intensity_vals %in% c(1, 2, 3)))){
    unk <- intensity_vals[!(intensity_vals %in% c(1, 2, 3))]
    cli::cli_alert_warning("Unknown intensity values were found:")
    cli::cli_ul(unk)
  }
  
  
  # Impact codes: are there any present in CDFW data that are not included in lookup table?
  impacts_lookup_missing <- impacts %>% 
    # joining with lookup table by impact code
    anti_join(impacts_lookup, by = c("CodeImpact" = "CodeImp"))
  
  if (nrow(impacts_lookup_missing) > 0){
    cli::cli_alert_warning("The following impact codes were missing from the impacts lookup table:")
    cli::cli_ul(impacts_lookup_missing$CodeImpact)
  }
  
  
  # Tidying CDFW data -------------------------------------------------------
  
  impacts_merged <- impacts %>% 
    # joining with lookup table by impact code
    left_join(impacts_lookup, by = c("CodeImpact" = "CodeImp")) %>% 
    # changing numeric code
    mutate(Intensity = case_when(
      Intensity == 1 ~ "Low",
      Intensity == 2 ~ "Moderate",
      Intensity == 3 ~ "High"
    ))
  
  
  # Assigning columns to loader table ---------------------------------------
  disturb_LT$ob_code <- impacts_merged$SurveyID
  disturb_LT$disturbance_type <- impacts_merged$`Impact type`
  disturb_LT$disturbance_comment <- impacts_merged$Other
  disturb_LT$disturbance_intensity <- impacts_merged$Intensity
  
  # disturbance age and extent not present in CDFW data:
  disturb_LT$disturbance_age <- 'NA'
  disturb_LT$disturbance_extent <- 'NA'
  
  # save filled in loader table
  out_path <- file.path(out_dir, "disturbanceLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(disturb_LT, out_path)
}
