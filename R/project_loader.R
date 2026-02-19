library(tidyverse)

#' Extract project loader table data from CDFW project files
#'
#' @param in_path Directory of files to read from. Function will look for files called RAProjects.csv
#' @param out_path Path to write output file to.
#'
#' @return
#' @export
#'
#' @examples
project_loader <- function(in_dir, out_dir){
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  # read in RAProjects
  project_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = "RAProjects.csv", value = TRUE)
  
  projects_df_list <- lapply(project_files, read_csv, progress = FALSE, show_col_types = FALSE)
  
  projects <- do.call(bind_rows, projects_df_list)
  
  # tidying CDFW data -----------------------------------------------------------
  
  ### Converting Start Date and End Time to just Dates ###
  projects$ProjectStartDate <- as_date(mdy_hms(projects$ProjectStartDate))
  projects$ProjectEndDate <- as_date(mdy_hms(projects$ProjectEndDate))
  
  # warn if dates are before 1900
  if (any(projects$ProjectStartDate < as.Date("1900-01-01"), na.rm = TRUE)) {
    cli::cli_alert_warning("Some project start dates are before 1900.")
  }
  
  if (any(projects$ProjectEndDate < as.Date("1900-01-01"), na.rm = TRUE)) {
    cli::cli_alert_warning("Some project end dates are before 1900.")
  }
  
  today <- Sys.Date()
  
  if (any(projects$ProjectStartDate > today, na.rm = TRUE)) {
    cli::cli_alert_warning("Some project start dates are in the future.")
  }
  
  if (any(projects$ProjectEndDate > today, na.rm = TRUE)) {
    cli::cli_alert_warning("Some project end dates are in the future.")
  }
  
  # assigning columns to loader table -------------------------------------------
  
  project_LT <- projects %>% rename(user_pj_code = ProjectCode,
                                    project_name = ProjectName,
                                    project_description = ProjectDescription,
                                    start_date = ProjectStartDate,
                                    stop_date = ProjectEndDate)
  
  project_LT <- project_LT %>%
    group_by(user_pj_code) %>% 
    arrange(desc(nchar(project_description))) %>% 
    slice(1)
  
  # save filled in loader table
  out_path <- file.path(out_dir, "projectLT.csv")
  cli::cli_alert_success("Writing output to: {out_path}")
  write_csv(project_LT, out_path)
  
}


