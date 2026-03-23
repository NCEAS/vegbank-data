library(tidyverse)

#' Extract project information from RAProjects files
#' and writes to a vegbank loader table.
#'
#' @param in_dir Directory of vegbank data to read from
#' @param out_dir Directory of data to write to
#'
#' @return None. Writes loader tables projectLT.csv to `out_dir`.
#' 
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Converts project start and end dates from datetime to date format
#'   \item Validates date ranges
#'   \item Handles duplicate project codes by retaining only the record with the longest
#'         (most detailed) project description
#'   \item Maps CDFW field names to VegBank loader table field names
#' }
project_loader <- function(in_dir, out_dir){
  
  # read in projects file
  project_files <- dir(in_dir, full.names = TRUE) %>% 
    grep(pattern = "RAProjects.csv", value = TRUE)
  
  projects_df_list <- lapply(project_files, read_csv, progress = FALSE, show_col_types = FALSE)
  
  projects <- do.call(bind_rows, projects_df_list)
  
  
  # tidying CDFW data -----------------------------------------------------------
  
  ### Converting Start Date and End Time to just Dates ###
  projects$ProjectStartDate <- as_date(mdy_hms(projects$ProjectStartDate, quiet = TRUE))
  projects$ProjectEndDate <- as_date(mdy_hms(projects$ProjectEndDate, quiet = TRUE))
  
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
  
  project_LT <- projects %>% select(user_pj_code = ProjectCode,
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


