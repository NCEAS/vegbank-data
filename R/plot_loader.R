library(tidyverse)
library(sf)
library(tigris)
library(rnaturalearth)
library(vegbankr)
source("R/build_loader_table.R")
options(tigris_use_cache = TRUE, tigris_class = "sf")

# Remaining Issues (Wait for Update):
# location_accuracy
# slope_gradient
# methodNarrative
# taxonObservationArea
# observationNarrative
# landscapeNarrative
# tree_Ht
# basalArea
# nameOther

# load in CDFW data -----------------------------------------------------------

# convert a data frame with a crs (epsg code) and an easting/northing coordinate ("UTME_final", "UTMN_final") to lat/lon coordinates
convert_to_ll <- function(df_group) {
  crs_utm <- unique(df_group$crs)
  if (length(crs_utm) != 1 || is.na(crs_utm)) return(NULL)
  
  df <- st_as_sf(df_group,
                 coords = c("UTME_final", "UTMN_final"),
                 crs = crs_utm,
                 na.fail = FALSE
  ) %>%
    st_transform(4326) %>%
    mutate(
      real_longitude = st_coordinates(geometry)[, 1],
      real_latitude = st_coordinates(geometry)[, 2]
    ) %>%
    st_drop_geometry() %>%
    select(.row_id, real_longitude, real_latitude)
}

# read in files from input directory and join into one table
load_files <- function(in_dir) {
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  plot_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = 'RAPlots.csv', value = TRUE)
  alt_plot_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = 'AltPlots.csv', value = TRUE)
  #survey_point_files <- dir(sub_folders, full.names = TRUE) %>% 
  #  grep(pattern = 'SurveyPoints.csv', value = TRUE)
  impact_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = 'RAImpacts.csv', value = TRUE)
  alt_strata_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = 'AltStrata.csv', value = TRUE)
  classification_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = 'RAClassification.csv', value = TRUE)
  project_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = 'RAProjects.csv', value = TRUE)
  
  
  plots_df_list <- lapply(plot_files, 
                          read_csv,
                          progress = FALSE,
                          show_col_types = FALSE,
                          col_types = cols(`PalmJoshua` = col_character(),
                                           `DesertRip` = col_character()),
                          guess_max = 20000)
  plots <- do.call(bind_rows, plots_df_list)
  alt_plots_df_list <- lapply(alt_plot_files,
                              read_csv,
                              progress = FALSE,
                              show_col_types = FALSE,
                              col_types = cols(`Representative` = col_character()),
                              guess_max = 20000)
  alt_plots <- do.call(bind_rows, alt_plots_df_list)
  alt_strata_df_list <- lapply(alt_strata_files, read_csv, progress = FALSE, show_col_types = FALSE, guess_max = 20000)
  alt_strata <- do.call(bind_rows, alt_strata_df_list)
  
  # join AltPlots and AltStrata with RAPlots
  plots_merged <- plots %>% 
    left_join(alt_plots, by = "SurveyID") %>% 
    left_join(alt_strata, by = "SurveyID") %>% 
    select(where(~!all(is.na(.x)))) %>% # dropping columns that are filled with NA values
    mutate(SurveyDate = as.Date(lubridate::mdy_hms(SurveyDate))) # drop time from date column after reformatting 
  
  if (any(plots_merged$SurveyDate < as.Date("1900-01-01"), na.rm = TRUE)) {
    cli::cli_alert_warning("Some survey dates are before 1900.")
  }
  
  today <- Sys.Date()
  
  if (any(plots_merged$SurveyDate > today, na.rm = TRUE)) {
    cli::cli_alert_warning("Some survey dates are in the future.")
  }
  
  return(plots_merged)
  
}

# normalize elevation with correct units, and error measurement
normalize_elevation <- function(plots_merged){
  
  ### elevation (PlotObservations) ###
  # Elevation (RAPlots) related to ft_mElevation (RAPlots)
  # Numbers should be converted to meters if the unit in ft_mElevation starts with f.
  
  feet_designators <- grep("^f|F", unique(plots_merged$ft_mElevation), value = TRUE)
  
  plots_merged <- plots_merged %>% 
    mutate(
      Elevation = case_when(
        ft_mElevation %in% feet_designators ~ Elevation * 0.3048,
        TRUE ~ Elevation
      )
    )
  
  # ErrorMeasurement and ErrorUnits in  RAPlots
  # If measurements are not in meters, needs to be converted
  feet_designators <- grep("^f|F", unique(plots_merged$ErrorUnits), value = TRUE)
  # need to figure out what to do with the values PDOP, pdop and Laptop
  plots_merged <- plots_merged %>% 
    mutate(
      ErrorMeasurement = case_when(
        ErrorUnits %in% feet_designators ~ ErrorMeasurement * 0.3048,
        TRUE ~ ErrorMeasurement
      )
    )
  
  return(plots_merged)
  
  
}

# normalize CRS and convert to lat/lon
normalize_coordinates <- function(plots_merged){
  
  ### author_datum (PlotObservations) ###
  # GPS_datum (RAPlots)
  # Inconsistent Formatting. Does VegBank need these changed?
  # May delete this, if unnecessary
  # Also, what is the correct format? "NAD83"?
  plots_merged <- plots_merged %>% 
    mutate(
      author_datum = case_when(
        GPS_datum %in% c("Nad83", "NAD 83", "NAD83", "nad83") ~ "NAD83",
        GPS_datum %in% c("WGS84", "WGS 84") ~ "WGS84",
        GPS_datum %in% c("NAD 27") ~ "NAD27",
        TRUE ~ NA_character_
      )
    ) %>% 
    mutate(
      crs = case_when(
        author_datum == "WGS84" & UTM_zone == 10 ~ 32610, # create crs column of EPSG codes from datum and UTM zone
        author_datum == "WGS84" & UTM_zone == 11 ~ 32611,
        author_datum == "NAD83" & UTM_zone == 10 ~ 26910,
        author_datum == "NAD83" & UTM_zone == 11 ~ 26911,
        author_datum == "NAD27" & UTM_zone == 10 ~ 26710,
        author_datum == "NAD27" & UTM_zone == 11 ~ 26711,
        # if there are UTME & UTMN and zones, but now datum, we are going to assume that it is 'NAD83'
        # the difference between NAD83 and WGS84 is 1-2 meters, so hopefully irrelevant
        is.na(author_datum) & UTM_zone == 10 ~ 26910,
        is.na(author_datum) & UTM_zone == 11 ~ 26911,
        TRUE ~ NA_real_
      )
    ) 
  
  # use UTME_final and UTMN_final in RAPlots.csv
  # Convert UTM to lat long
  # Three reference systems: NAD83, NAD27, WGS83; Two zones: 10 and 11
  # First split by zone, convert to lat/long, then merge them back together
  # It keeps removing NA values by default and I am trying to keep them
  # New Version (To not affect row numbers)
  
  plots_UTM <- plots_merged %>%
    mutate(.row_id = row_number()) %>% # adding row id
    mutate(UTME_final = case_when( # if UTME_final is NA, use UTME column
      is.na(UTME_final) ~ UTME,
      TRUE ~ UTME_final)) %>% 
    mutate(UTMN_final = case_when( # if UTMN_final is NA, use UTMN column
      is.na(UTMN_final) ~ UTMN,
      TRUE ~ UTMN_final
    )) 
  
  df_coords <- plots_UTM %>%
    filter(!is.na(UTME_final),
           !is.na(UTMN_final),
           !is.na(crs)) %>%
    group_split(crs) %>%
    map_dfr(convert_to_ll)
  
  plots_merged <- plots_UTM %>%
    left_join(df_coords, by = ".row_id") 
  return(plots_merged)
}

assign_state_county <- function(plots_merged){
  # getting state boundaries
  suppressMessages(states_sf <- tigris::states(progress_bar = FALSE) %>% 
                     st_transform(4326))
  
  # getting courty boundaries
  suppressMessages(counties_sf <- tigris::counties(progress_bar = FALSE) %>% 
                     st_transform(4326))
  
  state_county_points <- plots_merged %>% 
    select(".row_id", "real_longitude", "real_latitude") %>% 
    drop_na() %>% 
    st_as_sf(coords = c("real_longitude", "real_latitude"),
             crs = 4326, remove = FALSE) %>% 
    st_join(states_sf, join = st_intersects) %>% 
    rename(stateProvince = NAME) %>%
    st_join(counties_sf, join = st_intersects) %>%
    rename(county = NAME) %>%
    select(".row_id", "stateProvince", "county") %>% 
    st_drop_geometry()
  
  # Add back to data by row id
  plots_merged <- plots_merged %>% 
    left_join(state_county_points, by = ".row_id")
  
  missing_county <- plots_merged[is.na(plots_merged$county), ] # what to do with areas that could be in multiple states?
  
  if (nrow(missing_county) > 0){
    disp <- missing_county %>%
      select(Location_name, real_latitude, real_longitude, UTMN_final, UTM_zone) %>% 
      distinct()
    
    cli::cli_alert_warning("There are {nrow(missing_county)} rows with missing location information.")
  }
  
  plots_merged <- plots_merged %>%
    mutate(
      county = case_when(
        is.na(county) &
          str_detect(
            Location_name,
            "Quail Hollow Quarry Conservation Areas|Schwann Lake, Twin Lakes"
          ) ~ "Santa Cruz",
        TRUE ~ county
      ),
      stateProvince = case_when(
        is.na(stateProvince) &
          str_detect(
            Location_name,
            "Quail Hollow Quarry Conservation Areas|Schwann Lake, Twin Lakes|Carrizo|Los Angeles Department of Water and Power - Fish Slough|BLM - Fish Slough|Fish Slough Ecological Reserve|Pickel Meadows|Pickel Meadow|Millie Lake|anta Clara River"
          ) ~ "California",
        TRUE ~ stateProvince
      )
    )
  
  plots_merged$country <- "USA"
  plots_merged$continent <- "North America"
  return(plots_merged)
}

# TODO: fix this after feedback from CDFW. need to deal with rows that have % in them
normalize_area_shape <- function(plots_merged){
  
  #!!!PROBLEM!!!
  # Using PlotArea and ViewRadius from RAPlots
  
  # Removing units from values (VegBank assumes square meters)
  plots_merged <- plots_merged %>% 
    mutate(
      PlotArea = str_remove(PlotArea, "^~ ?"), # removing leading ~ from '~700' record
      PlotArea = str_remove(PlotArea, " ?(m²|sq\\. ?m|sp\\. ?M|sq ?m|sq\\.? ?M)"),  # removing units
    )
  
  # I know this is ugly, maybe better ways to do it, but we do have to separate out these rows somehow so I just decided to do by index  
  hec <- which(grepl("Hectare|Hectares", plots_merged$PlotArea, ignore.case = TRUE))
  plots_merged$PlotArea[hec] <- gsub("1/2", "0.5", plots_merged$PlotArea[hec])
  plots_merged$PlotArea[hec] <- gsub("Hectare|Hectares", "", plots_merged$PlotArea[hec])
  plots_merged$PlotArea[hec] <- as.character(as.numeric(plots_merged$PlotArea[hec]) * 10000)
  
  # for now, turn the % and Entire Polygon  into NAs
  # TODO: fix this after feedback from CDFW. keeping it out of the main mutate for now
  pct <- which(grepl("%|Entire Polygon|Other \\(please type in\\)", plots_merged$PlotArea, ignore.case = TRUE))
  odd_values <- unique(grep("%|Entire Polygon|Other \\(please type in\\)", plots_merged$PlotArea, ignore.case = TRUE, value = TRUE))
  
  if (length(pct > 0)){
    cli::cli_alert_warning("Some values in the PlotArea column were not able to be converted to numbers. Here is a sample of some of the {length(odd_values)} unique values:")
    cli::cli_text("{odd_values}")
  }
  plots_merged$PlotArea[pct] <- NA
  # end TODO
  
  # -1 indicates plot has no boundaries (no area)
  plots_merged <- plots_merged %>%
    mutate(PlotArea_num = parse_number(as.character(PlotArea), na = c("NA","na","Not recorded","not recorded", "`")), # changing PlotArea to number, combining NAs
           dims = str_extract_all(as.character(SurveyDimensions), "\\d+(?:\\.\\d+)?"),
           SurveyLength = suppressWarnings(as.numeric(map_chr(dims, 1, .default = NA))),
           SurveyWidth  = suppressWarnings(as.numeric(map_chr(dims, 2, .default = NA))),
           area_from_radius = if_else(!is.na(ViewRadius) & is.na(PlotArea_num), pi * (as.numeric(ViewRadius)^2), NA_real_),
           area_from_dims   = if_else(!is.na(SurveyLength) & !is.na(SurveyWidth),
                                      SurveyLength * SurveyWidth, NA_real_),
           PlotArea = coalesce(PlotArea_num, area_from_radius, area_from_dims, -1)
    )
  
  # filling in the columns that are missing or incorrect in the PlotShape column
  plots_merged <- plots_merged %>%
    mutate(
      .tol = pmax(SurveyLength, SurveyWidth, na.rm = TRUE) * 0.02,
      .is_square = !is.na(SurveyLength) & !is.na(SurveyWidth) &
        SurveyLength > 0 & SurveyWidth > 0 &
        abs(SurveyLength - SurveyWidth) <= coalesce(.tol, 0),
      
      PlotShape = case_when(
        !is.na(PlotShape)                     ~ PlotShape,
        !is.na(ViewRadius) & ViewRadius > 0   ~ "circle",
        .is_square                            ~ "square",
        !.is_square & !is.na(SurveyLength) & !is.na(SurveyWidth) &
          SurveyLength > 0 & SurveyWidth > 0  ~ "rectangle",
        TRUE                                  ~ NA_character_
      )
    )
  
  t <- which(!is.na(plots_merged$PlotShape) & plots_merged$PlotArea == -1)
  
  if (length(t > 0)){
    cli::cli_alert_warning("Some rows have a PlotShape value, but no PlotArea. This affects rows with the following SurveyIds ({length(t)} rows): {plots_merged$SurveyID[t]}")
  }
  
  t <- which(is.na(plots_merged$PlotShape) & plots_merged$PlotArea != -1)
  
  if (length(t > 0)){
    cli::cli_alert_warning("Some rows have a PlotArea value, but no PlotShape This affects rows with the following SurveyIds ({length(t)} rows): {plots_merged$SurveyID[t]}")
  }
  
  return(plots_merged)
}

normalize_slope_aspect <- function(plots_merged){
  ### slope_aspect (PlotObservations) ###
  # Aspect_actual (RAPlots) to Aspect_gen (RAPlots)
  # Flat: -1, Variable: -2
  # 0 and 999, not sure yet
  # Aspect_actual remains as is unless Aspect_gen is Flat or Variable
  plots_merged <- plots_merged %>% 
    mutate(
      Aspect_actual = case_when(
        Aspect_gen == "Flat" ~ -1,
        Aspect_gen == "Variable" ~ -2,
        TRUE ~ Aspect_actual
      )
    )
  
  # Note: All flat locations have been changed to -1. This may be sensitive to
  # change. GitHub Issue #4: "Currently, flat locations could be listed as 0,
  # 999, or left blank." If a flat location was listed as 0, 999, or NA 
  # beforehand, it has now been changed to -1.
  
  ### slope_gradient (PlotObservations) ###
  # Slope_actual (RAPlots) to Slope_gen (RAPlots)
  # Code -1 if irregular to determine
  # # If Slope_actual is missing, take the midpoint of Slope_gen
  # If Slope_gen > 25, use value 35
  # assume 999 is a NULL value
  plots_merged <- plots_merged %>%
    mutate(Slope_actual = if_else(Slope_actual == 999, NA, Slope_actual)) %>% 
    mutate(
      .m = str_match(Slope_gen, "\\b(\\d+)\\s*[-–]\\s*(\\d+)\\b"),
      .mid = ((as.numeric(.m[,2]) + as.numeric(.m[,3])) / 2),
      
      slope = coalesce(
        Slope_actual,
        if_else(str_detect(Slope_gen, ">\\s*25"), 35, NA_real_),
        .mid
      )
    ) %>%
    select(-.m, -.mid)
  return(plots_merged)
}

normalize_methods <- function(plots_merged){
  ### methodNarrative (PlotObservations) ###
  # Survey_Type (RAPlots) and AdditionalNotes (AltPlots)
  # AltPlots is empty
  # Capitalizing entries
  plots_merged <- plots_merged %>% 
    mutate(
      methodNarrative = tolower(Survey_Type)
    )
  return(plots_merged)
}

normalize_topo_position <- function(plots_merged){
  ### topo_position (PlotObservations) ###
  # MacroTopo (RAPlots)
  # Convert to corresponding values in LMacroTopo.csv
  
  
  lookup_plot_position <- tibble::tribble(
    ~MacroTopo,                          ~topo_position,
    "Bottom",                               "Basin floor",
    "Lower 1/3 of slope",                   "Lowslope",
    "Bottom to Lower 1/3 of slope",          "Lowslope",
    "Bottom to Mid 1/3 of slope",            "Lowslope",
    "Bottom to Upper 1/3 of slope",          "Midslope",
    "Entire slope",                          "Midslope",
    "Middle 1/3 of slope",                   "Midslope",
    "Middle to Upper 1/3 of slope",          "Midslope",
    "Lower to Middle 1/3 of slope",           "Midslope",
    "Lower to Upper 1/3 of slope",            "Midslope",
    "Upper 1/3 of slope",                    "High slope",
    "Upper 1/3 of slope to Ridgetop",         "High slope",
    "Middle 1/3 of slope to Ridgetop",        "High slope",
    "Lower 1/3 of slope to Ridgetop",         "Midslope",
    "Ridge top",                             "Interfluve",
    "upper",                                "High slope",
    "mid",                                  "Midslope",
    "Bench",                                "Step in slope",
    "Channel bed",                           "Channel bed",
    "Lowslope",                              "Lowslope",
    "6",                                    "Basin floor",
    "Not recorded",                          NA_character_,
    "<Null>",                               NA_character_,
    "Lower 1/3 of slope to Ridegetop",               "Midslope",
    "Other",                                        NA_character_,
    "Wash (channel bed)",                            "Channel bed",
    "Toeslope",                                     "Toeslope",
    "Terrace (former shoreline or floodplain)",     "Low level",
    "Ridge Top",                                    "Interfluve",
    "Dune/sandfield",                               "Low level",
    "Draw",                                         "Channel bed",
    "Pediment",                                     "Lowslope",
    "Unknown",                                      NA_character_,
    "Edge of basin/wetland",                        "Basin floor",
    "* choose one *",                               NA_character_,
    "Badland (complex of draws & interfluves)",     "Midslope",
    "High slope",                                   "High slope",
    "Midslope",                                     "Midslope",
    "Low level",                                    "Low level",
    "Step in slope",                                "Step in slope",
    "Toe-slope",                                    "Toeslope",
    "Saddle/Summit/Ridgeline",                      "Interfluve",
    "Mid-slope",                                    "Midslope",
    "Braided/flood plain",                          "Low level",
    "Rolling Hills/Ridges",                         "Interfluve",
    "Broad wash",                                   "Channel bed",
    "Bar/swale",                                    "Low level",
    "Bajada",                                       "Toeslope",
    "Pavements",                                    "High level",
    "Valley",                                       "Low level",
    "Playa",                                        "Basin floor",
    "Narrow wash",                                  "Channel bed",
    "Deeply dissected hills",                       "Midslope",
    "Mud hills/Badlands",                           "Midslope",
    "Entire Slope",                                 "Midslope",
    "Basin/Wetland",                                "Basin floor",
    "Eroded Hills",                                 "Midslope",
    "Flat",                                         "Low level",
    "Toeslope (alluvial fan/bajada)",               "Toeslope",
    "High level/summit to HighSlope",               "High slope",
    "Basin/wetland",                                "Basin floor",
    "Low- to midslope",                             "Midslope",
    "High level/summit",                            "High level",
    "Lower",                                        "Lowslope",
    "Mid",                                          "Midslope",
    "Upper",                                        "High slope",
    "Top",                                          "Interfluve",
    "High level",                                   "High level",
    "Basin floor",                                  "Basin floor",
    "Channel wall",                                 "Channel wall",
    "Interfluve/Summit",                            "Interfluve",
    "bottom",                                       "Low level"
  )
  
  
  plots_merged <- plots_merged %>% 
    left_join(lookup_plot_position, by = "MacroTopo")
  
  
  diff <- setdiff(
    unique(na.omit(plots_merged$MacroTopo)),
    lookup_plot_position$MacroTopo
  )
  
  if (length(diff) > 0){
    cli::cli_alert_warning("Some values in the MacroTopo field are not present in the vegbank lookup table: {diff}")
  }
  return(plots_merged)
}

calc_percent_rock <- function(plots_merged){
  ### percentOther (PlotObservations) ###
  # Boulders/Stones/Cobbles/Gravels (RAPlots) - percentRockGravel (plots)
  # Boulders/Stones/Cobbles (RAPlots) - percentOther (PlotObservations)
  # Gravels (RAPlots) - percentRockGravel (PlotObservations)
  # Need to combine 4 columns into one
  # CDFW unsure if we want Boulders/Stones/Cobbles with percentOther or
  # percentRockGravel
  # After exploratory tables, combine into percentRockGravel
  plots_merged <- plots_merged %>% 
    mutate(across(c(Boulders, Stones, Cobbles, Gravels), ~ na_if(.x, 999))) %>% 
    mutate(
      percentRockGravel = rowSums(cbind(Boulders, Stones, Cobbles, Gravels), na.rm = TRUE)
    )
  
  if (max(plots_merged$percentRockGravel, na.rm = T) > 100){
    cli::cli_alert_warning("Some percent rock gravel values are greater than 100. Check input data.")
  }
  return(plots_merged)
  
}

calc_tree_cover <- function(plots_merged){
  # Conif_cover/Hdwd_cover/RegenTree_cover (RAPlots) - treeCover (plots)
  # Need to combine 3 columns into one
  plots_merged <- plots_merged %>% 
    mutate(across(c(Hdwd_cover, Conif_cover, RegenTree_cover), ~ na_if(.x, 999))) %>% 
    mutate(
      treeCover = rowSums(cbind(Hdwd_cover, Conif_cover, RegenTree_cover), na.rm = TRUE)
    )
  
  if (max(plots_merged$treeCover, na.rm = T) > 100){
    cli::cli_alert_warning("Some tree cover values ({length(which(plots_merged$treeCover > 100))} rows) are greater than 100. Check input data.")
  }
  return(plots_merged)
}

calc_conif_height <- function(plots_merged){
  ### treeHt (PlotObservations) ###_rat
  # Conif_ht2 and Hdwd_ht2 (RAPlots)
  # First, I need to convert Conif_ht2 and Hdwd_ht2 to numeric by taking the 
  # midpoint
  
  # Conif_ht2
  plots_merged <- plots_merged %>% 
    mutate(
      Conif_ht22 = case_when(
        
        # Midpoint Measurements
        Conif_ht2 == "5-10 m" ~ 7.5,
        Conif_ht2 == "0.5-1 m" ~ 0.75,
        Conif_ht2 == "10-15 m" ~ 12.5,
        Conif_ht2 == "2-5 m" ~ 3.5,
        Conif_ht2 == "20-35m" ~ 27.5,
        Conif_ht2 == "15-20 m" ~ 17.5,
        Conif_ht2 == "35-50 m" ~ 42.5,
        Conif_ht2 == "20-35 m" ~ 27.5,
        Conif_ht2 == "5-10m" ~ 7.5,
        Conif_ht2 == "10-15m" ~ 12.5,
        Conif_ht2 == "15-20m" ~ 17.5,
        Conif_ht2 == "35-50m" ~ 42.5,
        Conif_ht2 == "2-5m" ~ 3.5,
        Conif_ht2 == ".5-1m" ~ 0.75,
        Conif_ht2 == "1-2 m" ~ 1.5,
        Conif_ht2 == "1-2m" ~ 1.5,
        
        # Out of Range Measurements (Must Adjust! These are placeholders)
        Conif_ht2 == "<0.5 m" ~ 0.25,
        Conif_ht2 == "<.5m" ~ 0.25,
        Conif_ht2 == ">50 m" ~ 55,
        Conif_ht2 == ">50m" ~ 55,
        
        # Miscellaneous
        Conif_ht2 == "0" ~ 0,
        
        # Missing Values
        Conif_ht2 == "N/A" ~ NA,
        Conif_ht2 == "Not recorded" ~ NA,
        Conif_ht2 == "Not present" ~ NA,
        Conif_ht2 == "<Null>" ~ NA,
        
        TRUE ~ NA_real_
      )
    )
  
  # make sure all values are accounted for
  na_new <- which(is.na(plots_merged$Conif_ht22) & !(plots_merged$Conif_ht2 %in% c("<Null>", "N/A", "Not recorded", "Not present", NA)))
  
  if (length(na_new) > 0){
    cli::cli_alert_warning("Some Conif_ht2 values were not able to be converted to numeric ({length(na_new)} rows). Check input data.")
  }
  return(plots_merged)
  
}

calc_hdwd_height <- function(plots_merged){
  # Hdwd_ht2
  plots_merged <- plots_merged %>% 
    mutate(
      Hdwd_ht22 = case_when(
        
        # Midpoint Measurements
        Hdwd_ht2 == "2-5 m" ~ 3.5,
        Hdwd_ht2 == "1-2 m" ~ 1.5,
        Hdwd_ht2 == "1-2m" ~ 1.5,
        Hdwd_ht2 == "0.5-1 m" ~ 0.75,
        Hdwd_ht2 == ".5-1m" ~ 0.75,
        Hdwd_ht2 == "15-20 m" ~ 17.5,
        Hdwd_ht2 == "5-10m" ~ 7.5,
        Hdwd_ht2 == "10-15m" ~ 12.5,
        Hdwd_ht2 == "2-5m" ~ 3.5,
        Hdwd_ht2 == "20-35m" ~ 27.5,
        Hdwd_ht2 == "15-20m" ~ 17.5,
        Hdwd_ht2 == "35-50m" ~ 42.5,
        
        # Out of Range Measurements (Must Adjust! These are placeholders)
        Hdwd_ht2 == "<.5m" ~ 0.25,
        
        # Miscellaneous
        Hdwd_ht2 == "0" ~ 0,
        
        # Missing Values
        Hdwd_ht2 == "N/A" ~ NA,
        Hdwd_ht2 == "Not recorded" ~ NA,
        Hdwd_ht2 == "Not present" ~ NA,
        Hdwd_ht2 == "<Null>" ~ NA,
        
        TRUE ~ NA_real_
      )
    )
  
  # make sure all values are accounted for
  na_new <- which(is.na(plots_merged$Hdwd_ht22) & !(plots_merged$Hdwd_ht2 %in% c("<Null>", "N/A", "Not recorded", "Not present", NA)))
  
  if (length(na_new) > 0){
    cli::cli_alert_warning("Some Hdwd_ht2 values were not able to be converted to numeric ({length(na_new)} rows). Check input data.")
  }
  return(plots_merged)
  
}

# TODO: check with CDFW on this modification to cover
assign_tree_height <- function(plots_merged){
  ### treeHt (PlotObservations) ###
  # can take max of hardwood and conifer
  plots_merged <- plots_merged %>%
    mutate(across(contains("_cover"), ~ if_else(.x < 1, .x * 100, .x))) %>%
    mutate(
      treeHt = pmax(Conif_ht22, Hdwd_ht22, na.rm = TRUE),
      # case for both values being NA
      treeHt = if_else(is.infinite(treeHt), NA_real_, treeHt)
    )
  
  return(plots_merged)
}

assign_growth_form <- function(plots_merged){
  
  plots_merged <- plots_merged %>%
    mutate(
      conif_ht_mid = case_when(
        Conif_ht2 == "5-10 m"  ~ 7.5,
        Conif_ht2 == "0.5-1 m" ~ 0.75,
        Conif_ht2 == "10-15 m" ~ 12.5,
        Conif_ht2 == "2-5 m"   ~ 3.5,
        Conif_ht2 %in% c("20-35m","20-35 m") ~ 27.5,
        Conif_ht2 %in% c("15-20 m","15-20m") ~ 17.5,
        Conif_ht2 %in% c("35-50 m","35-50m") ~ 42.5,
        Conif_ht2 %in% c("5-10m")            ~ 7.5,
        Conif_ht2 %in% c("10-15m")           ~ 12.5,
        Conif_ht2 %in% c("2-5m")             ~ 3.5,
        Conif_ht2 %in% c(".5-1m")            ~ 0.75,
        Conif_ht2 == "1-2 m"                 ~ 1.5,
        Conif_ht2 == "<0.5 m"                ~ 0.25,
        Conif_ht2 %in% c(">50 m",">50m")     ~ 55,
        Conif_ht2 == "0"                     ~ 0,
        Conif_ht2 %in% c("N/A","Not recorded","Not present") ~ NA_real_,
        TRUE ~ NA_real_
      ),
      hdwd_ht_mid = case_when(
        Hdwd_ht2 %in% c("2-5 m","2-5m")   ~ 3.5,
        Hdwd_ht2 == "1-2 m"               ~ 1.5,
        Hdwd_ht2 == "0.5-1 m"             ~ 0.75,
        Hdwd_ht2 %in% c("15-20 m","15-20m") ~ 17.5,
        Hdwd_ht2 %in% c("5-10m")          ~ 7.5,
        Hdwd_ht2 %in% c("10-15m")         ~ 12.5,
        Hdwd_ht2 %in% c("20-35m")         ~ 27.5,
        Hdwd_ht2 %in% c("35-50m")         ~ 42.5,
        Hdwd_ht2 == "<.5m"                ~ 0.25,
        Hdwd_ht2 == "0"                   ~ 0,
        Hdwd_ht2 %in% c("N/A","Not recorded","Not present") ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
  
  plots_merged <- plots_merged %>%
    mutate(
      # which growthform is most common
      gf1_is_conif = case_when(
        is.na(Conif_cover) & is.na(Hdwd_cover) ~ NA,
        !is.na(Conif_cover) &  is.na(Hdwd_cover) ~ TRUE,
        is.na(Conif_cover)  & !is.na(Hdwd_cover) ~ FALSE,
        Conif_cover > Hdwd_cover ~ TRUE,
        Hdwd_cover  > Conif_cover ~ FALSE,
        Conif_cover == Hdwd_cover ~ TRUE,   # tie default -> Conifer first
        TRUE ~ NA
      ),
      
      growthform1Type  = case_when(
        is.na(gf1_is_conif) ~ NA_character_,
        gf1_is_conif        ~ "Conifer Tree",
        !gf1_is_conif       ~ "Hardwood Tree"
      ),
      growthform2Type  = case_when(
        is.na(gf1_is_conif) ~ NA_character_,
        !is.na(Conif_cover) & is.na(Hdwd_cover) ~ NA_character_,
        is.na(Conif_cover) & !is.na(Hdwd_cover) ~ NA_character_,
        # if tied, no clear second (can change this)
        !is.na(Conif_cover) & !is.na(Hdwd_cover) & Conif_cover == Hdwd_cover ~ NA_character_,
        gf1_is_conif  ~ "Hardwood Tree",
        TRUE          ~ "Conifer Tree"
      ),
      
      growthform1Cover = case_when(
        is.na(gf1_is_conif) ~ NA_real_,
        gf1_is_conif        ~ conif_ht_mid,
        TRUE                ~ hdwd_ht_mid
      ),
      growthform2Cover = case_when(
        is.na(gf1_is_conif) ~ NA_real_,
        !is.na(Conif_cover) & is.na(Hdwd_cover) ~ NA_real_,
        is.na(Conif_cover) & !is.na(Hdwd_cover) ~ NA_real_,
        !is.na(Conif_cover) & !is.na(Hdwd_cover) & Conif_cover == Hdwd_cover ~ NA_real_,
        gf1_is_conif  ~ hdwd_ht_mid,
        TRUE          ~ conif_ht_mid
      )
    ) %>%
    select(-gf1_is_conif)
  
  return(plots_merged)
}

check_existing_plots <- function(plots_merged, vb_url = "https://api-dev.vegbank.org", renew_cache = FALSE){
  
  cache_dir  <- rappdirs::user_cache_dir("vegbank")
  cache_file <- file.path(cache_dir, "pl_all.csv")
  
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  obj <- if (file.exists(cache_file) & !renew_cache) {
    pl_all <- read_csv(cache_file, progress = FALSE, show_col_types = FALSE, guess_max = 20000)
  } else {
    cli::cli_alert_info("Downloading vb plots data.")
    ### user_pl_code (PlotObservations) ###
    # For now, there is no matches so user_pl_code will remain empty
    vb_set_base_url(vb_url)
    
    # Adaptive, resumable pager for VegBank plot observations
    
    page_init  <- 5000  # shrink this if there is an error
    page_min   <- 500   # don't go smaller than this
    max_pages  <- 500   # hard stop
    sleep_sec  <- 0.05  # brief pause to avoid error
    keep_cols  <- c("pl_code","latitude","longitude","ob_code","author_plot_code", "author_obs_code", "state_province","country")
    checkpoint <- "pl_all_checkpoint.rds" # just in case something fails
    save_every <- 10
    
    out <- list()
    seen_codes <- character(0)
    limit <- page_init
    
    # column types
    text_cols <- c("ob_code","state_province","country", "author_obs_code")
    num_cols  <- c("latitude","longitude")
    
    for (i in seq_len(max_pages)) {
      offset <- (i - 1L) * limit
      # try once
      # on failure (e.g., 504), halve the limit and retry
      chunk <- tryCatch(
        vb_get_plot_observations(limit = limit, offset = offset),
        error = function(e) {
          limit <<- max(page_min, floor(limit/2))
          tryCatch(get_all_plot_observations(limit = limit, offset = offset),
                   error = function(e2) { NULL })
        }
      )
      if (is.null(chunk) || !nrow(chunk)) { break }
      
      keep <- intersect(keep_cols, names(chunk))
      if (length(keep)) chunk <- chunk[, keep, drop = FALSE]
      
      # normalize
      chunk <- chunk %>%
        mutate(
          across(any_of(text_cols), as.character),
          across(any_of(num_cols),  as.numeric)
        )
      
      if ("pl_code" %in% names(chunk)) {
        new <- !chunk$pl_code %in% seen_codes
        if (!any(new)) { break }
        seen_codes <- c(seen_codes, chunk$pl_code[new])
        chunk <- chunk[new, , drop = FALSE]
      }
      
      out[[length(out) + 1L]] <- chunk
      total <- sum(vapply(out, nrow, integer(1)))
      
      if (nrow(chunk) < limit) { break }
      
      if (save_every > 0 && (i %% save_every == 0)) {
        #Normalize again
        out_fixed <- map(out, ~ .x %>%
                           mutate(
                             across(any_of(text_cols), as.character),
                             across(any_of(num_cols),  as.numeric)
                           ))
        tmp <- bind_rows(out_fixed) %>% distinct()
        saveRDS(tmp, checkpoint)
      }
      
      if (sleep_sec > 0) Sys.sleep(sleep_sec) }
    
    # normalize again
    out_fixed <- map(out, ~ .x %>%
                       mutate(
                         across(any_of(text_cols), as.character),
                         across(any_of(num_cols),  as.numeric)
                       ))
    
    pl_all <- bind_rows(out_fixed) %>% distinct()
    write_csv(pl_all, cache_file)
  }
  
  plots_merged_check <- plots_merged %>%
    left_join(
      pl_all %>% select(author_plot_code, pl_code),
      by = c("SurveyID" = "author_plot_code")
    )
  
  if (any(plots_merged_check$SurveyID %in% pl_all$author_plot_code)){
    cli::cli_alert_warning("Some SurveyId values already exist in the vegbank database. Is this expected?")
  }
  
  return(NULL)
  
}

# helper function for range to midpoint
range_to_midpoint <- function(x,
                              less_rule = c("half", "keep", "na"),
                              greater_rule = c("keep", "na"),
                              treat_zero_as_na = FALSE) {
  less_rule    <- match.arg(less_rule)
  greater_rule <- match.arg(greater_rule)
  
  x0 <- as.character(x) %>% str_squish() %>% str_to_lower()
  
  # standard missing values
  x0[x0 %in% c("", "<null>", "n/a", "not recorded", "not present", "na")] <- NA_character_
  
  # normalize dash types
  x0 <- str_replace_all(x0, "–", "-")
  
  # convert fractions
  # "1/2" -> "0.5"
  x0 <- str_replace_all(x0, "1\\s*/\\s*2", "0.5")
  
  # handle .5 (convert to 0.5)
  x0 <- str_replace_all(x0, "^.5-1", "0.5-1")
  
  # remove units/spaces/words
  x_clean <- str_replace_all(x0, "[^0-9.<>-]+", "")
  
  out <- rep(NA_real_, length(x_clean))
  
  # ranges : "0.5-1" or "2-5"
  is_range <- !is.na(x_clean) & str_detect(x_clean, "^[0-9.]+-[0-9.]+$")
  if (any(is_range)) {
    m <- str_match(x_clean[is_range], "^([0-9.]+)-([0-9.]+)$")
    lo <- as.numeric(m[, 2])
    hi <- as.numeric(m[, 3])
    out[is_range] <- (lo + hi) / 2
  }
  
  # less-than : "<0.5"
  is_less <- !is.na(x_clean) & str_detect(x_clean, "^<[0-9.]+$")
  if (any(is_less)) {
    v <- as.numeric(str_remove(x_clean[is_less], "^<"))
    out[is_less] <- dplyr::case_when(
      less_rule == "half" ~ v / 2,
      less_rule == "keep" ~ v,
      TRUE ~ NA_real_
    )
  }
  
  # greater-than : ">50"
  is_greater <- !is.na(x_clean) & str_detect(x_clean, "^>[0-9.]+$")
  if (any(is_greater)) {
    v <- as.numeric(str_remove(x_clean[is_greater], "^>"))
    out[is_greater] <- if (greater_rule == "keep") v else NA_real_
  }
  
  # plain numbers like "0" or "7.5"
  is_num <- !is.na(x_clean) & str_detect(x_clean, "^[0-9.]+$")
  if (any(is_num)) {
    out[is_num] <- as.numeric(x_clean[is_num])
  }
  
  if (treat_zero_as_na) out[out == 0] <- NA_real_
  
  out
}

# calculate shrub height
calc_shrub_height <- function(plots_merged){
  
  mv_list <- c("", "<null>", "n/a", "not recorded", "not present", "na", 'N/A', 'Not recorded', 'Not present', '<Null>')
  
  plots_merged <- plots_merged %>%
    mutate(
      Shrub_ht22 = range_to_midpoint(Shrub_ht2, less_rule = "half", greater_rule = "keep")
    ) %>% 
    mutate(Shrub_ht2 = if_else(Shrub_ht2 %in% mv_list, NA, Shrub_ht2))
  
  nas <- which(is.na(plots_merged$Shrub_ht22) & !is.na(plots_merged$Shrub_ht2))
  
  if (length(nas) > 0){
    cli::cli_alert_warning("Some shrub heights were unable to be converted to numeric: {unique(plots_merged$Shrub_ht2[nas])}")
  }
  
  return(plots_merged)
}

# calculate field height
calc_herb_height <- function(plots_merged){
  
  mv_list <- c("", "<null>", "n/a", "not recorded", "not present", "na", 'N/A', 'Not recorded', 'Not present', '<Null>')
  
  plots_merged <- plots_merged %>%
    mutate(
      Herb_ht22 = range_to_midpoint(Herb_ht2, less_rule = "half", greater_rule = "keep")
    ) %>% 
    mutate(Herb_ht2 = if_else(Herb_ht2 %in% mv_list, NA, Herb_ht2))
  
  # TODO: fix this one
  nas <- which(is.na(plots_merged$Herb_ht22) & !is.na(plots_merged$Herb_ht2))
  
  if (length(nas) > 0){
    cli::cli_alert_warning("Some herb heights were unable to be converted to numeric: {unique(plots_merged$Herb_ht2[nas])}")
  }
  
  return(plots_merged)
}

plots_loader <- function(in_dir, out_dir){
  
  # read in all the files and join to one table
  plots_merged <- load_files(in_dir)
  plots_merged <- normalize_elevation(plots_merged)
  plots_merged <- normalize_coordinates(plots_merged)
  plots_merged <- assign_state_county(plots_merged)
  plots_merged <- normalize_area_shape(plots_merged)
  plots_merged <- normalize_slope_aspect(plots_merged)
  plots_merged <- normalize_methods(plots_merged)
  plots_merged <- normalize_topo_position(plots_merged)
  plots_merged <- calc_percent_rock(plots_merged)
  plots_merged <- calc_tree_cover(plots_merged)
  plots_merged <- calc_conif_height(plots_merged)
  plots_merged <- calc_hdwd_height(plots_merged)
  plots_merged <- assign_tree_height(plots_merged)
  plots_merged <- assign_growth_form(plots_merged)
  plots_merged <- calc_shrub_height(plots_merged)
  plots_merged <- calc_herb_height(plots_merged)
  check_existing_plots(plots_merged, vb_url = "https://api-dev.vegbank.org", renew_cache = FALSE)



  # previously this only assigned user_ob_code to the SurveyId if it existed in the classification table
  plots_merged <- plots_merged %>% 
    mutate(user_ob_code = SurveyID)
  
  # Time should be removed
  plots_merged <- plots_merged %>% 
    mutate(SurveyDate = as_date(ymd(SurveyDate)))
  
  
  plots_template_fields <- build_loader_table(
    sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
    sheet = "PlotObservations",
    source_df = plots_merged
  )
  
  plots_LT <- plots_template_fields$template
  
  # fix column names
  # user ob code?
  plots_LT$user_obs_code <- plots_merged$SurveyID
  plots_LT$author_obs_code <- plots_merged$SurveyID
  plots_LT$user_plot_code <- plots_merged$Stand_ID
  plots_LT$author_plot_code <- plots_merged$Stand_ID
  plots_LT$real_latitude <- plots_merged$real_latitude
  plots_LT$real_longitude <- plots_merged$real_longitude
  plots_LT$location_accuracy <- plots_merged$ErrorMeasurement
  plots_LT$confidentiality_status <- plots_merged$ConfidentialityStatus
  plots_LT$author_e <- plots_merged$UTME_final
  plots_LT$author_n <- plots_merged$UTMN_final
  plots_LT$author_zone <- plots_merged$UTM_zone
  plots_LT$author_datum <- plots_merged$author_datum
  plots_LT$author_location <- plots_merged$SiteLocation
  plots_LT$azimuth <- plots_merged$W_Axis_Bearing
  plots_LT$shape <- plots_merged$PlotShape
  plots_LT$area <- plots_merged$PlotArea
  plots_LT$stand_size <- plots_merged$Stand_Size
  plots_LT$elevation <- plots_merged$Elevation
  plots_LT$slope_aspect <- plots_merged$Aspect_actual
  plots_LT$slope_gradient <- plots_merged$slope
  plots_LT$topo_position <- plots_merged$topo_position
  plots_LT$rock_type <- plots_merged$Substrate
  plots_LT$pj_code <- plots_merged$ProjectCode
  plots_LT$obs_start_date <- plots_merged$SurveyDate
  plots_LT$method_narrative <- plots_merged$methodNarrative
  plots_LT$successional_status = plots_merged$Trend
  plots_LT$basal_area <- plots_merged$BasalStem
  plots_LT$hydrologic_regime <- plots_merged$Upl_Wet_text
  plots_LT$percent_litter <- plots_merged$Litter
  plots_LT$percent_bare_soil <- plots_merged$Bare_fines
  plots_LT$percent_qater <- plots_merged$Water
  plots_LT$tree_ht <- plots_merged$treeHt
  plots_LT$shrub_ht <- plots_merged$Shrub_ht22
  plots_LT$field_ht <- plots_merged$Herb_ht22 
  plots_LT$tree_cover <- plots_merged$treeCover
  plots_LT$shrub_cover <- plots_merged$Shrub_cover
  plots_LT$field_cover <- plots_merged$Herb_cover
  plots_LT$nonvascular_cover <- plots_merged$NonVasc_Veg_cover
  plots_LT$dominant_stratum <- plots_merged$DomForm
  plots_LT$growthform_1_cover <- plots_merged$growthform1Cover
  plots_LT$growthform_2_cover <- plots_merged$growthform2Cover
  plots_LT$growthform_1_type <- plots_merged$growthform1Type
  plots_LT$growthform_2_type <- plots_merged$growthform2Type
  
  
  
  # save filled in loader table
  out_path <- file.path(out_dir, "plotsLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(plots_LT, out_path)
}


