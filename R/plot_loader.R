library(tidyverse)
library(sf)
library(tigris)
library(rnaturalearth)
library(vegbankr)
options(tigris_use_cache = TRUE, tigris_class = "sf")


#' Convert a data frame with a CRS (epsg code) and an easting/northing 
#' coordinate ("UTME_final", "UTMN_final") to lat/lon coordinates
#'
#' @param df_group data.frame with `crs`, `UTME_final`, `UTMN_final` columns
#'
#' @return data.frame with a row identifier, latitude, and longitude column
#' 
#' @details
#' This function is designed to work with grouped data (split by CRS) to handle
#' different coordinate reference systems. It converts UTM coordinates to
#' geographic coordinates (WGS84, EPSG:4326) using the sf package.
#' 
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

#' Reads RAPlots.csv, AltPlots.csv, and AltStrata.csv files from VegBank
#' subdirectories and merges them by SurveyID
#' 
#' @param in_dir Directory of VegBank data to read from
#' @param out_dir Directory of data to write to
#' 
#' @return Merged data frame containing all plot data with validated survey dates
#' 
#' @details
#' \itemize{
#'   \item Left joins AltPlots and AltStrata to RAPlots by SurveyID
#'   \item Drops columns that are entirely NA
#'   \item Converts SurveyDate from datetime to date format
#'   \item Validates dates
#' }
# read in files from input directory and join into one table
load_plot_files <- function(in_dir) {
  
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

#' Converts elevation and GPS error measurements to meters 
#' 
#' @param plots_merged Data frame containing plot data with elevation fields
#' 
#' @return Data frame with elevation and error measurements in meters
#' 
#' @details
#' **Conversions**
#' \itemize{
#'   \item Elevation: Converts from feet to meters if `ft_mElevation` starts with 'f' or 'F'
#'   \item ErrorMeasurement: Converts from feet to meters if `ErrorUnits` starts with 'f' or 'F'
#'   \item Conversion factor: 1 foot = 0.3048 meters
#' }
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

#' Standardizes GPS datum values, assigns EPSG codes, and converts UTM coordinates
#' to WGS84 latitude/longitude
#' 
#' @param plots_merged Data frame containing plot data with coordinate fields
#' 
#' @return Data frame with standardized datums, EPSG codes, and latitude/longitude
#'         coordinates
#'         
#' @details
#' **Coordinate Conversion**
#' \itemize{
#'   \item Uses `UTME_final` and `UTMN_final` (falls back to `UTME` and `UTMN` if NA)
#'   \item Splits data by CRS, converts each group separately
#'   \item Merges converted coordinates back by row ID
#'   \item Missing datum defaults to NAD83 (difference from WGS84 is 1-2 meters)
#' }
# normalize CRS and convert to lat/lon
normalize_coordinates <- function(plots_merged){
  
  ### author_datum (PlotObservations) ###
  # GPS_datum (RAPlots)
  # Inconsistent Formatting. Does VegBank need these changed?
  # May delete this, if unnecessary
  # Also, what is the correct format? "NAD83"?
  plots_merged <- plots_merged %>% 
    mutate(UTM_zone = if_else(SurveyID == "CVRP0093 ", 10, UTM_zone)) %>% # fix this one plot
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

#' Determines state and county for each plot by spatially joining coordinates
#' with US state and county boundaries from the TIGRIS package
#' 
#' @param plots_merged Data frame containing plot data with latitude/longitude
#'                     coordinates
#'                     
#' @return Data frame with state, county, country, and continent fields populated
#' 
#' @details
#' **Spatial Matching:**
#' \itemize{
#'   \item Downloads US state and county boundaries via TIGRIS
#'   \item Performs spatial intersection to assign locations
#'   \item Warns about plots with missing location information
#' }
#' **Manual Corrections:**
#' Some known locations with problematic coordinates are manually assigned:
#' \itemize{
#'   \item Quail Hollow Quarry Conservation Areas → Santa Cruz County
#'   \item Fish Slough areas → California
#'   \item Pickel Meadows → California
#' }
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

#' Standardizes plot area measurements to square meters and infers plot shapes 
#' from dimensions when not explicitly recorded
#' 
#' @param plots_merged Data frame containing plot data with area and shape fields
#' 
#' @return Data frame with normalized area (square meters) and shape fields
#' 
#' @details
#' **Area Normalization:**
#' \itemize{
#'   \item Removes unit designators (m², sq m, etc.)
#'   \item Converts hectares to square meters (1 hectare = 10,000 m²)
#'   \item Handles fractional hectares (e.g., "1/2 Hectare" → 5000 m²)
#'   \item Calculates area from ViewRadius if direct measurement unavailable
#'   \item Calculates area from SurveyLength × SurveyWidth if dimensions available
#'   \item Sets area to -1 if plot has no defined boundaries
#'   \item Converts problematic values (%, "Entire Polygon") to NA
#' }
#' **Shape Inference:**
#' When PlotShape is missing, shape is inferred from measurements:
#' \itemize{
#'   \item ViewRadius > 0 → "circle"
#'   \item Length ≈ Width (within 2% tolerance) → "square"
#'   \item Length ≠ Width → "rectangle"
#' }
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
  
  # turn the % and Entire Polygon  into NAs per Rosie 206-03-03
  pct <- which(grepl("%|Entire Polygon|Other \\(please type in\\)", plots_merged$PlotArea, ignore.case = TRUE))
  plots_merged$PlotArea[pct] <- NA
  plots_merged$PlotArea[pct] <- NA
  
  # -1 indicates plot has no boundaries (no area)
  plots_merged <- plots_merged %>%
    mutate(PlotArea_num = parse_number(as.character(PlotArea), na = c("NA","na","Not recorded","not recorded", "`")), # changing PlotArea to number, combining NAs
           dims = str_extract_all(as.character(SurveyDimensions), "\\d+(?:\\.\\d+)?"),
           SurveyLength = suppressWarnings(as.numeric(map_chr(dims, 1, .default = NA))),
           SurveyWidth  = suppressWarnings(as.numeric(map_chr(dims, 2, .default = NA))),
           area_from_dims   = if_else(!is.na(SurveyLength) & !is.na(SurveyWidth),
                                      SurveyLength * SurveyWidth, NA_real_),
           PlotArea = coalesce(PlotArea_num, area_from_dims, NA)
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
        .is_square                            ~ "square",
        !.is_square & !is.na(SurveyLength) & !is.na(SurveyWidth) &
          SurveyLength > 0 & SurveyWidth > 0  ~ "rectangle",
        TRUE                                  ~ NA_character_
      )
    ) %>% 
    # Rapid assessments are dimensionless, and the radius (which is a minimum) is provided only to assist with mapping. Remove plot shape where survey_type = Rapid Assessment, and do not calculate an area based on RARadius.
    mutate(PlotShape = if_else(tolower(Survey_Type) == "rapid assessment", NA, PlotShape)) %>% 
    mutate(PlotArea = if_else(tolower(Survey_Type) == "rapid assessment", NA, PlotArea))
  
  return(plots_merged)
}

#' Standardizes slope aspect codes and calculates numeric slope gradient from
#' categorical or measured values
#' 
#' @param plots_merged Data frame containing plot data with slope fields
#' 
#' @return Data frame with normalized aspect and gradient values
#' 
#' @details
#' **Aspect Codes:**
#' \itemize{
#'   \item Flat locations: -1
#'   \item Variable aspect: -2
#'   \item Otherwise: Uses actual measured aspect (0-360 degrees)
#' }
#' **Gradient Calculation:**
#' Priority order:
#' \enumerate{
#'   \item Slope_actual (measured value, treats 999 as NA)
#'   \item If Slope_gen indicates ">25", uses 35
#'   \item Calculates midpoint from Slope_gen ranges (e.g., "10-15" → 12.5)
#' }
#'
#' @note
#' All flat locations are coded as -1 regardless of whether they were originally
#' recorded as 0, 999, or blank.
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

#' Standardizes survey type descriptions by converting to lowercase
#' 
#' @param plots_merged Data frame containing plot data with Survey_Type field
#' 
#' @return Data frame with lowercase methodNarrative field
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

#' Maps detailed macrotopographic position descriptions to standardized
#' VegBank topographic position categories
#' 
#' @param plots_merged Data frame containing plot data with MacroTopo field
#' 
#' @return Data frame with standardized topo_position field
#' 
#' @details
#' Uses a comprehensive lookup table mapping 70+ position descriptions to
#' standardized categories including:
#' \itemize{
#'   \item Basin floor
#'   \item Lowslope
#'   \item Midslope
#'   \item High slope
#'   \item Interfluve
#'   \item Channel bed
#'   \item Toeslope
#'   \item Low level / High level
#' }
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

#' Sums boulder, stone, cobble, and gravel cover into a single percentage 
#' 
#' @param plots_merged Data frame containing rock/gravel cover fields
#' 
#' @return Data frame with calculated percentRockGravel field
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
    ) %>% 
    # if survey is accuracy assessment make rock gravel NA per CDFW review
    mutate(percentRockGravel = if_else(tolower(Survey_Type) == "accuracy assessment", NA, percentRockGravel))
  
  if (max(plots_merged$percentRockGravel, na.rm = T) > 100){
    cli::cli_alert_warning("Some percent rock gravel values are greater than 100. Check input data.")
  }
  return(plots_merged)
  
}

#' Sums hardwood, conifer, and regenerating tree cover into a single percentage
#' 
#' @param plots_merged Data frame containing tree cover fields
#' 
#' @return Data frame with calculated treeCover field
calc_tree_cover <- function(plots_merged){
  # Conif_cover/Hdwd_cover/RegenTree_cover (RAPlots) - treeCover (plots)
  # Need to combine 3 columns into one
  plots_merged <- plots_merged %>% 
    mutate(across(ends_with("_cover"), ~ na_if(.x, 999))) %>% 
    mutate(
      treeCover = rowSums(cbind(Hdwd_cover, Conif_cover, RegenTree_cover), na.rm = TRUE)
    ) %>% 
    mutate(treeCover = if_else(treeCover > 100, 100, treeCover)) # a few rows where tree cover exceeds 100, turn into 100 per Rosie 2026-03-03
  
  return(plots_merged)
}

#' Converts categorical conifer height ranges to numeric midpoint values
#' 
#' @param plots_merged Data frame containing Conif_ht2 field
#' 
#' @return Data frame with calculated Conif_ht22 numeric field
#' 
#' @details
#' Manually maps all observed conifer height categories to numeric values
calc_conif_height <- function(plots_merged){
  ### treeHt (PlotObservations) ###_rat
  # Conif_ht2 and Hdwd_ht2 (RAPlots)
  # First, I need to convert Conif_ht2 and Hdwd_ht2 to numeric by taking the 
  # midpoint
  
  # Conif_ht2
  plots_merged <- plots_merged %>%
    mutate(
      Conif_ht22 = range_to_midpoint(
        Conif_ht2,
        less_rule = "half",      # "<0.5" -> 0.25
        greater_rule = "keep"    # ">50" -> 50
      )
    )
  
  x0 <- plots_merged$Conif_ht2 %>% as.character() %>% stringr::str_squish() %>% stringr::str_to_lower()
  x0[x0 %in% c("", "<null>", "n/a", "not recorded", "not present", "na")] <- NA_character_
  
  bad <- which(is.na(plots_merged$Conif_ht22) & !is.na(x0))
  if (length(bad) > 0) {
    cli::cli_alert_warning(
      "Some Conif_ht2 values could not be converted ({length(bad)} rows): {paste(unique(plots_merged$Conif_ht2[bad]), collapse = ', ')}"
    )
  }
  
  return(plots_merged)
  
}

#' Converts categorical hardwood height ranges to numeric midpoint values
#' 
#' @param plots_merged Data frame containing Hdwd_ht2 field
#' 
#' @return Data frame containing Hdwd_ht22 field
#' 
#' @details
#' Manually maps all observed hardwood height categories to numeric values
calc_hdwd_height <- function(plots_merged){
  # Hdwd_ht2
  plots_merged <- plots_merged %>%
    mutate(
      Hdwd_ht22 = range_to_midpoint(
        Hdwd_ht2,
        less_rule = "half",      # "<0.5" -> 0.25
        greater_rule = "keep"    # ">50" -> 50 (if it appears)
      )
    )
  
  # Warn on any non-missing values that failed conversion
  x0 <- plots_merged$Hdwd_ht2 %>% as.character() %>% stringr::str_squish() %>% stringr::str_to_lower()
  x0[x0 %in% c("", "<null>", "n/a", "not recorded", "not present", "na")] <- NA_character_
  
  bad <- which(is.na(plots_merged$Hdwd_ht22) & !is.na(x0))
  if (length(bad) > 0) {
    cli::cli_alert_warning(
      "Some Hdwd_ht2 values could not be converted ({length(bad)} rows): {paste(unique(plots_merged$Hdwd_ht2[bad]), collapse = ', ')}"
    )
  }
  
  return(plots_merged)
}

#' Determines the maximum tree height from hardwood and conifer measurements.
#' Ensures cover percentages are scaled to 0-100 range
#' 
#' @param plots_merged Data frame containing tree height and cover fields
#' 
#' @return Data frame with treeHt field (maximum of conifer and hardwood heights)
#' 
#' @details
#' \itemize{
#'   \item Scales cover values: if <1, multiplies by 100 to convert from proportion
#'   \item Takes maximum of Conif_ht22 and Hdwd_ht22
#'   \item Handles case where both are NA (sets to NA instead of Inf)
#' }
# TODO: check with CDFW on this modification to cover
assign_tree_height <- function(plots_merged){
  ### treeHt (PlotObservations) ###
  # can take max of hardwood and conifer
  plots_merged <- plots_merged %>%
    mutate(
      treeHt = pmax(Conif_ht22, Hdwd_ht22, na.rm = TRUE),
      # case for both values being NA
      treeHt = if_else(is.infinite(treeHt), NA_real_, treeHt)
    )
  
  return(plots_merged)
}

#' Queries VegBank API to check if any plot codes (SurveyIDs) already exist in
#' the database
#' 
#' @param plots_merged Data frame containing SurveyID field
#' @param renew_cache Logical. If TRUE, re-downloads data from API. If FALSE,
#' uses cached data if available
#' 
#' @return None
#' 
#' @details
#' **Adaptive Paging:**
#' \itemize{
#'   \item Starts with 5000 records per page
#'   \item Reduces page size on errors (minimum 500)
#'   \item Saves checkpoints every 10 pages
#'   \item Deduplicates records across pages
#' }
check_existing_plots <- function(plots_merged, renew_cache = FALSE){
  
  cache_dir  <- rappdirs::user_cache_dir("vegbank")
  cache_file <- file.path(cache_dir, "plots_all.csv")
  
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  obj <- if (file.exists(cache_file) & !renew_cache) {
    pl_all <- read_csv(cache_file, progress = FALSE, show_col_types = FALSE, guess_max = 20000)
  } else {
    cli::cli_alert_info("Downloading vb plots data.")
    ### user_pl_code (PlotObservations) ###
    # For now, there is no matches so user_pl_code will remain empty
    
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

#' Helper function to convert height ranges to numeric midpoint values, with
#' special handling for less-than and greater-than values
#' 
#' @param x Character vector of height values
#' @param less_rule How to handle less-than values
#' @param greater_rule How to handle greater-than values
#' @param treat_zero_as_na Logical. If TRUE, converts 0 values to NA
#' 
#' @return Numeric vector of converted height values
#' 
#' @details
#' **Conversions:**
#' \itemize{
#'   \item Ranges: "5-10 m" → 7.5
#'   \item Less-than: "<0.5 m" → 0.25 (if less_rule = "half")
#'   \item Greater-than: ">50 m" → 50 (if greater_rule = "keep")
#'   \item Fractions: "1/2" → 0.5
#'   \item Plain numbers: "7.5" → 7.5
#'   \item Missing values: "N/A", "Not recorded", "<Null>" → NA
#' }
#' **Preprocessing:**
#' \itemize{
#'   \item Normalizes whitespace and case
#'   \item Converts dash types (em-dash → hyphen)
#'   \item Removes units and text
#' }
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
  
  valid_pattern <- "^[^a-z]"
  is_valid <- is.na(x0) | str_detect(x0, valid_pattern)
  
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
  
  out[!is_valid] <- NA_real_
  
  out
}

#' Converts categorical shrub height ranges to numeric midpoint values using the
#' generic range_to_midpoint helper
#' 
#' @param plots_merged Data frame containing Shrub_ht2 field
#' 
#' @return Data frame with calculated Shrub_ht22 field
#' 
#' @details
#' Uses range_to_midpoint with less_rule = "half" and greater_rule = "keep"
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

#' Converts categorical herbaceous vegetation height ranges to numeric midpoint
#' values using the range_to_midpoint helper
#' 
#' @param plots_merged Data frame containing Herb_ht2 field
#' 
#' @return Data frame with calculated Herb_ht22 field
#' 
#' @details
#' Uses range_to_midpoint with less_rule = "half" and greater_rule = "keep"
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



#' Extract location description from multiple columns
#'
#' @param plots_merged A data frame containing plot data with columns
#'   Site_history, SiteLocation, and AdditionalNotes
#'
#' @return A data frame with an additional author_location column containing
#'   the first non-NA value from Site_history, SiteLocation, or AdditionalNotes
#'   (in that priority order). Invalid SiteLocation values are set to NA.
#'
extract_location_description <- function(plots_merged){
  plots_merged <- plots_merged %>% 
      mutate(SiteLocation = if_else(SiteLocation == "; UTM2 to UTM", NA, SiteLocation)) %>% 
      mutate(author_location = Location_name) %>% 
      mutate(location_narrative = coalesce(Site_history, SiteLocation, AdditionalNotes))
  
}

#' Ensures text fields do not exceed VegBank's maximum character limits
#' 
#' @param plots_merged Data frame containing text fields
#' 
#' @return Data frame with truncated fields
#' 
#' @details
#' **Truncations:**
#' \itemize{
#'   \item SiteLocation: 200 characters
#'   \item DomForm: 40 characters
#' }
# truncate fields that are varchar(n)
truncate_fields <- function(plots_merged){
  plots_merged <- plots_merged %>% 
    mutate(location_narrative = substr(location_narrative, 1, 200)) %>% 
    mutate(DomForm = substr(DomForm, 1, 40))
}


#' Handles duplicate user_pl_code values by assigning unique suffixes when
#' plot-level attributes differ
#' 
#' @param plots_LT Data frame containing plot loader data table
#' 
#' @return Data frame with deduplicated user_pl_code values
#' 
#' @details
#' **Deduplication Logic:**
#' \enumerate{
#'   \item Identifies plots with duplicate user_pl_code
#'   \item Checks if plot-level attributes (coordinates, elevation, shape, etc.) differ
#'   \item Assigns suffixes (_1, _2, etc.) to plots with different attributes
#'   \item Leaves truly identical duplicates as-is
#' }
#'
#' **Plot-Level Fields Checked:**
#' Coordinates, elevation, slope, aspect, area, shape, topographic position,
#' and other spatial attributes. Differences in these fields indicate distinct
#' plots that happen to share the same code.
# handle duplicate plot data
# TODO: redo this and 
deduplicate_plot_data <- function(plots_LT){
  
  plot_fields <- c(
    "real_latitude",
    "real_longitude",
    "latitude",
    "longitude",
    "location_accuracy",
    "confidentiality_status",
    "confidentiality_reason",
    "author_e",
    "author_n",
    "author_zone",
    "author_datum",
    "author_location",
    "location_narrative",
    "azimuth",
    "dsgpoly",
    "shape",
    "area",
    "stand_size",
    "placement_method",
    "permanence",
    "layout_narrative",
    "elevation",
    "elevation_accuracy",
    "elevation_range",
    "slope_aspect",
    "min_slope_aspect",
    "max_slope_aspect",
    "slope_gradient",
    "min_slope_gradient",
    "max_slope_gradient",
    "topo_position",
    "landform",
    "surficial_deposits",
    "rock_type"
  ) 
  
  present_fields <- intersect(plot_fields, names(plots_LT))
  
  needs_suffix <- plots_LT %>%
    group_by(user_pl_code) %>%
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    select(all_of(present_fields), user_pl_code) %>% 
    ungroup() %>% 
    mutate(dupe = duplicated(.) | duplicated(., fromLast = TRUE)) %>% 
    filter(!dupe) %>% 
    group_by(user_pl_code) %>% 
    mutate(row_num = row_number()) %>% 
    mutate(user_pl_code_dedup = paste0(user_pl_code, "_", row_num)) %>%
    select(-row_num, -dupe)
  
  plots_LT_fixed <- left_join(
    plots_LT,
    needs_suffix,
    by = join_by(
      user_pl_code,
      latitude,
      longitude,
      location_accuracy,
      confidentiality_status,
      author_e,
      author_n,
      author_zone,
      author_datum,
      author_location,
      location_narrative,
      azimuth,
      shape,
      area,
      stand_size,
      elevation,
      slope_aspect,
      slope_gradient,
      topo_position,
      rock_type
    )
  ) %>% 
    mutate(user_pl_code = if_else(!is.na(user_pl_code_dedup), user_pl_code_dedup, user_pl_code)) %>% 
    select(-user_pl_code_dedup)
  
  if (nrow(plots_LT_fixed) != nrow(plots_LT)){
    cli::cli_alert_warning("Deduplicated loader table does not have the same rows as original. Check deduplicate_plot_data function.")
  }
  
  return(plots_LT_fixed)
  
}

#' Main function that orchestrates the plot data processing pipeline from raw
#' CSV files to VegBank-compatible loader table
#' 
#' @param in_dir Directory of VegBank data to read from
#' @param out_dir Directory of data to write to
#' @param renew_cache Logical. If TRUE, refreshes cached VegBank API data
#' 
#' @return None
#' 
#' @details
#' This function executes a comprehensive data processing pipeline from data
#' loading and merging, coordinate processing, plot characteristics,
#' vegetation metrics, looking at data quality, and field mapping.
plots_loader <- function(in_dir, out_dir, renew_cache = FALSE){
  
  # read in all the files and join to one table
  plots_merged <- load_plot_files(in_dir)
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
  plots_merged <- calc_shrub_height(plots_merged)
  plots_merged <- calc_herb_height(plots_merged)
  plots_merged <- extract_location_description(plots_merged)
  plots_merged <- truncate_fields(plots_merged)
  # check for missing projects
  check_existing_plots(plots_merged, renew_cache = renew_cache)
  
  # Time should be removed
  plots_merged <- plots_merged %>% 
    mutate(SurveyDate = as_date(ymd(SurveyDate)))
  

  plots_LT <- plots_merged %>%
    mutate(SurveyID = toupper(SurveyID)) %>% 
    select(
      user_ob_code = SurveyID,
      user_pl_code = Stand_ID,
      author_obs_code = SurveyID,
      author_plot_code = Stand_ID,
      latitude = real_latitude,
      longitude = real_longitude,
      real_longitude,
      real_latitude,
      author_location,
      location_narrative,
      location_accuracy = ErrorMeasurement,
      confidentiality_status = ConfidentialityStatus,
      author_e = UTME_final,
      author_n = UTMN_final,
      author_zone = UTM_zone,
      author_datum,
      state_province = stateProvince,
      azimuth = W_Axis_Bearing,
      shape = PlotShape,
      area = PlotArea,
      stand_size = Stand_Size,
      elevation = Elevation,
      slope_aspect = Aspect_actual,
      slope_gradient = slope,
      topo_position,
      landscape_narrative = Site_history,
      rock_type = Substrate,
      user_pj_code = ProjectCode,
      obs_start_date = SurveyDate,
      method_narrative = methodNarrative,
      successional_status = Trend,
      basal_area = BasalStem,
      hydrologic_regime = Upl_Wet_text,
      percent_litter = Litter,
      percent_bare_soil = Bare_fines,
      percent_water = Water,
      tree_ht = treeHt,
      shrub_ht = Shrub_ht22,
      field_ht = Herb_ht22,
      tree_cover = treeCover,
      shrub_cover = Shrub_cover,
      field_cover = Herb_cover,
      nonvascular_cover = NonVasc_Veg_cover,
      dominant_stratum = DomForm,
      total_cover = Veg_cover,
      percent_bedrock = Bedrock,
      percent_rock_gravel = percentRockGravel,
      rock_type = Substrate
    )
  
  plots_LT <- deduplicate_plot_data(plots_LT)
  
  # save filled in loader table
  out_path <- file.path(out_dir, "plotsLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(plots_LT, out_path)

}


