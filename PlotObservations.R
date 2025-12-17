library(tidyverse)
library(here)
library(sf)
library(tigris)
library(rnaturalearth)
library(vegbankr)
source("Build_Loader_Table.R")

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

# read in RARevele CDFW data
folder <- 'data/RAReleve'

plots_1 <- read_csv(here(folder, 'RAPlots.csv'), 
                  col_types = cols( `PlotOther5` = col_character())) # PlotOther5 manually set to character due to parsing issue
alt_plots_1 <- read_csv(here(folder, 'AltPlots.csv')) %>% 
  mutate(AltPlotsID = NA,
         inVegBank = NA)
survey_points <- read_csv(here(folder, 'SurveyPoints.csv'))
impacts_1 <- read_csv(here(folder, 'RAImpacts.csv'))
alt_strata_1 <- read_csv(here(folder, 'AltStrata.csv')) %>% 
  mutate(AltStrataID = NA,
         InVegBank = NA ) # adding columns present in alt_plots_2
  mutate('AdditionalNotes' = NA,
        'inVegBank' = NA ) # adding columns present in alt_plots_2
classification_1 <- read_csv(here(folder, 'RAClassification.csv'))
projects_1 <- read_csv(here(folder, "RAProjects.csv"))

# read in AARecon data

folder_2 <- 'data/AARecon'

plots_2 <- read_csv(here(folder_2, 'RAPlots.csv'), 
                    col_types = cols( `PlotOther5` = col_character())) %>% 
  mutate('locfixed' = NA) # missing a column present in RAReleve's RAPlots.csv
alt_plots_2 <- read_csv(here(folder_2, 'AltPlots.csv'))
impacts_2 <- read_csv(here(folder_2, 'RAImpacts.csv'))
alt_strata_2 <- read_csv(here(folder_2, 'AltStrata.csv'))
classification_2 <- read_csv(here(folder_2, 'RAClassification.csv'))
projects_2 <- read_csv(here(folder_2, "RAProjects.csv"))

# merging together RAReleve and AARecon
plots <- rbind(plots_1, plots_2)
alt_plots <- rbind(alt_plots_1, alt_plots_2)
impacts <- rbind(impacts_1, impacts_2)

alt_strata <- rbind(alt_strata_1, alt_strata_2)

# read in CDFW lookup tables
# confidentiality_lookup <- read_csv(here(folder, 'LConfidentiality.csv'))
# height_lookup <- read_csv(here(folder, 'LHeight.csv'))
# standsize_lookup <- read_csv(here(folder, 'LStandSize.csv'))
# substrate_lookup <- read_csv(here(folder, 'LSubstrate.csv'))
# macrotopo_lookup <- read_csv(here(folder, 'LMacroTopo.csv'))
# slope_lookup <- read_csv(here(folder, 'LSlope.csv'))
# survey_lookup <- read_csv(here(folder, 'LSurveyType.csv'))



# create blank Loader Table dataframe -----------------------------------------------------

plots_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "PlotObservations",
  source_df = plots
)

plots_LT <- plots_template_fields$template

# checking values -------------------------------------------------------------

# SurveyDate  in RAPlots will be mampped to obsStartDate'
# Time needs to should be removed
head(plots$SurveyDate)

# Elevation in RAPlots will be mapped to 'elevation' related to ft_mElevation (RAPlots) - elevation (plots)
# Values will be converted to their unit of measurement as stated in the column ft_mElevation
unique(plots$ft_mElevation)                       

# Substrate  in RAPlots will be mapped to 'rock_type'
# Using the LSubstrate lookup table
# GitHub Issue #3: CA currently uses its own classifications (LSubstrate)
# We need to determine if CA classifications can be mapped onto FGCD standards
unique(plots$Substrate)

# Aspect_actual (RAPlots) to Aspect_gen (RAPlots)? - slope_aspect (plots)
# GitHub Issue #4: Apply the codes from the PlotObservations slope_aspect to
# RAPlots Aspect field
# If too flat to determine: -1
# If too irregular to determine: -2
# Flat locations can be listed as 0, 999, or NA
# Reference: Aspect_gen (RAPlots)
# Variable: NA
# Which one is irregular?
head(plots$Aspect_actual)
unique(plots$Aspect_gen)

# Slope_actual (RAPlots) to Slope_gen (RAPlots)? - slope_gradient (plots)
# Code if irregular to determine as well? Along with 0, 999, NA values
# Looks like Reference is Slope_gen
# Also, it looks like there are two categories for when the slope is over 25
# degrees, in values ">25 degrees" and "> 25 Degrees". Merge? GitHub Issue
# If Slope_actual is missing, take the midpoint of Slope_gen
# If Slope_gen > 25, use value 35
head(plots$Slope_actual)
unique(plots$Slope_gen)

# Stand_Size in RAPlots will be mapped to 'stand_size'
# Using the LStandSize lookup table
# Actually, thinking this doesn't need any tidying
unique(plots$Stand_Size)
unique(standsize_lookup$Stand_Size)
unique(standsize_lookup$StandSizeNum)

# PlotArea in RAPlots will be mapped to 'area'
# -1 indicates plot has no boundaries
# Data shows inconsistencies, there are different units and missing units
unique(plots$PlotArea)
# Also, combine PlotArea, ViewRadius, and SurveyDimensions together into area?
unique(plots$ViewRadius)
unique(plots$SurveyDimensions)

# PlotShape in RAPlots will be mapped to 'shape'
# Data is not all shapes. There are measurements as well
unique(plots$PlotShape)

# Shrub_ht2 in RAPlots and will be mapped to 'shrubHt'
# LoTreeTallShrub_Ht, LoMidShrub_Ht, and DwarfShrub_Ht in AltStrata.csv is empty, so will not combine
# Also not sure if we grab the maximum height and then combine
unique(plots$Shrub_ht2)
unique(alt_strata$LoTreeTallShrub_Ht) # NA
unique(alt_strata$LoMidShrub_Ht) # NA
unique(alt_strata$DwarfShrub_Ht) # NA

# Herb_ht2 in RAPlots will be mapped to 'fieldHt'
# Also not sure if we grab the maximum height and then combine
unique(plots$Herb_ht2)

# Survey_Type (RAPlots) & AdditionalNotes (AltPlots) - methodNarrative (PlotObservations)
# Reference: LSurveyType.csv
# Capitalize entries
unique(plots$Survey_Type) 

# tidying CDFW data -----------------------------------------------------------

# join AltPlots and AltStrata with RAPlots
plots_merged <- plots %>% 
  left_join(alt_plots, by = "SurveyID") %>% 
  left_join(alt_strata, by = "SurveyID") %>% 
  select(where(~!all(is.na(.x)))) # dropping columns that are filled with NA values
  
# location_accuracy -------------------------------------------------------
# ErrorMeasurement and ErrorUnits in  RAPlots
# If measurements are not in meters, needs to be converted
unique(plots_merged$ErrorUnits)
# need to figure out what to do with the values PDOP, pdop and Laptop
plots_merged <- plots_merged %>% 
  mutate(
    ErrorMeasurement = case_when(
      ErrorUnits %in% c("F", "ft", "ft.", 'feet') ~ ErrorMeasurement * 0.3048,
      TRUE ~ ErrorMeasurement
    )
  )

### author_datum (PlotObservations) ###
# GPS_datum (RAPlots)
# Inconsistent Formatting. Does VegBank need these changed?
# May delete this, if unnecessary
# Also, what is the correct format? "NAD83"?
unique(plots_merged$GPS_datum)
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
      author_datum == "WGS84" & UTM_zone == 10 ~ 32610,
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


# real_longitude & real_latitude ------------------------------------------
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

convert_to_ll <- function(df_group) {
  crs_utm <- unique(df_group$crs)
  if (length(crs_utm) != 1 || is.na(crs_utm)) return(NULL)
  
  st_as_sf(
    df_group,
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

df_coords <- plots_UTM %>%
  filter(!is.na(UTME_final),
         !is.na(UTMN_final),
         !is.na(crs)) %>%
  group_split(crs) %>%
  map_dfr(convert_to_ll)

plots_merged <- plots_UTM %>%
  left_join(df_coords, by = ".row_id") 
# %>% select(-.row_id)


# county, stateProvince, country, continent -------------------------------
options(tigris_use_cache = TRUE, tigris_class = "sf")

# getting state boundaries
states_sf <- tigris::states() %>% 
  st_transform(4326)

# getting courty boundaries
counties_sf <- tigris::counties() %>% 
  st_transform(4326)

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


# Troubleshooting rows with missing spatial information -------------------
missing_county <- plots_merged[is.na(plots_merged$county), ] # what to do with areas that could be in multiple states?
unique(missing_county$Location_name)

missing_state <- plots_merged[is.na(plots_merged$stateProvince), ]
unique(missing_state$Location_name)

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
plots$merged$continent <- "North America"


# area --------------------------------------------------------------------
#!!!PROBLEM!!!
# Using PlotArea and ViewRadius from RAPlots

# Removing units from values (VegBank assumes square meters)
plots_merged <- plots_merged %>% 
  mutate(
    PlotArea = str_remove(PlotArea, "^~ ?"), # removing leading ~ from '~700' record
    PlotArea = str_remove(PlotArea, " ?(m²|sq\\. ?m|sp\\. ?M|sq ?m|sq\\.? ?M)")) # removing units

# -1 indicates plot has no boundaries
# Also, combine PlotArea, ViewRadius, and SurveyDimensions together into area?
# 5 parsing failures
plots_merged <- plots_merged %>%
  mutate(PlotArea_num = parse_number(as.character(PlotArea), na = c("NA","na","Not recorded","not recorded")), # changing PlotArea to number, combining NAs
         dims = str_extract_all(as.character(SurveyDimensions), "\\d+(?:\\.\\d+)?"),
         SurveyLength = suppressWarnings(as.numeric(map_chr(dims, 1, .default = NA))),
         SurveyWidth  = suppressWarnings(as.numeric(map_chr(dims, 2, .default = NA))),
         # ADD: if PlotShape = 'circle' & is.na(radius). then calculate the are
         # area_from_radius = if_else(!is.na(ViewRadius), pi * (as.numeric(ViewRadius)^2), NA_real_),
         area_from_dims   = if_else(!is.na(SurveyLength) & !is.na(SurveyWidth),
                                    SurveyLength * SurveyWidth, NA_real_),
         PlotArea = coalesce(PlotArea_num, area_from_radius, area_from_dims, -1)
  )

# filling in the columns that are missing or incorrect in the PlotShape column
plots_merged <- plots_merged %>% 
  mutate(
    PlotShape = case_when(
      PlotShape == "10 m x 10 m" ~ "square",
      PlotShape == "12 m x 9 m" ~ "rectangle",
      PlotShape == "20 m x 5 m" ~ "rectangle",
      SurveyDimensions == "10 m x 10 m" & is.na(PlotShape) ~ "square",
      TRUE ~ PlotShape
    )
  )

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


### elevation (PlotObservations) ###
# Elevation (RAPlots) related to ft_mElevation (RAPlots)
# Numbers should be converted to meters if the unit in ft_mElevation says ft.
plots_merged <- plots_merged %>% 
  mutate(
    Elevation = case_when(
      ft_mElevation %in% c("F", "ft", "ft.") ~ Elevation * 0.3048,
      TRUE ~ Elevation
    )
  )

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
plots_merged <- plots_merged %>%
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

### methodNarrative (PlotObservations) ###
# Survey_Type (RAPlots) and AdditionalNotes (AltPlots)
# AltPlots is empty
# Capitalizing entries
plots_merged <- plots_merged %>% 
  mutate(
    methodNarrative = case_when(
      Survey_Type == "Rapid assessment" ~ "Rapid Assessment",
      Survey_Type == "releve" ~ "Releve",
      Survey_Type == "rapid assessment" ~ "Rapid Assessment"
    )
  )

### topo_position (PlotObservations) ###
# MacroTopo (RAPlots)
# Convert to corresponding values in LMacroTopo.csv
plots_merged <- plots_merged %>% 
  mutate(
    topo_position = case_when(
      MacroTopo == "upper" ~ "Upper 1/3 of slope",
      MacroTopo == "mid" ~ "Middle 1/3 of slope",
      MacroTopo == "6" ~ "Bottom",
      TRUE ~ MacroTopo
    )
  )

### user_ob_code (PlotObservations) ###
# SurveyID (RAClassifications)
plots_merged <- plots_merged %>%
  mutate(
    user_ob_code = classification$SurveyID[match(SurveyID,
                                                 classification$SurveyID)]
  )

### obsStartDate (PlotObservations) ###
# SurveyDate (RAPlots)
# Time should be removed
plots_merged <- plots_merged %>% 
  mutate(
    SurveyDate = as_date(mdy_hms(SurveyDate))
  )

### percentOther (PlotObservations) ###
# Boulders/Stones/Cobbles/Gravels (RAPlots) - percentRockGravel (plots)
# Boulders/Stones/Cobbles (RAPlots) - percentOther (PlotObservations)
# Gravels (RAPlots) - percentRockGravel (PlotObservations)
# Need to combine 4 columns into one
# CDFW unsure if we want Boulders/Stones/Cobbles with percentOther or
# percentRockGravel
# After exploratory tables, combine into percentRockGravel
plots_merged <- plots_merged %>% 
  mutate(
    percentRockGravel = rowSums(cbind(Boulders, Stones, Cobbles, Gravels))
    )

# Substrate (RAPlots) - rock_type (plots)
# GitHub Issue #3 needs more clarification

# Conif_cover/Hdwd_cover/RegenTree_cover (RAPlots) - treeCover (plots)
# Need to combine 3 columns into one
plots_merged <- plots_merged %>% 
  mutate(
    treeCover = rowSums(cbind(Hdwd_cover, Conif_cover, RegenTree_cover))
  )

### treeHt (PlotObservations) ###
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
      
      # Out of Range Measurements (Must Adjust! These are placeholders)
      Conif_ht2 == "<0.5 m" ~ 0.25,
      Conif_ht2 == ">50 m" ~ 55,
      Conif_ht2 == ">50m" ~ 55,
      
      # Miscellaneous
      Conif_ht2 == "0" ~ 0,
      
      # Missing Values
      Conif_ht2 == "N/A" ~ NA,
      Conif_ht2 == "Not recorded" ~ NA,
      Conif_ht2 == "Not present" ~ NA,
      
      TRUE ~ NA_real_
    )
  )

# Hdwd_ht2
plots_merged <- plots_merged %>% 
  mutate(
    Hdwd_ht22 = case_when(
      
      # Midpoint Measurements
      Hdwd_ht2 == "2-5 m" ~ 3.5,
      Hdwd_ht2 == "1-2 m" ~ 1.5,
      Hdwd_ht2 == "0.5-1 m" ~ 0.75,
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
      
      TRUE ~ NA_real_
    )
  )

plots_merged <- plots_merged %>% 
  mutate(
    # Formula
    conif_ratio = Conif_cover / (Conif_cover + Hdwd_cover)
  ) %>% 
  
  mutate(
    treeHt = case_when(
      
      # Conif_ht2 has an observation but Hdwd_ht2 doesn't
      !is.na(Conif_ht22) & is.na(Hdwd_ht22) & Conif_ht22 == 7.5 ~ 7.5,
      !is.na(Conif_ht22) & is.na(Hdwd_ht22) & Conif_ht22 == 0.75 ~ 0.75,
      !is.na(Conif_ht22) & is.na(Hdwd_ht22) & Conif_ht22 == 12.5 ~ 12.5,
      !is.na(Conif_ht22) & is.na(Hdwd_ht22) & Conif_ht22 == 3.5 ~ 3.5,
      !is.na(Conif_ht22) & is.na(Hdwd_ht22) & Conif_ht22 == 27.5 ~ 27.5,
      !is.na(Conif_ht22) & is.na(Hdwd_ht22) & Conif_ht22 == 17.5 ~ 17.5,
      !is.na(Conif_ht22) & is.na(Hdwd_ht22) & Conif_ht22 == 42.5 ~ 42.5,
      !is.na(Conif_ht22) & is.na(Hdwd_ht22) & Conif_ht22 == 1.5 ~ 1.5,
      TRUE ~ NA_real_
      )
    ) %>% 
  
  mutate(
    treeHt = case_when(
      # Hdwd_ht2 has an observation but Conif_ht2 doesn't
      !is.na(Hdwd_ht22) & is.na(Conif_ht22) & Hdwd_ht22 == 3.5 ~ 3.5,
      !is.na(Hdwd_ht22) & is.na(Conif_ht22) & Hdwd_ht22 == 1.5 ~ 1.5,
      !is.na(Hdwd_ht22) & is.na(Conif_ht22) & Hdwd_ht22 == 0.75 ~ 0.75,
      !is.na(Hdwd_ht22) & is.na(Conif_ht22) & Hdwd_ht22 == 17.5 ~ 17.5,
      !is.na(Hdwd_ht22) & is.na(Conif_ht22) & Hdwd_ht22 == 7.5 ~ 7.5,
      !is.na(Hdwd_ht22) & is.na(Conif_ht22) & Hdwd_ht22 == 12.5 ~ 12.5,
      !is.na(Hdwd_ht22) & is.na(Conif_ht22) & Hdwd_ht22 == 27.5 ~ 27.5,
      !is.na(Hdwd_ht22) & is.na(Conif_ht22) & Hdwd_ht22 == 42.5 ~ 42.5,
      TRUE ~ NA_real_
    )
  ) %>% 
  
  mutate(
    treeHt = case_when(
      
      # Both Hdwd_ht2 and Conif_ht2 has observations, >= 30%, Hdwd_ht2 is taller
      !is.na(Hdwd_ht22) & !is.na(Conif_ht22) & conif_ratio >= 0.30 
      & Hdwd_ht22 > Conif_ht22 ~ Hdwd_ht22,
      
      # Both Hdwd_ht2 and Conif_ht2 has observations, >= 30%, Conif_ht2 is taller
      !is.na(Hdwd_ht22) & !is.na(Conif_ht22) & conif_ratio >= 0.30
      & Conif_ht22 > Hdwd_ht22 ~ Conif_ht22,
      
      # Both Hdwd_ht2 and Conif_ht2 has observations, <30%
      !is.na(Hdwd_ht22) & !is.na(Conif_ht22) & conif_ratio < 0.30
      ~ Hdwd_ht22,
      
      TRUE ~ NA_real_
    )
  )

### growthform1/2Cover (PlotObservations) ###
# Conif_ht2
plots_merged <- plots_merged %>% 
  mutate(
    growthform1Cover = case_when(
      
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
      
      # Out of Range Measurements (Must Adjust! These are placeholders)
      Conif_ht2 == "<0.5 m" ~ 0.25,
      Conif_ht2 == ">50 m" ~ 55,
      Conif_ht2 == ">50m" ~ 55,
      
      # Miscellaneous
      Conif_ht2 == "0" ~ 0,
      
      # Missing Values
      Conif_ht2 == "N/A" ~ NA,
      Conif_ht2 == "Not recorded" ~ NA,
      Conif_ht2 == "Not present" ~ NA,
      
      TRUE ~ NA_real_
    )
  )

# Hdwd_ht2
plots_merged <- plots_merged %>% 
  mutate(
    growthform2Cover = case_when(
      
      # Midpoint Measurements
      Hdwd_ht2 == "2-5 m" ~ 3.5,
      Hdwd_ht2 == "1-2 m" ~ 1.5,
      Hdwd_ht2 == "0.5-1 m" ~ 0.75,
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
      
      TRUE ~ NA_real_
    )
  )

### growthform1/2Type (PlotObservations) ###
# growthform1Type
plots_merged <- plots_merged %>% 
  mutate(
    growthform1Type = case_when(
      !is.na(growthform1Cover) ~ "Conifer Tree",
      is.na(growthform1Cover) ~ NA,
      
      TRUE ~ NA_character_
    )
  )

# growthform2Type
plots_merged <- plots_merged %>% 
  mutate(
    growthform2Type = case_when(
      !is.na(growthform2Cover) ~ "Hardwood Tree",
      is.na(growthform2Cover) ~ NA,
      
      TRUE ~ NA_character_
    )
  )

### user_pl_code (PlotObservations) ###
# For now, there is no matches so user_pl_code will remain empty
# set_vb_base_url("https://api-dev.vegbank.org")

# CREATING DF BY LOOPING THROUGH "PAGES" OF VALUES
# saved as csv so commented the code

# Adaptive, resumable pager for VegBank plot observations

# page_init  <- 5000  # shrink this if there is an error
# page_min   <- 500   # don't go smaller than this
# max_pages  <- 500   # hard stop
# sleep_sec  <- 0.05  # brief pause to avoid error
# keep_cols  <- c("pl_code","latitude","longitude","ob_code","author_plot_code", "author_obs_code", "state_province","country")
# checkpoint <- "pl_all_checkpoint.rds" # just in case something fails
# save_every <- 10

# out <- list()
# seen_codes <- character(0)
# limit <- page_init

# column types
# text_cols <- c("ob_code","state_province","country", "author_obs_code")
# num_cols  <- c("latitude","longitude")

# for (i in seq_len(max_pages)) {
  # offset <- (i - 1L) * limit
  # message(sprintf("Page %d | limit=%d | offset=%d", i, limit, offset))
  
  # try once
  # on failure (e.g., 504), halve the limit and retry
  # chunk <- tryCatch(
    # get_all_plot_observations(limit = limit, offset = offset),
    # error = function(e) {
      # message("  Request failed: ", conditionMessage(e))
      # limit <<- max(page_min, floor(limit/2))
      # message("  Reducing limit and retrying with limit=", limit)
      # tryCatch(get_all_plot_observations(limit = limit, offset = offset),
               # error = function(e2) { message("  Retry failed."); NULL })
    # }
  # )
  # if (is.null(chunk) || !nrow(chunk)) { message("  No rows returned; stopping."); break }
  
  # keep <- intersect(keep_cols, names(chunk))
  # if (length(keep)) chunk <- chunk[, keep, drop = FALSE]
  
  # normalize
  # chunk <- chunk %>%
    # mutate(
      # across(any_of(text_cols), as.character),
      # across(any_of(num_cols),  as.numeric)
    # )
  
  # if ("pl_code" %in% names(chunk)) {
    # new <- !chunk$pl_code %in% seen_codes
    # if (!any(new)) { message("  All rows seen already; stopping."); break }
    # seen_codes <- c(seen_codes, chunk$pl_code[new])
    # chunk <- chunk[new, , drop = FALSE]
  # }
  
  # out[[length(out) + 1L]] <- chunk
  # total <- sum(vapply(out, nrow, integer(1)))
  # message(sprintf("  +%d new rows (total: %d)", nrow(chunk), total))
  
  # if (nrow(chunk) < limit) { message("  Short page; done."); break }
  
  # if (save_every > 0 && (i %% save_every == 0)) {
  # Normalize again
    # out_fixed <- map(out, ~ .x %>%
                       # mutate(
                         # across(any_of(text_cols), as.character),
                         # across(any_of(num_cols),  as.numeric)
                       # ))
    # tmp <- bind_rows(out_fixed) %>% distinct()
    # saveRDS(tmp, checkpoint)
    # message(sprintf("  Saved checkpoint (%d rows) -> %s", nrow(tmp), checkpoint))
  # }
  
  # if (sleep_sec > 0) Sys.sleep(sleep_sec)
# }

# normalize again
# out_fixed <- map(out, ~ .x %>%
                   # mutate(
                     # across(any_of(text_cols), as.character),
                     # across(any_of(num_cols),  as.numeric)
                   # ))

# pl_all <- bind_rows(out_fixed) %>% distinct()

# dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
# write_csv(pl_all, here("data", "pl_all.csv"))

csv_path <- here("data", "pl_all.csv")
pl_all <- read_csv(csv_path, show_col_types = FALSE)

plots_merged <- plots_merged %>%
  left_join(
    pl_all %>% select(author_plot_code, pl_code),
    by = c("SurveyID" = "author_plot_code")
  )

sum(plots_merged$SurveyID %in% pl_all$author_plot_code)

normalize_id <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9]", "") %>%
    str_squish()
}

RA_ids <- plots_merged %>%
  mutate(SurveyID_norm = normalize_id(SurveyID)) %>%
  pull(SurveyID_norm) %>%
  unique()

VB_ids <- pl_all %>%
  mutate(author_norm = normalize_id(author_plot_code)) %>%
  pull(author_norm) %>%
  unique()

sum(RA_ids %in% VB_ids)

# Since there are no matches above, vb_pl_code will be left empty for now.

### vb_pj_code ####

# pj_all <- get_all_projects(limit = 1000)

# dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
# write_csv(pj_all, here("data", "pj_all.csv"))

pj_all <- read_csv(here("data", "pj_all.csv"), show_col_types = FALSE)

pj_all <- pj_all %>%
  mutate(project_name_norm = str_squish(str_to_lower(project_name)),
         project_code_norm = str_squish(str_to_lower(pj_code)))

projects_norm <- projects %>%
  mutate(
    ProjectName_norm = str_squish(str_to_lower(ProjectName)),
    ProjectCode_norm = str_squish(str_to_lower(ProjectCode))
  )

match_by_name <- projects_norm %>%
  left_join(
    pj_all %>% select(pj_code, project_name_norm),
    by = c("ProjectName_norm" = "project_name_norm")
  )

match_by_code <- match_by_name %>%
  left_join(
    pj_all %>% select(pj_code, project_code_norm),
    by = c("ProjectCode_norm" = "project_code_norm"),
    suffix = c("", "_fromCode")
  )

sum(!is.na(match_by_name$pj_code))
sum(!is.na(match_by_code$pj_code_fromCode))

head(projects_norm %>% select(ProjectName, ProjectName_norm, ProjectCode, ProjectCode_norm))
head(pj_all %>% select(project_name, project_name_norm, pj_code, project_code_norm))

intersect(projects_norm$ProjectName_norm, pj_all$project_name_norm)
intersect(projects_norm$ProjectCode_norm, pj_all$project_code_norm)
# Assigning columns to loader table -------------------------------------------
plots_LT$author_plot_code <- plots_merged$SurveyID
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
plots_LT$obsStartDate <- plots_merged$SurveyDate
plots_LT$methodNarrative <- plots_merged$methodNarrative
plots_LT$successionalStatus = plots_merged$Trend
plots_LT$basalArea <- plots_merged$BasalStem
plots_LT$hydrolicRegime <- plots_merged$Upl_Wet_text
plots_LT$percentLitter <- plots_merged$Litter
plots_LT$percentBareSoil <- plots_merged$Bare_fines
plots_LT$percentWater <- plots_merged$Water
plots_LT$treeHt <- plots_merged$treeHt
plots_LT$shrubHt <- plots_merged$Shrub_ht2
plots_LT$fieldHt <- plots_merged$Herb_ht2
plots_LT$treeCover <- plots_merged$treeCover
plots_LT$shrubCover <- plots_merged$Shrub_cover
plots_LT$fieldCover <- plots_merged$Herb_cover
plots_LT$nonvascularCover <- plots_merged$NonVasc_Veg_cover
plots_LT$dominantStratum <- plots_merged$DomLayer
plots_LT$growthform1Cover <- plots_merged$growthform1Cover
plots_LT$growthform2Cover <- plots_merged$growthform2Cover
plots_LT$growthform1Type <- plots_merged$growthform1Type
plots_LT$growthform2Type <- plots_merged$growthform2Type

# save filled in loader table
write_csv(plots_LT, here('loader_tables', 'PlotObservationsLT.csv'))
