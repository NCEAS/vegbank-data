library(tidyverse)
library(here)
library(sf)
source("Build_Loader_Table.R")

# Personal Notes (Will Delete):
# waterDepth: no mapping
# shoreDistance: no mapping
# soilDepth: no mapping
# organicDepth: no mapping
# vb_so_code: no mapping
# soilTaxonSrc: no mapping
# percentBedRock: RAPlots' Bedrock
# percentRockGravel: RAPlots' Gravels (Maybe RAPlots' Boulders, Stones, Cobbles; AltPlots' Small_rock?) For now, I map to just Gravels.
# percentWood: no mapping
# percentLitter: RAPlots' Litter
# percentBareSoil: RAPlots' Bare_fines
# percentWater: RAPlots' Water
# percentOther: Maybe combine RAPlots' Boulders, Stones, and Cobbles?
# nameOther: Possibly AltPlots' Small_rock and combine percentOther? Won't do it yet. I leave it alone for now.
# treeHt: RAPlots' Conif_ht2, Hdwd_ht2, UnderTree_ht2, RegenTree_ht2; AltStrata's OverstoryTree_Ht, EmergentTree_Ht. But AltStrata has missing values.
# shrubHt: RAPlots' Shrub_ht2; AltStrata's LoTreeTallShrub_Ht, LoMidShrub_Ht, DwarfShrub_Ht. AltStrata also has missing values.
# fieldHt: RAPlots' Herb_ht2, but it has a range problem like treeHt and shrubHt.
# nonvascularHt: AltStrata's Nonvascular_Ht, but it is empty?
# submergedHt: no mapping
# treeCover: RAPlots' Conif_cover, AltStrata's Epiphyte_Cov, OverstoryTree_Cov, EmergentTree_Cov. AltStrata is empty.
# shrubCover: RAPlots' Shrub_cover. AltStrata is empty, but AltStrata's LoTreeTallShrub_Cov, LoMidShrub_Cov, DwarfShrub_Cov
# fieldCover: RAPlots' Herb_cover. Looks good!
# nonvascularCover: RAPlots' NonVasc_Veg_cover, AltStrata's Nonvascular_Cov. AltStrata is empty.
# floatingCover: no mapping
# submergedCover: no mapping
# dominantStratum: AltPlots' DomLayer, but they're all empty?
# growthform1Type: no mapping
# growthform2Type: no mapping
# growthform3Type: no mapping
# growthform1Cover: no mapping
# growthform2Cover: no mapping
# growthform3Cover: no mapping
# totalCover: no mapping

# load in CDFW data -----------------------------------------------------------

# set folder where data is saved
folder <- 'data'

# RAPlots
# Column 84 was set to character due to parsing issue
plots <- read_csv(here(folder, 'RAPlots.csv'), 
                  col_types = cols(.default = col_guess(), 
                                   `PlotOther5` = col_character()))
alt_plots <- read_csv(here(folder, 'AltPlots.csv'))
survey_points <- read_csv(here(folder, 'SurveyPoints.csv'))
impacts <- read_csv(here(folder, 'RAImpacts.csv'))
alt_strata <- read_csv(here(folder, 'AltStrata.csv'))
classification <- read_csv(here(folder, 'RAClassification.csv'))

# loading CA lookup tables
confidentiality_lookup <- read_csv(here(folder, 'LConfidentiality.csv'))
height_lookup <- read_csv(here(folder, 'LHeight.csv'))
standsize_lookup <- read_csv(here(folder, 'LStandSize.csv'))
substrate_lookup <- read_csv(here(folder, 'LSubstrate.csv'))
macrotopo_lookup <- read_csv(here(folder, 'LMacroTopo.csv'))
slope_lookup <- read_csv(here(folder, 'LSlope.csv'))
survey_lookup <- read_csv(here(folder, 'LSurveyType.csv'))

# creating loader table -------------------------------------------------------

# create blank data frame -----------------------------------------------------

plots_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "PlotObservations",
  source_df = plots
)

plots_LT <- plots_template_fields$template

# checking values -------------------------------------------------------------

# SurveyID (RAPlots) - author_plot_code (PlotObservations)
# AltPlots can be joined with RAPlots
intersect(plots$SurveyID, alt_plots$SurveyID)
setdiff(alt_plots$SurveyID, plots$SurveyID)
# SurveyPoints can be joined with RAPlots
intersect(plots$SurveyID, survey_points$SurveyID) # Empty SurveyPoints
# RAClassification can be joined with RAPlots
intersect(plots$SurveyID, classification$SurveyID)
# AltStrata can be joined with RAPlots
intersect(plots$SurveyID, alt_strata$SurveyID) # Almost Empty AltStrata
setdiff(alt_strata$SurveyID, plots$SurveyID)

# ErrorMeasurement (RAPlots) + ErrorUnits (RAPlots) - location_accuracy (PlotObservations)
# Numbers should be converted to their unit of measurement as stated in
# ErrorUnits
unique(plots$ErrorMeasurement)
unique(plots$ErrorUnits)
class(plots$ErrorMeasurement) # numeric
class(plots$ErrorUnits) # character
# ErrorUnits also contains PDOP and Laptop. What to do with those?

# SurveyDate (RAPlots) - obsStartDate (plots)
# Time should be removed
unique(plots$SurveyDate)
class(plots$SurveyDate) # character

# Elevation (RAPlots) related to ft_mElevation (RAPlots) - elevation (plots)
# Numbers should be converted to their unit of measurement as stated in
# ft_mElevation
unique(plots$Elevation)
unique(plots$ft_mElevation)                       
class(plots$Elevation) # numeric
class(plots$ft_mElevation) # character

# Substrate (RAPlots) - rock_type (plots)
# LSubstrate
# GitHub Issue #3: CA currently uses its own classifications (LSubstrate)
# We need to determine if CA classifications can be mapped onto FGCD standards
unique(plots$Substrate)
class(plots$Substrate) # character
# Ask what this means

# Aspect_actual (RAPlots) to Aspect_gen (RAPlots)? - slope_aspect (plots)
# GitHub Issue #4: Apply the codes from the PlotObservations slope_aspect to
# RAPlots Aspect field
# If too flat to determine: -1
# If too irregular to determine: -2
# Flat locations can be listed as 0, 999, or NA
# Reference: Aspect_gen (RAPlots)
# Variable: NA
# Which one is irregular?
unique(plots$Aspect_actual)
unique(plots$Aspect_gen)
class(plots$Aspect_actual) # numeric
class(plots$Aspect_gen) # character

# Slope_actual (RAPlots) to Slope_gen (RAPlots)? - slope_gradient (plots)
# Code if irregular to determine as well? Along with 0, 999, NA values
# Looks like Reference is Slope_gen
# Also, it looks like there are two categories for when the slope is over 25
# degrees, in values ">25 degrees" and "> 25 Degrees". Merge? GitHub Issue
# If Slope_actual is missing, take the midpoint of Slope_gen
# If Slope_gen > 25, use value 35
unique(plots$Slope_actual)
unique(plots$Slope_gen)
class(plots$Slope_actual) # numeric
class(plots$Slope_gen) # character

# Stand_Size (RAPlots) - stand_size (plots)
# LStandSize
# Numbers will be matched to their code in LStandSize
# Actually, thinking this doesn't need any tidying
unique(plots$Stand_Size)
unique(standsize_lookup$Stand_Size)
unique(standsize_lookup$StandSizeNum)
class(plots$Stand_Size) # character
class(standsize_lookup$Stand_Size) # character
class(standsize_lookup$StandSizeNum) # numeric

# PlotArea (RAPlots) - area (plots)
# -1 indicates plot has no boundaries
# Data shows inconsistencies, there are different units and missing units
unique(plots$PlotArea)
class(plots$PlotArea) # character
# Also, combine PlotArea, ViewRadius, and SurveyDimensions together into area?
unique(plots$ViewRadius)
unique(plots$SurveyDimensions)
class(plots$ViewRadius) # numeric
class(plots$SurveyDimensions) # character

# PlotShape (RAPlots) - shape (plots)
# Data is not all shapes. There are measurements as well
unique(plots$PlotShape)
class(plots$PlotShape) # character

# Boulders/Stones/Cobbles/Gravels (RAPlots) - percentRockGravel (plots)
# Need to combine 4 columns into one
unique(plots$Boulders)
unique(plots$Stones)
unique(plots$Cobbles)
unique(plots$Gravels)
class(plots$Boulders) # numeric
class(plots$Stones) # numeric
class(plots$Cobbles) # numeric
class(plots$Gravels) # numeric

# Conif_cover/Hdwd_cover/RegenTree_cover (RAPlots) - treeCover (plots)
# Need to combine 3 columns into one
# UnderTree_cover is missing values, so it will not be included
unique(plots$Conif_cover)
unique(plots$Hdwd_cover)
unique(plots$RegenTree_cover)
class(plots$Conif_cover) # numeric
class(plots$Hdwd_cover) # numeric
class(plots$RegenTree_cover) # numeric

# Latitude_WGS84_Final (AltPlots) - real_latitude (plots)
unique(alt_plots$Latitude_WGS84_Final)
class(alt_plots$Latitude_WGS84_Final) # numeric

# Longitude_WGS84_Final (AltPlots) - real_longitude (plots)
unique(alt_plots$Longitude_WGS84_Final)
class(alt_plots$Longitude_WGS84_Final)

# UTME_final (RAPlots) - author_e (PlotObservations)
unique(plots$UTME_final)
class(plots$UTME_final) # numeric

# UTMN_final (RAPlots) - author_n (PlotObservations)
unique(plots$UTMN_final)
class(plots$UTMN_final) # numeric

# GPS_datum (RAPlots) - author_datum (PlotObservations)
# Formatting is all over the place. Does VegBank want these to be changed to
# consistent formatting?
unique(plots$GPS_datum)
class(plots$GPS_datum) # character

# When assigning columns to loader table, column types are all changed to
# numeric.

# MacroTopo (RAPlots) - topo_position (PlotObservations)
# Looks like there are some messy values, like "upper" and "6"
unique(plots$MacroTopo)
class(plots$MacroTopo) # character

# Trend (AltPlots) - successionalStatus (PlotObservations)
# Looks fine
unique(alt_plots$Trend)
class(alt_plots$Trend) # character

# BasalStem (RAPlots) - basalArea (PlotObservations)
unique(plots$BasalStem)
class(plots$BasalStem) # numeric

# Upl_Wet_text (RAPlots) - hydrologicRegime (PlotObservations)
# Looks fine
unique(plots$Upl_Wet_text)
class(plots$Upl_Wet_text) # character

# Gravels (RAPlots) - percentRockGravel (PlotObservations)
# May need to get combined with the other percents
unique(plots$Gravels)
class(plots$Gravels) # numeric

# Boulders, Stones, Cobbles (RAPlots) - percentOther (PlotObservations)
# Combine
# Mapping may change
unique(plots$Boulders)
unique(plots$Stones)
unique(plots$Cobbles)
class(plots$Boulders) # numeric
class(plots$Stones) # numeric
class(plots$Cobbles) # numeric

# Conif_ht2, Hdwd_ht2, UnderTree_ht2, RegenTree_ht2 (RAPlots), OverstoryTree_Ht, EmergentTree_Ht (AltStrata) - treeHt (PlotObservations)
# long explanation
unique(plots$Conif_ht2)
unique(plots$Hdwd_ht2)
unique(plots$RegenTree_ht2)
unique(alt_strata$OverstoryTree_Ht) # AltStrata has no values
unique(alt_strata$EmergentTree_Ht) # AltStrata has no values
class(plots$Conif_ht2) # character
class(plots$Hdwd_ht2) # character
class(plots$RegenTree_ht2) # character

# Shrub_ht2 (RAPlots), LoTreeTallShrub_Ht, LoMidShrub_Ht, DwarfShrub_Ht (AltStrata) - shrubHt (PlotObservations)
# Also not sure if we grab the maximum height and then combine
# AltStrata has missing values again. I don't think we combine?
# May also need to convert to numeric
unique(plots$Shrub_ht2)
unique(alt_strata$LoTreeTallShrub_Ht) # NA
unique(alt_strata$LoMidShrub_Ht) # NA
unique(alt_strata$DwarfShrub_Ht) # NA
class(plots$Shrub_ht2) # character

# Herb_ht2 (RAPlots) - fieldHt (PlotObservations)
# Also not sure if we grab the maximum height and then combine
# May also need to convert to numeric
unique(plots$Herb_ht2)
class(plots$Herb_ht2) # character

# Survey_Type (RAPlots) & AdditionalNotes (AltPlots) - methodNarrative (PlotObservations)
# Reference: LSurveyType.csv
# Capitalize entries
unique(plots$Survey_Type) 
class(plots$Survey_Type) # character

# tidying CDFW data -----------------------------------------------------------

plots_merged <- plots

### author_plot_code (PlotObservations) ###
# AltPlots can be joined with RAPlots
plots_merged <- plots_merged %>% 
  left_join(alt_plots, by = "SurveyID")
# AltStrata can be joined with RAPlots
plots_merged <- plots_merged %>% 
  left_join(alt_strata, by = "SurveyID")
# Very empty
# SurveyPoints can be joined but there is nothing in it

### location_accuracy (PlotObservations) ###
# ErrorMeasurement + ErrorUnits (RAPlots)
# Numbers should be converted to their unit of measurement in ErrorUnits
plots_merged <- plots_merged %>% 
  mutate(
    ErrorMeasurement = case_when(
      ErrorUnits %in% c("F", "ft", "ft.") ~ ErrorMeasurement * 0.3048,
      TRUE ~ ErrorMeasurement
    )
  )

### author_e (PlotObservations) ###
### author_n (PlotObservations) ###
# UTME_final (RAPlots) and UTMN_final (RAPlots)
# Convert UTM to lat long
# Possibly switch to UTME instead of UTME_final?
# Two total zones: 10 and 11
# I will split by zone, convert, then merge them back together
# It keeps removing NA values by default and I am trying to keep them
# New Version (To not affect row numbers)
plots_merged <- plots_merged %>%
  mutate(
    .row_id = row_number(),
    UTM_zone_chr = str_to_upper(str_squish(as.character(UTM_zone)))
  )

df_10N <- plots_merged %>%
  filter(UTM_zone_chr == "10",
         !is.na(UTME_final), !is.na(UTMN_final)) %>%
  st_as_sf(coords = c("UTME_final", "UTMN_final"), crs = 32610, na.fail = FALSE) %>%
  st_transform(4326) %>%
  mutate(
    author_e = st_coordinates(geometry)[, 1],  # lon
    author_n = st_coordinates(geometry)[, 2]   # lat
  ) %>%
  st_drop_geometry() %>%
  select(.row_id, author_e, author_n)

df_11N <- plots_merged %>%
  filter(UTM_zone_chr == "11",
         !is.na(UTME_final), !is.na(UTMN_final)) %>%
  st_as_sf(coords = c("UTME_final", "UTMN_final"), crs = 32611, na.fail = FALSE) %>%
  st_transform(4326) %>%
  mutate(
    author_e = st_coordinates(geometry)[, 1],
    author_n = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(.row_id, author_e, author_n)

# combine computed coords and join back to FULL dataset (preserves row count)
df_N_all <- bind_rows(df_10N, df_11N)

# merge
plots_merged <- plots_merged %>%
  left_join(df_N_all, by = ".row_id") %>%
  select(-.row_id, -UTM_zone_chr)

### author_datum (PlotObservations) ###
# GPS_datum (RAPlots)
# Inconsistent Formatting. Does VegBank need these changed?
# May delete this, if unnecessary
# Also, what is the correct format? "NAD83"?
plots_merged <- plots_merged %>% 
  mutate(
    author_datum = case_when(
      GPS_datum %in% c("Nad83", "NAD 83", "NAD83") ~ "NAD83",
      GPS_datum %in% c("WGS84", "WGS 84") ~ "WGS84",
      GPS_datum %in% c("NAD 27") ~ "NAD27",
      TRUE ~ GPS_datum
    )
  )
# Use author_datum to assign column to loader table

### area (PlotObservations) ### !!!PROBLEM!!!
# PlotArea (RAPlots) and ViewRadius (RAPlots)
# Units will be removed
plots_merged <- plots_merged %>% 
  mutate(PlotArea = str_remove(PlotArea, 
                               " ?(m²|sq\\. ?m|sp\\. ?M|sq ?m|sq\\.? ?M)"))
# There is a value here that looks like "~700". Should we remove the "~"?

# -1 indicates plot has no boundaries
# Also, combine PlotArea, ViewRadius, and SurveyDimensions together into area?
# 5 parsing failures
plots_merged <- plots_merged %>%
  mutate(PlotArea_num = parse_number(as.character(PlotArea), na = c("NA","na","Not recorded","not recorded")),
         dims = str_extract_all(as.character(SurveyDimensions), "\\d+(?:\\.\\d+)?"),
         SurveyLength = suppressWarnings(as.numeric(map_chr(dims, 1, .default = NA))),
         SurveyWidth  = suppressWarnings(as.numeric(map_chr(dims, 2, .default = NA))),
         area_from_radius = if_else(!is.na(ViewRadius), pi * (as.numeric(ViewRadius)^2), NA_real_),
         area_from_dims   = if_else(!is.na(SurveyLength) & !is.na(SurveyWidth),
                                    SurveyLength * SurveyWidth, NA_real_),
         PlotArea = coalesce(PlotArea_num, area_from_radius, area_from_dims, -1)
  )

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
# PlotShape will be entered in as-is except for 10m x 10m = square, 12x9 =
# rectangle, 20x5 = rectangle, and the two blank rows in PlotShape where
# SurveyDimensions is equal to 10mx10m will be changed to Square in PlotShape

### shape (PlotObservations) ###
# PlotShape (RAPlots)
# I'll convert 10x10 to square, 12x9 to rect, 20x5 to rect, and the 
# surveydimensions' 10x10 to square
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
# plots_merged$shape

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
# plots_merged <- plots_merged %>% 
#   mutate(
#     percentRockGravel = rowSums(cbind(Boulders, Stones, Cobbles, Gravels))
#   )
plots_merged <- plots_merged %>% 
  mutate(
    percentOther = rowSums(cbind(Boulders, Stones, Cobbles))
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

# Assigning columns to loader table -------------------------------------------
plots_LT$author_plot_code <- plots_merged$SurveyID
plots_LT$real_latitude <- plots_merged$Latitude_WGS84_Final
plots_LT$real_longitude <- plots_merged$Longitude_WGS84_Final
plots_LT$location_accuracy <- plots_merged$ErrorMeasurement
plots_LT$confidentiality_status <- plots_merged$ConfidentialityStatus
plots_LT$author_e <- plots_merged$author_e
plots_LT$author_n <- plots_merged$author_n
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
plots_LT$successionalStatus = plots_merged$Trend
plots_LT$basalArea <- plots_merged$BasalStem
plots_LT$hydrolicRegime <- plots_merged$Upl_Wet_text
plots_LT$treeHt <- plots_merged$treeHt
plots_LT$shrubHt <- plots_merged$Shrub_ht2
plots_LT$fieldHt <- plots_merged$Herb_ht2
plots_LT$treeCover <- plots_merged$treeCover
plots_LT$shrubCover <- plots_merged$Shrub_cover
plots_LT$fieldCover <- plots_merged$Herb_cover
plots_LT$dominantStratum <- plots_merged$DomLayer
