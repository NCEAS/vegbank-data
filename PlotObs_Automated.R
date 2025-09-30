library(tidyverse)
library(here)
library(googlesheets4)

# Personal Notes (Will Delete):
# vb_pl_code: no mapping yet
# user_pl_code: no mapping yet
# author_plot_code: AltPlots' SurveyID can be left-joined.
#                   AltStrata's SurveyID can be left-joined.
#                   SurveyPoints is empty?
# vb_parent_pl_code: no mapping yet
# user_parent_pl_code: no mapping yet
# real_latitude: AltPlots' Latitude_WGS84_Final
# real_longitude: AltPlots' Longitude_WGS84_Final
# location_accuracy: SurveyPoints' ErrorMeasurement; but it is empty?
# confidentiality_status: RAPlots' ConfidentialityStatus
# confidentiality_reason: no mapping yet
# author_e: SurveyPoints' UTM_E; but it is empty?
# author_n: SurveyPoints' UTM_N; but it is empty?
# author_zone: SurveyPoints' UTM_zone; but it is empty?
# author_datum: no mapping yet
# author_location: no mapping yet
# location_narrative:

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

# creating loader table -------------------------------------------------------
cache_dir <- file.path(Sys.getenv("HOME"), ".vegbank_gs_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
options(gargle_oauth_cache = cache_dir)

gs4_auth(cache = TRUE,
         scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")  # first time: choose account; later runs: silent

sheet_url <- "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393"
vars <- read_sheet(sheet_url, sheet = "PlotObservations", range = "C1:C", col_names = FALSE)

fields <- vars[[1]] %>% 
  tail(-1) %>%
  unique() %>% 
  discard(~ is.na(.x) || str_squish(.x) == "") %>% 
  str_squish()


# create blank data frame
plots_LT <- as_tibble(
  setNames(
    lapply(fields, \(.) rep(NA_character_, nrow(plots))),
    fields
  )
)
gargle::gargle_oauth_sitrep()

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

# ErrorMeasurement (RAPlots) + ErrorUnits (RAPlots) - location_accuracy (plots)
# Numbers should be converted to their unit of measurement as stated in
# ErrorUnits
unique(plots$ErrorMeasurement)
unique(plots$ErrorUnits)
class(plots$ErrorMeasurement) # numeric
class(plots$ErrorUnits) # character
# ErrorUnits also contains PDOP and Laptop. What to do with those?

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

# When assigning columns to loader table, column types are all changed to
# numeric.

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

# SurveyDate (RAPlots) - obsStartDate (plots)
# Time should be removed
plots_merged$SurveyDate <- as_date(mdy_hms(plots$SurveyDate))

# Elevation (RAPlots) related to ft_mElevation (RAPlots) - elevation (plots)
# Numbers should be converted to meters if the unit in ft_mElevation says ft.
plots_merged <- plots_merged %>% 
  mutate(
    Elevation = case_when(
      ft_mElevation %in% c("F", "ft", "ft.") ~ Elevation * 0.3048,
      TRUE ~ Elevation
    )
  )

# Substrate (RAPlots) - rock_type (plots)
# GitHub Issue #3 needs more clarification

# Aspect_actual (RAPlots) to Aspect_gen (RAPlots) - slope_aspect (plots)
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

# Slope_actual (RAPlots) to Slope_gen (RAPlots) - slope_gradient (plots)
# Code -1 if irregular to determine
# GitHub Issue TBD

# Boulders/Stones/Cobbles/Gravels (RAPlots) - percentRockGravel (plots)
# Need to combine 4 columns into one
plots_merged <- plots_merged %>% 
  mutate(
    percentRockGravel = rowSums(cbind(Boulders, Stones, Cobbles, Gravels))
  )

# Conif_cover/Hdwd_cover/RegenTree_cover (RAPlots) - treeCover (plots)
# Need to combine 3 columns into one
plots_merged <- plots_merged %>% 
  mutate(
    treeCover = rowSums(cbind(Hdwd_cover, Conif_cover, RegenTree_cover))
  )

# PlotArea (RAPlots) - area (plots)
# Units will be removed
plots_merged <- plots_merged %>% 
  mutate(PlotArea = str_remove(PlotArea, 
                               " ?(m²|sq\\. ?m|sp\\. ?M|sq ?m|sq\\.? ?M)"))
# There is a value here that looks like "~700". Should we remove the "~"?

# -1 indicates plot has no boundaries
# Also, combine PlotArea, ViewRadius, and SurveyDimensions together into area?
# 5 parsing failures
plots_merged <- plots_merged %>%
  mutate(PlotArea_num = parse_number(as.character(PlotArea, na = c("NA","na","Not recorded","not recorded"))),
         dims = str_extract_all(as.character(SurveyDimensions), "\\d+(?:\\.\\d+)?"),
         SurveyLength = suppressWarnings(as.numeric(map_chr(dims, 1, .default = NA))),
         SurveyWidth  = suppressWarnings(as.numeric(map_chr(dims, 2, .default = NA))),
         area_from_radius = if_else(!is.na(ViewRadius), pi * (as.numeric(ViewRadius)^2), NA_real_),
         area_from_dims   = if_else(!is.na(SurveyLength) & !is.na(SurveyWidth),
                                    SurveyLength * SurveyWidth, NA_real_),
         PlotArea = coalesce(PlotArea_num, area_from_radius, area_from_dims, -1)
  )

# PlotShape (RAPlots) - shape (plots)
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
plots_merged$shape

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

# ErrorMeasurement + ErrorUnits (RAPlots) - location_accuracy (plots)
# Numbers should be converted to their unit of measurement in ErrorUnits
plots_merged <- plots_merged %>% 
  mutate(
    ErrorMeasurement = case_when(
      ErrorUnits %in% c("F", "ft", "ft.") ~ ErrorMeasurement * 0.3048,
      TRUE ~ ErrorMeasurement
    )
  )


# Assigning columns to loader table -------------------------------------------
plots_LT$author_plot_code <- plots_merged$SurveyID
plots_LT$real_latitude <- plots_merged$Latitude_WGS84_Final
plots_LT$real_longitude <- plots_merged$Longitude_WGS84_Final
plots_LT$location_accuracy <- plots_merged$ErrorMeasurement
plots_LT$confidentiality_status <- plots_merged$ConfidentialityStatus
plots_LT$author_e <- plots_merged$UTME
plots_LT$author_n <- plots_merged$UTMN
plots_LT$author_zone <- plots_merged$UTM_zone
plots_LT$author_datum <- plots_merged$GPS_datum
plots_LT$author_location <- plots_merged$SiteLocation
plots_LT$azimuth <- plots_merged$W_Axis_Bearing
plots_LT$shape <- plots_merged$PlotShape
plots_LT$area <- plots_merged$PlotArea
plots_LT$stand_size <- plots_merged$Stand_Size
plots_LT$elevation <- plots_merged$Elevation
plots_LT$slope_aspect <- plots_merged$Aspect_actual
plots_LT$slope_gradient <- plots_merged$Slope_actual
plots_LT$topo_position <- plots_merged$MacroTopo
plots_LT$rock_type <- plots_merged$Substrate
plots_LT$pj_code <- plots_merged$ProjectCode
plots_LT$obsStartDate <- plots_merged$SurveyDate
plots_LT$successionalStatus = plots_merged$Trend
plots_LT$hydrolicRegime <- plots_merged$Upl_Wet_text
plots_LT$shrubHt <- plots_merged$Shrub_ht2
plots_LT$fieldHt <- plots_merged$Herb_ht2
plots_LT$treeCover <- plots_merged$treeCover
plots_LT$shrubCover <- plots_merged$Shrub_cover
plots_LT$fieldCover <- plots_merged$Herb_cover
plots_LT$dominantStratum <- plots_merged$DomLayer

