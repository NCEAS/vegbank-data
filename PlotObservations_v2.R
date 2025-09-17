library(tidyverse)
library(here)
library(stringr)

# load in CDFW data -------------------------------------------------------
# RAPlots
csv_path <- here("data", "RAPlots.csv")
plots <- read_csv(csv_path, show_col_types = FALSE)
#AltPlots
csv_path <- here("data", "AltPlots.csv")
alt_plots <- read_csv(csv_path, show_col_types = FALSE)

# loading CA lookup tables
# MacroTopo
csv_path <- here("data", "LMacroTopo.csv")
macrotopo_lookup <- read_csv(csv_path, show_col_types = FALSE)
# Substrate
csv_path <- here("data", "LSubstrate.csv")
substrate_lookup <- read_csv(csv_path, show_col_types = FALSE)
# Stand Size
csv_path <- here("data", "LStandSize.csv")
standsize_lookup <- read_csv(csv_path, show_col_types = FALSE)
# Height
csv_path <- here("data", "LHeight.csv")
height_lookup <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------
# create a header csv with only variable names
csv_path <- here("loader_tables", "PlotObservations_Header.csv")
template <- read_csv(csv_path, show_col_types = FALSE)

# Checking values ---------------------------------------------------------

# Survey date should take the form "MM/DD/YY hh:mm:ss"
# all values take the correct form.

ProjectStartDate_chr = str_squish(as.character(plots$SurveyDate))

date_pattern <- "^(0[1-9]|1[0-2])/([0-2][0-9]|3[0-1])/\\d{2} ([0-1][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"

invalid_dates <- plots %>%
  filter(!str_detect(SurveyDate, date_pattern))

unique(invalid_dates$SurveyDate)

# making sure values that should be from L* tables are found in L* tables
# MacroTopo contains some values that are not in the lookup tables. How will we handle these data?
# Substrate and Stand Size values are all valid.

# MacroTopo
plots %>%
  filter(!MacroTopo %in% macrotopo_lookup$MacroTopoValue) %>%
  distinct(MacroTopo)

# Substrate
plots %>%
  filter(!Substrate %in% substrate_lookup$Substrate) %>%
  distinct(Substrate)

# Stand Size
plots %>%
  filter(!Stand_Size %in% standsize_lookup$Stand_Size) %>%
  distinct(Stand_Size)

# Upl_Wet_txt should either be wetland/riparian or upland.
# All values are valid

unique(plots$Upl_Wet_text)

# PlotShape should be one of "circle", "square", "rectangle", "linear", and "triangle".
# some values for PlotShape are dimensions (such as "20 m x 5 m"). How will we handle these values?

unique(plots$PlotShape)

# Tidying CDFW data -------------------------------------------------------
# Converting elevation values that are in ft to meter.

# Checking unit values used in CDFW data
unique(plots$ft_mElevation)
unique(plots$ErrorUnits)

plots <- plots %>%
  mutate(
    elevation_m = case_when(
      ft_mElevation == 'm'                               ~ Elevation,
      ft_mElevation %in% c("ft.", "ft", "F")             ~ Elevation * 0.3048,
      is.na(ft_mElevation)                               ~ NA_real_
      ),
    error_m = case_when(
      ErrorUnits == 'm'                                  ~ ErrorMeasurement,
      ErrorUnits %in% c("ft.", "ft", "F")                ~ ErrorMeasurement * 0.3048,
      is.na(ErrorUnits)                                  ~ NA_real_,
      TRUE                                               ~ NA_real_
    ),
    # Combining percent covers
    # percentRockGravel
    RockGravel = rowSums(cbind(Boulders, Stones, Cobbles, Gravels)),
    # treeCover (unsure whether there is any overlap in hdwd, conif, and regen trees)
    tree_cover = rowSums(cbind(Hdwd_cover, Conif_cover, RegenTree_cover))
  )

#AltStrata table also has contains data that can map to PlotObservations.
plots_combined <- left_join(plots, alt_plots, by = "SurveyID")

# Assigning columns to loader table ---------------------------------------
# Changing column types to support to data (probably a more efficient way to do this)
template <- template %>%
  mutate(
    real_latitude = as.numeric(real_latitude),
    real_longitude = as.numeric(real_longitude),
    location_accuracy = as.numeric(location_accuracy),
    confidentiality_status = as.numeric(confidentiality_status),
    author_e = as.numeric(author_e),
    author_n = as.numeric(author_n),
    author_zone = as.numeric(author_zone),
    azimuth = as.numeric(azimuth),
    elevation = as.numeric(elevation),
    slope_aspect = as.numeric(slope_aspect),
    slope_gradient = as.numeric(slope_gradient),
    treeCover = as.numeric(treeCover),
    shrubCover = as.numeric(shrubCover),
    fieldCover = as.numeric(fieldCover)
    
  )

PlotObs_LT <- bind_rows(
  template,
  tibble(author_plot_code = plots_combined$SurveyID,
         real_latitude = plots_combined$Latitude_WGS84_Final,
         real_longitude = plots_combined$Longitude_WGS84_Final,
         location_accuracy = plots_combined$ErrorMeasurement,
         confidentiality_status = plots_combined$ConfidentialityStatus,
         author_e = plots_combined$UTME,
         author_n = plots_combined$UTMN,
         author_zone = plots_combined$UTM_zone,
         author_datum = plots_combined$GPS_datum,
         author_location = plots_combined$SiteLocation,
         azimuth = plots_combined$W_Axis_Bearing,
         shape = plots_combined$PlotShape,
         area = plots_combined$PlotArea,
         stand_size = plots_combined$Stand_Size,
         elevation = plots_combined$elevation_m,
         slope_aspect = plots_combined$Aspect_actual,
         slope_gradient = plots_combined$Slope_actual,
         topo_position = plots_combined$MacroTopo,
         rock_type = plots_combined$Substrate,
         pj_code = plots_combined$ProjectCode,
         obsStartDate = plots_combined$SurveyDate,
         successionalStatus = plots_combined$Trend,
         hydrolicRegime = plots_combined$Upl_Wet_text,
         shrubHt = plots_combined$Shrub_ht2,
         fieldHt = plots_combined$Herb_ht2,
         treeCover = plots_combined$tree_cover,
         shrubCover = plots_combined$Shrub_cover,
         fieldCover = plots_combined$Herb_cover,
         dominantStratum = plots_combined$DomLayer
  )
)

# things to adjust: dominant stratum takes in only tree, shrub, groun. Needs tree, shrub, ground, novasc, floating, submerged.
# Rest of Variables are unmatched