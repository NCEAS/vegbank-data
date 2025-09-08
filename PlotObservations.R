library(tidyverse)
library(here)

# load in CDFW data -----------------------------------------------------------

# set folder where data is saved
folder <- 'data'

# RAPlots
# Column 84 was set to character due to parsing issue
plots <- read_csv(here(folder, 'RAPlots.csv'), 
                  col_types = cols(.default = col_guess(), 
                                   `PlotOther5` = col_character()))

# loading CA lookup tables
confidentiality_lookup <- read_csv(here(folder, 'LConfidentiality.csv'))
height_lookup <- read_csv(here(folder, 'LHeight.csv'))
standsize_lookup <- read_csv(here(folder, 'LStandSize.csv'))
substrate_lookup <- read_csv(here(folder, 'LSubstrate.csv'))

# creating loader table -------------------------------------------------------

# getting number of rows
nrow <- nrow(plots)

# create blank data frame
plots_LT <- data.frame(pl_code = rep(NA, nrow),
                       author_plot_code = rep(NA, nrow),
                       parent_pl_code = rep(NA, nrow),
                       real_latitude = rep(NA, nrow),
                       real_longitude = rep(NA, nrow),
                       location_accuracy = rep(NA, nrow),
                       confidentiality_status = rep(NA, nrow),
                       confidentiality_reason = rep(NA, nrow),
                       author_e = rep(NA, nrow),
                       author_n = rep(NA, nrow),
                       author_zone = rep(NA, nrow),
                       author_datum = rep(NA, nrow),
                       author_location = rep(NA, nrow),
                       location_narrative = rep(NA, nrow),
                       azimuth = rep(NA, nrow),
                       dsgpoly = rep(NA, nrow),
                       shape = rep(NA, nrow),
                       area = rep(NA, nrow),
                       stand_size = rep(NA, nrow),
                       placement_method = rep(NA, nrow),
                       permanence = rep(NA, nrow),
                       layout_narrative = rep(NA, nrow),
                       elevation = rep(NA, nrow),
                       elevation_accuracy = rep(NA, nrow),
                       elevation_range = rep(NA, nrow),
                       slope_aspect = rep(NA, nrow),
                       min_slope_aspect = rep(NA, nrow),
                       max_slope_aspect = rep(NA, nrow),
                       slope_gradient = rep(NA, nrow),
                       min_slope_gradient = rep(NA, nrow),
                       max_slope_gradient = rep(NA, nrow),
                       topo_position = rep(NA, nrow),
                       landform = rep(NA, nrow),
                       surficial_deposits = rep(NA, nrow),
                       rock_type = rep(NA, nrow),
                       quadrangleName = rep(NA, nrow),
                       county = rep(NA, nrow),
                       stateProvince = rep(NA, nrow),
                       country = rep(NA, nrow),
                       continent = rep(NA, nrow),
                       ob_code = rep(NA, nrow),
                       pj_code = rep(NA, nrow),
                       authorObsCode = rep(NA, nrow),
                       parent_ob_code = rep(NA, nrow),
                       obsStartDate = rep(NA, nrow),
                       obsEndDate = rep(NA, nrow),
                       dateAccuracy = rep(NA, nrow),
                       cm_code = rep(NA, nrow),
                       coverDispersion = rep(NA, nrow),
                       autoTaxonCover = rep(NA, nrow),
                       sm_code = rep(NA, nrow),
                       stratumIndex1 = rep(NA, nrow),
                       stratumHeight1 = rep(NA, nrow),
                       stratumBase1 = rep(NA, nrow),
                       stratumCover1 = rep(NA, nrow),
                       eightMoreStratumSets = rep(NA, nrow),
                       methodNarrative = rep(NA, nrow),
                       taxonObservationArea = rep(NA, nrow),
                       stemSizeLimit = rep(NA, nrow),
                       stemObservationArea = rep(NA, nrow),
                       stemSampleMethod = rep(NA, nrow),
                       originalData = rep(NA, nrow),
                       effortLevel = rep(NA, nrow),
                       floristicQuality = rep(NA, nrow),
                       bryophiteQuality = rep(NA, nrow),
                       lichenQuality = rep(NA, nrow),
                       observationNarrative = rep(NA, nrow),
                       landscapeNarrative = rep(NA, nrow),
                       homogeneity = rep(NA, nrow),
                       phenologicAspect = rep(NA, nrow),
                       standMaturity = rep(NA, nrow),
                       successionalStatus = rep(NA, nrow),
                       basalArea = rep(NA, nrow),
                       hydrologicRegime = rep(NA, nrow),
                       soilMoistureRegime = rep(NA, nrow),
                       soilDrainage = rep(NA, nrow),
                       waterSalinity = rep(NA, nrow),
                       waterDepth = rep(NA, nrow),
                       shoreDistance = rep(NA, nrow),
                       soilDepth = rep(NA, nrow),
                       organicDepth = rep(NA, nrow),
                       st_code = rep(NA, nrow),
                       soilTaxonSrc = rep(NA, nrow),
                       percentBedRock = rep(NA, nrow),
                       percentRockGravel = rep(NA, nrow),
                       percentWood = rep(NA, nrow),
                       percentLitter = rep(NA, nrow),
                       percentBareSoil = rep(NA, nrow),
                       percentWater = rep(NA, nrow),
                       percentOther = rep(NA, nrow),
                       nameOther = rep(NA, nrow),
                       treeHt = rep(NA, nrow),
                       shrubHt = rep(NA, nrow),
                       fieldHt = rep(NA, nrow),
                       nonvascularHt = rep(NA, nrow),
                       submergedHt = rep(NA, nrow),
                       treeCover = rep(NA, nrow),
                       shrubCover = rep(NA, nrow),
                       fieldCover = rep(NA, nrow),
                       nonvascularCover = rep(NA, nrow),
                       floatingCover = rep(NA, nrow),
                       submergedCover = rep(NA, nrow),
                       dominantStratum = rep(NA, nrow),
                       growthform1Type = rep(NA, nrow),
                       growthform2Type = rep(NA, nrow),
                       growthform3Type = rep(NA, nrow),
                       growthform1Cover = rep(NA, nrow),
                       growthform2Cover = rep(NA, nrow),
                       growthform3Cover = rep(NA, nrow),
                       totalCover = rep(NA, nrow))

# checking values -------------------------------------------------------------


                       