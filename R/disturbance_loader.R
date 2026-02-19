library(tidyverse)

disturbance_loader <- function(in_dir, out_dir){
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  # read in RAImpacts
  impact_files <- dir(sub_folders, full.names = TRUE) %>% 
    grep(pattern = "RAImpacts.csv", value = TRUE)
  
  impacts_df_list <- lapply(impact_files, read_csv, progress = FALSE, show_col_types = FALSE)
  impacts <- do.call(bind_rows, impacts_df_list)

  
  impacts_lookup <- read_csv(file.path(in_dir, "lookup-tables/LImpacts.csv"), progress = FALSE, show_col_types = FALSE)
  
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
  
  
  lookup_disturbance <- tibble::tribble(
    ~`Impact type`,                                   ~vegbank_disturbance,
    "Development",                            "Human, general",
    "ORV activity",                           "Roads and vehicular traffic",
    "Agriculture",                            "Cultivation",
    "Grazing",                                "Grazing, domestic stock", # several grazing options
    "Competition from exotics",               "Other disturbances",
    "Logging",                                "Timber harvest, general", # several timber harvest options
    "Insufficient population/stand size",     "Other disturbances",
    "Altered flood/tidal regime",             "Hydrologic alteration",
    "Mining",                                 "Human, general",
    "Hybridization",                          "Other disturbances",
    "Groundwater pumping",                    "Hydrologic alteration",
    "Dam/inundation",                         "Hydrologic alteration",
    "Other",                                  "Other disturbances",
    "Surface water diversion",                "Hydrologic alteration",
    "Road/trail construction/maint.",         "Roads and vehicular traffic",
    "Biocides",                               "Herbicide or chemical",
    "Pollution",                              "Human, general",
    "Unknown",                                "unknown",
    "Vandalism/dumping/litter",               "Human, general",
    "Foot traffic/trampling",                 "Trampling and trails",
    "Improper burning regime",                "Fire suppression",
    "Over collecting/poaching",               "Human, general",
    "Erosion/runoff",                         "Erosion",
    "Altered thermal regime",                 "Other disturbances",
    "Landfill",                               "Human, general",
    "Degrading water quality",                "Hydrologic alteration",
    "Wood cutting",                           "Timber harvest, selective", # several timer harvest options
    "Military operations",                    "Human, general",
    "Recreational use (non ORV)",              "Trampling and trails",
    "Nest parasitism",                        "Natural, general",
    "Non-native predators",                   "Other disturbances",
    "Rip-rap, bank protection",               "Hydrologic alteration",
    "Channelization (human caused)",           "Hydrologic alteration",
    "Feral pigs",                             "Animal, general",
    "Burros",                                 "Animal, general", # or one of the grazings?
    "Rills",                                  "Erosion",
    "Phytogenic mounding",                    "Erosion",
    "Sudden Oak Death (SODS)",                 "Plant disease",
    NA, "Unknown"
  )
  
  
  impacts_merged <- impacts %>% 
    # joining with lookup table by impact code
    left_join(impacts_lookup, by = c("CodeImpact" = "CodeImp")) %>% 
    left_join(lookup_disturbance, by = "Impact type") %>% 
    # changing numeric code
    mutate(Intensity = case_when(
      Intensity == 1 ~ "Low",
      Intensity == 2 ~ "Moderate",
      Intensity == 3 ~ "High"
    )) %>% 
    mutate(Other = paste(Other, `Impact type`, sep = "; ")) %>% 
    mutate(Other = gsub("NA; ", "", Other))
  
  disturb_LT <- impacts_merged %>%
    select(user_ob_code = SurveyID,
           type = vegbank_disturbance,
           comment = Other,
           intensity = Intensity) %>% 
    mutate(user_do_code = row_number())
  
  # save filled in loader table
  out_path <- file.path(out_dir, "disturbanceLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(disturb_LT, out_path)
}
