# this script uploads new plot observations and related data (community classifications, taxon observations, etc)

library(vegbankr)
library(dplyr)

vb_set_base_url("https://api-dev.vegbank.org")

# read in loader tables
out_dir <- "data/loader-tables"

plots <- read.csv(file.path(out_dir, "plotsLT.csv"))
projects <- read.csv(file.path(out_dir, "projectLT.csv"))
party <- read.csv(file.path(out_dir, "partyLT.csv"))
contrib <- read.csv(file.path(out_dir, "contributorLT.csv"))
dist <- read.csv(file.path(out_dir, "disturbanceLT.csv"))
comm <- read.csv(file.path(out_dir, "communityClassificationsLT.csv"))
strat <- read.csv(file.path(out_dir, "strataCoverLT.csv"))
tax <- read.csv(file.path(out_dir, "taxonInterpretationsLT.csv"))
strat_defs <- read.csv(file.path(out_dir, "strataDefinitionsLT.csv"))
soils <- read.csv(file.path(out_dir, "soilLT.csv"))

# filter out rows that don't conform to requirements
# best to examine why rows were dropped before proceeding to upload step to ensure
# no errors in processing workflow
proj_code <- unique(projects$user_pj_code)

contrib_semi <- contrib %>% 
  filter(!is.na(vb_ar_code))

plots_semi <- plots %>% 
  filter(user_pj_code %in% proj_code)

plot_obs_code <- unique(plots_semi$user_ob_code)

dist_semi <- dist %>% filter(user_ob_code %in% plot_obs_code) # all dist need obs code, check with CDFW
comm_semi <- comm %>% filter(user_ob_code %in% plot_obs_code) %>% 
  filter(!is.na(vb_cc_code)) 

strat_defs_semi <- strat_defs %>% 
  filter(!is.na(vb_sy_code)) %>% 
  filter(user_ob_code %in% plot_obs_code)

strat_meth <- unique(strat_defs_semi$user_sr_code)

strat_semi <- strat %>% filter(user_ob_code %in% plot_obs_code) %>% 
  filter(!is.na(author_plant_name)) %>% 
  filter(user_sr_code %in% strat_meth)

tax_code <- unique(strat_semi$user_to_code)
tax_semi <- tax %>% 
  filter(!is.na(vb_pc_code)) %>% 
  filter(!is.na(vb_ar_code)) %>% 
  filter(user_to_code %in% tax_code)

# validate filtered tables
results <- vb_validate_plot_observations(plot_observations = plots_semi,
                            projects = projects,
                            parties = party,
                            contributors = contrib_semi,
                            disturbances = dist_semi,
                            community_classifications = comm_semi,
                            strata_cover_data = strat_semi,
                            taxon_interpretations = tax_semi,
                            strata = strat_defs_semi,
                            soils = soils)
# set token
vb_set_token(tokens = token)

# upload
vb_upload_plot_observations(plot_observations = plots_semi,
                            projects = projects,
                            parties = party,
                            contributors = contrib_semi,
                            disturbances = dist_semi,
                            community_classifications = comm_semi,
                            strata_cover_data = strat_semi,
                            taxon_interpretations = tax_semi,
                            strata = strat_defs_semi,
                            soils = soils,
                            dry_run = TRUE)


