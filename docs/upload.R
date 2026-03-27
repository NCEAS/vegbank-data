library(vegbankr)
library(dplyr)
vb_set_base_url("https://api-dev.vegbank.org")
vb_set_token(tokens = token)


## plots

plots <- read.csv("data/loader-tables/plotsLT.csv")
projects <- read.csv("data/loader-tables/projectLT.csv") 
party <- read.csv("data/loader-tables/partyLT.csv") 
contrib <- read.csv("data/loader-tables/contributorLT.csv")
dist <- read.csv("data/loader-tables/disturbanceLT.csv")
comm <- read.csv("data/loader-tables/communityClassificationsLT.csv")
strat <- read.csv("data/loader-tables/strataCoverLT.csv") 
tax <- read.csv("data/loader-tables/taxonInterpretationsLT.csv")
strat_defs <- read.csv("data/loader-tables/strataDefinitionsLT.csv") 
soils <- read.csv("data/loader-tables/soilLT.csv")

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

vb_upload_plot_observations(plot_observations = plots,
                            projects = projects,
                            parties = party,
                            contributors = contrib,
                            disturbances = dist,
                            community_classifications = comm,
                            strata_cover_data = strat,
                            taxon_interpretations = tax,
                            strata = strat_defs,
                            soils = soils,
                            dry_run = TRUE)


## community concepts

community_concepts <- read.csv("data/loader-tables/communityConceptsLT.csv")
community_names <- read.csv("data/loader-tables/communityNamesLT.csv")

ref <- data.frame(user_rf_code = "MCV - CDFW CNPS",
                  short_name = "MCV - CDFW CNPS",
                  full_citation = "CNPS. 2026. A Manual of California Vegetation, Online Edition. http://www.cnps.org/cnps/vegetation/; searched on 2026-02-06.",
                  url = "http://www.cnps.org/cnps/vegetation/")

party <- data.frame(user_py_code = "CDFW CNPS",
                    organization_name = "California Native Plant Society (CNPS) and California Department of Fish and Wildlife (CDFW)")

vb_upload("community-concepts",
          community_concepts = community_concepts,
          community_names = community_names,
          references = ref,
          parties = party,
          dry_run = TRUE)

