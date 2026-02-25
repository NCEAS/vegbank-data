validate_and_write <- function(out_dir = "data/loader-tables/"){
  plots <- read.csv(file.path(out_dir, "plotsLT.csv"))
  projects <- read.csv(file.path(out_dir, "projectLT.csv"))
  party <- read.csv(file.path(out_dir, "partyLT.csv"))
  contrib <- read.csv(file.path(out_dir, "contributorLT.csv"))
  dist <- read.csv(file.path(out_dir, "disturbanceLT.csv"))
  comm <- read.csv(file.path(out_dir, "communityClassificationsLT.csv"))
  strat <- read.csv(file.path(out_dir, "strataCoverLT.csv")) %>%
    select(where(~ !all(is.na(.)))) %>%
    mutate(user_tm_code = user_to_code)
  tax <- read.csv(file.path(out_dir, "taxonInterpretationsLT.csv")) %>%
    select(where(~ !all(is.na(.))))
  strat_defs <- read.csv(file.path(out_dir, "strataDefinitionsLT.csv"))
  
  
  proj_code <- unique(projects$user_pj_code)
  plots_semi <- plots %>% filter(user_pj_code %in% proj_code) # all plots need project code, check with CDFW
  plot_obs_code <- unique(plots_semi$user_ob_code)
  dist_semi <- dist %>% filter(user_ob_code %in% plot_obs_code) # all dist need obs code, check with CDFW
  comm_semi <- comm %>% filter(user_ob_code %in% plot_obs_code) %>% 
    filter(!is.na(vb_cc_code)) %>% 
    select(where(~!all(is.na(.))))
  

  strat_defs_semi <- strat_defs %>% 
    filter(!is.na(vb_sy_code)) %>% 
    filter(user_ob_code %in% plot_obs_code)
  strat_meth <- unique(strat_defs_semi$user_sr_code)
  
  strat_semi <- strat %>% filter(user_ob_code %in% plot_obs_code) %>% 
    filter(!is.na(author_plant_name)) %>% 
    filter(user_sr_code %in% strat_meth)
  
  tax_code <- unique(strat_semi$user_to_code)
  tax_semi <- tax %>% 
    filter(!is.na(vb_pc_code) & !is.na(vb_ro_code)) %>% 
    filter(user_to_code %in% tax_code)
  
  write.csv(plots_semi, file.path(out_dir, "plotsLT.csv"))
  write.csv(dist_semi, file.path(out_dir, "disturbanceLT.csv"))
  write.csv(comm_semi, file.path(out_dir, "communityClassificationsLT.csv"))
  write.csv(strat_defs_semi, file.path(out_dir, "strataDefinitionsLT.csv"))
  write.csv(strat_semi, file.path(out_dir, "strataCoverLT.csv"))
  write.csv(tax_semi, file.path(out_dir, "taxonInterpretationsLT.csv"))
  
}