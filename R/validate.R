library(dplyr)
library(readr)
library(pointblank)
library(cli)
library(stringr)

fail_if_invalid <- function(agent, label) {
  
  agent <- interrogate(agent)
  
  if (!all_passed(agent)) {
    cli::cli_abort(c(
      "Validation failed for {.val {label}}.",
      "x" = "One or more validation checks did not pass."
    ))
  }
  
  invisible(agent)
}

validate_and_write <- function(out_dir = "data/loader-tables/"){
  
  plots <- read_csv(file.path(out_dir, "plotsLT.csv"), show_col_types = FALSE)
  projects <- read_csv(file.path(out_dir, "projectLT.csv"), show_col_types = FALSE)
  party <- read_csv(file.path(out_dir, "partyLT.csv"), show_col_types = FALSE)
  contrib <- read_csv(file.path(out_dir, "contributorLT.csv"), show_col_types = FALSE)
  dist <- read_csv(file.path(out_dir, "disturbanceLT.csv"), show_col_types = FALSE)
  comm <- read_csv(file.path(out_dir, "communityClassificationsLT.csv"), show_col_types = FALSE)
  
  strat <- read_csv(file.path(out_dir, "strataCoverLT.csv"), show_col_types = FALSE) %>%
    select(where(~ !all(is.na(.)))) %>%
    mutate(user_tm_code = user_to_code)
  
  tax <- read_csv(file.path(out_dir, "taxonInterpretationsLT.csv"), show_col_types = FALSE) %>%
    select(where(~ !all(is.na(.))))
  
  strat_defs <- read_csv(file.path(out_dir, "strataDefinitionsLT.csv"), show_col_types = FALSE)
  
  # plots -> projects
  
  fail_if_invalid(
    create_agent(plots) |>
      col_vals_not_null(vars(user_pj_code, user_ob_code)) |>
      col_vals_in_set(
        vars(user_pj_code),
        set = projects$user_pj_code
      ),
    "plotsLT vs projectLT"
  )
  
  proj_code <- unique(projects$user_pj_code)
  plots_semi <- plots %>% filter(user_pj_code %in% proj_code) # all plots need project code, check with CDFW
  
  # disturbance -> plots
  
  plot_obs_code <- unique(plots_semi$user_ob_code)
  
  fail_if_invalid(
    create_agent(dist) |>
      col_vals_not_null(vars(user_ob_code)) |>
      col_vals_in_set(
        vars(user_ob_code),
        set = plot_obs_code
      ),
    "disturbanceLT vs plotsLT"
  )
  
  dist_semi <- dist %>% filter(user_ob_code %in% plot_obs_code) # all dist need obs code, check with CDFW
  
  # community -> plots
  
  fail_if_invalid(
    create_agent(comm) |>
      col_vals_not_null(vars(user_ob_code, vb_cc_code)) |>
      col_vals_in_set(
        vars(user_ob_code),
        set = plot_obs_code
      ),
    "communityClassificationsLT vs plotsLT"
  )
  
  comm_semi <- comm %>%
    filter(user_ob_code %in% plot_obs_code) %>%
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