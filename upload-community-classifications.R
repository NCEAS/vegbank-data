# this script uploads new community classifications for which plot observations already exist in vegbank

library(vegbankr)
library(dplyr)

# load in R functions
file_path <- "R"
r_scripts_source <- list.files(file_path, recursive = FALSE, full.names = TRUE, pattern = "\\.[Rr]$")
invisible(lapply(r_scripts_source, source))

# read in community classifications (generated from example-workflow.R)
out_dir <- "data/loader-tables"
comm <- read.csv(file.path(out_dir, "communityClassificationsLT.csv"))

# check that all user_ob_code (SurveyIDs) are in VegBank and get the vegbank codes for each plot
existing <- check_existing_plots(comm, renew_cache = FALSE, out_dir) %>% 
  select(user_ob_code,
         vb_ob_code = ob_code) %>% 
  group_by(user_ob_code) %>% 
  slice_head(n = 1)

if (any(!(comm$user_ob_code %in% existing$user_ob_code))){
  cli::cli_alert_danger("Some plots in communityClassificationsLT.csv are not already in VegBank. See example-workflow.R and upload-plot-observations.R to upload them.")
}

if (any(!is.na(comm$vb_cc_code))){
  cli::cli_alert_danger("Some plots did not have a vegbank community classification code assigned (vb_cc_code). This field is required for ingest into VegBank. See community_loader.R.")
}

# assign vb_ob_code to community classifications, drop user_ob_code
community_classifications <- left_join(comm, existing, by = join_by(user_ob_code)) %>% 
  filter(!is.na(vb_ob_code)) %>% # WARN: drops rows without an existing vb ob code.
  filter(!is.na(vb_cc_code)) %>% # WARN: drops rows without vb cc code
  select(-user_ob_code)

# set token and upload
vb_set_token(tokens = token)
vb_upload_community_classifications(community_classifications = community_classifications,
                                    dry_run = TRUE)
