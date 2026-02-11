library(tidyverse)
library(stringr)
library(googlesheets4)
library(cli)
library(glue)
source("R/build_loader_table.R")

# load in CDFW data -----------------------------------------------------------

load_community_files <- function(in_dir) {
  
  sub_folders <- dir(in_dir, full.names = TRUE) %>%
    grep(pattern = "VegBankProject", value = TRUE)
  
  # files needed for CommunityClassifications
  plot_files <- dir(sub_folders, full.names = TRUE) %>%
    grep(pattern = "RAPlots.csv", value = TRUE)
  
  classification_files <- dir(sub_folders, full.names = TRUE) %>%
    grep(pattern = "RAClassification.csv", value = TRUE)
  
  project_files <- dir(sub_folders, full.names = TRUE) %>%
    grep(pattern = "RAProjects.csv", value = TRUE)
  
  # read + combine
  plots_df_list <- lapply(
    plot_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE,
    col_types = cols(`PalmJoshua` = col_character(),
                     `DesertRip` = col_character()),
    guess_max = 20000
  )
  plots <- do.call(bind_rows, plots_df_list)
  
  classification_df_list <- lapply(
    classification_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE,
    guess_max = 20000
  )
  classification <- do.call(bind_rows, classification_df_list)
  
  project_df_list <- lapply(
    project_files,
    read_csv,
    progress = FALSE,
    show_col_types = FALSE
  )
  projects <- do.call(bind_rows, project_df_list) %>%
    group_by(ProjectCode) %>% 
    arrange(desc(nchar(ProjectDescription))) %>% 
    slice(1)
  
  # create blank Loader Table dataframe -----------------------------------------------------
  community_template_fields <- build_loader_table(
    sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
    sheet = "CommunityClassifications",
    source_df = classification
  )
  
  community_LT <- community_template_fields$template
  
  plots <- plots
  classification <- classification
  projects <- projects
  community_LT <- community_LT
  
  out <- list("plots" = plots, "classification" = classification, "projects" = projects, "community_LT" = community_LT)
  
  return(out)
}

normalize_projects_classification <- function(projects, in_dir) {
  
  method_lookup <- read.csv(paste0(in_dir, "/lookup-tables/classification-methods-20260202.csv")) %>% 
    select(-ClassificationDescription, -ClassificationTool)
  
  projects_proj <- projects %>% 
    left_join(method_lookup, by = join_by(ProjectCode)) %>% 
    mutate(inspection = if_else(grepl("inspection", technique), TRUE, FALSE),
           multivariate_analysis = if_else(grepl("multiVariateAnalysis", technique), TRUE, FALSE),
           table_analysis = if_else(grepl("tableAnalysis", technique), TRUE, FALSE)) %>% 
    mutate(across(c(inspection, multivariate_analysis, table_analysis), ~ if_else(is.na(technique), NA, .))) %>% 
    mutate(across(c(ClassificationDescription, ClassificationTool), ~ if_else(is.na(.x), "", .x))) %>% 
    mutate(class_notes = paste(ClassificationDescription, ClassificationTool, sep = ", ")) %>% 
    mutate(class_notes = if_else(class_notes == ", ", NA, class_notes)) %>% 
    mutate(class_notes = gsub("^(,\\s*)|(,\\s*)$", "", class_notes)) %>% 
    group_by(ProjectCode) %>% 
    summarise(
      across(c(inspection, multivariate_analysis, table_analysis, class_notes), first),
      .groups = "drop"
    )
  
  # any ClassificationDescription that didn't match either bucket?
  unmatched <- projects_proj %>%
    filter(is.na(inspection) | is.na(table_analysis) | is.na(multivariate_analysis)) %>% 
    filter(!is.na(class_notes)) %>% 
    filter(!grepl("was not performed|Not formally classified", class_notes)) %>% 
    distinct(class_notes) %>%
    pull(class_notes)
  
  if (length(unmatched) > 0) {
    cli_alert_warning("Some Project ClassificationDescription/ClassificationTool values did not match patterns ({length(unmatched)} unique).")
    cli_text("Sample (up to 3):")
    cli_text(paste0("- ", str_trunc(head(unmatched, 3), 120)))
  }
  
  return(projects_proj)
}

# plots (class_confidence)

normalize_class_confidence <- function(plots) {
  conf_raw <- plots$Confidence_ID %>%
    as.character() %>%
    str_squish()
  
  conf_raw[conf_raw %in% c(
    "", "NA", "N/A", "<Null>",
    "Not recorded", "not recorded",
    "Not present", "not present",
    "not collected", NA
  )] <- NA_character_
  
  conf_pct <- suppressWarnings(
    as.numeric(str_remove(conf_raw, "%$"))
  )
  
  plots_conf <- plots %>%
    transmute(
      SurveyID,
      class_confidence = case_when(
        is.na(conf_raw) ~ NA_character_,
        
        conf_raw == "H" ~ "High",
        conf_raw == "M" ~ "Medium",
        conf_raw == "L" ~ "Low",
        
        conf_raw %in% c("High", "Medium", "Low") ~ conf_raw,
        
        !is.na(conf_pct) & conf_pct >= 75 ~ "High",
        !is.na(conf_pct) & conf_pct < 75  ~ "Medium",
        
        TRUE ~ NA_character_
      )
    )
  
  recognized <- is.na(conf_raw) |
    conf_raw %in% c("H","M","L","High","Medium","Low") |
    !is.na(conf_pct)
  
  weird_vals <- unique(conf_raw[!recognized])
  weird_vals <- weird_vals[!is.na(weird_vals)]
  
  if (length(weird_vals) > 0) {
    cli_alert_warning("Confidence_ID has unexpected/unparseable values ({length(weird_vals)} unique).")
    cli_text(paste0("- ", head(weird_vals, 20)))
  }
  
  return(plots_conf)
}

get_vb_cc <- function(vb_url, renew_cache = FALSE){
  
  cache_dir  <- rappdirs::user_cache_dir("vegbank")
  cache_file <- file.path(cache_dir, "cc_all.csv")
  
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  obj <- if (file.exists(cache_file) & !renew_cache) {
    cc_all <- read_csv(cache_file, progress = FALSE, show_col_types = FALSE, guess_max = 20000)
  } else {
    vb_set_base_url(vb_url) # "https://api-dev.vegbank.org"
    cli::cli_alert_info("Downloading vb community concept data.")
  
   page_init  <- 5000   # starting page size (can shrink if error)
   page_min   <- 500    # don't go smaller than this
   max_pages  <- 500    # hard stop
   sleep_sec  <- 0.05   # brief pause to avoid error
   checkpoint <- "cc_all_checkpoint.rds" # just in case something fails
   save_every <- 10
  
   out        <- list()
   seen_codes <- character(0)
   limit      <- page_init
  
   for (i in seq_len(max_pages)) {
     offset <- (i - 1L) * limit
  
     chunk <- tryCatch(
       vb_get_community_concepts(limit = limit, offset = offset),
       error = function(e) {
         limit <<- max(page_min, floor(limit / 2))
         tryCatch(
           vb_get_community_concepts(limit = limit, offset = offset),
           error = function(e2) {
             NULL
           }
        )
       }
     )
  
     if (is.null(chunk) || !nrow(chunk)) {
       break
     }
     
     if ("cc_code" %in% names(chunk)) {
       new <- !chunk$cc_code %in% seen_codes
       if (!any(new)) {
         break
      }
       seen_codes <- c(seen_codes, chunk$cc_code[new])
       chunk <- chunk[new, , drop = FALSE]
     }
  
     out[[length(out) + 1L]] <- chunk
     total <- sum(vapply(out, nrow, integer(1)))
     if (nrow(chunk) < limit) {
       break
     }
  
     if (save_every > 0 && (i %% save_every == 0)) {
       tmp <- bind_rows(out) %>% distinct()
       saveRDS(tmp, checkpoint)
     }
  
     if (sleep_sec > 0) Sys.sleep(sleep_sec)
   }
   
   out_char <- lapply(out, function(x) {
     x %>% mutate(comm_description = as.character(comm_description),
                  comm_party_comments = as.character(comm_party_comments),
                  status_rf_code = as.character(status_rf_code),
                  status_rf_label = as.character(status_rf_label),
                  parent_cc_code = as.character(parent_cc_code),
                  parent_name = as.character(parent_name),
                  stop_date = as.POSIXct(stop_date),
                  start_date = as.POSIXct(start_date))
   })
  
   cc_all <- bind_rows(out_char) %>% distinct()
   write_csv(cc_all, cache_file, progress = FALSE)
  }
  return(cc_all)
}

load_reference_tables <- function(in_dir){
  # Community concepts from VegBank
  cc_all <- get_vb_cc("https://api-dev.vegbank.org", renew_cache = FALSE)
  
  cc_current <- cc_all %>%
    filter(current_accepted == TRUE)
  
  # CA code map
  cacode_sheet_path <- file.path(in_dir, "lookup-tables/VegBank_CrosswalkHierarchyMCV.csv")
  
  cacode_map_raw <- read_csv(cacode_sheet_path, progress = FALSE, show_col_types = FALSE)
  
  cacode_map <- cacode_map_raw %>%
    mutate(
      CaCode_norm = str_squish(str_to_lower(CaCode)),
      NVC_norm = str_squish(str_to_lower(as.character(`2009/NVC_Code`)))
    ) %>%
    filter(!is.na(CaCode_norm), CaCode_norm != "", !is.na(NVC_norm), NVC_norm != "")
  
  # warn on duplicate CaCodes
  dup <- cacode_map %>%
    count(CaCode_norm) %>%
    filter(n > 1)
  # TODO: do we need to do anything about this? is this normal?
  # most recent run says: cacode_map has duplicate CaCode values (45 codes). Mapping may be ambiguous.
  # Sample duplicates:
  #  - 21.310.00- 32.037.00- 33.020.00- 35.110.00- 35.111.00- 35.150.00- 35.310.00- 36.310.00- 36.400.00- 37.070.00
  if (nrow(dup) > 0) {
    cli_alert_warning("cacode_map has duplicate CaCode values ({nrow(dup)} codes). Mapping may be ambiguous.")
    cli_text("Sample duplicates:")
    cli_text(paste0("- ", head(dup$CaCode_norm, 10)))
  }
  
  # cc lookup table
  cc_lookup <- cc_current %>%
    mutate(comm_code_norm = str_squish(str_to_lower(as.character(comm_code)))) %>%
    filter(!is.na(comm_code_norm), comm_code_norm != "") %>%
    distinct(comm_code_norm, .keep_all = TRUE) %>%
    select(cc_code, comm_code_norm)
  
  list(
    cacode_map = cacode_map,
    cc_lookup = cc_lookup
  )
}

assign_vb_cc_code <- function(classification, cacode_map, cc_lookup){

  cacode_map_1to1 <- cacode_map %>%
    group_by(CaCode_norm) %>%
    summarise(
      NVC_norm = first(NVC_norm),
      n_map = n(),
      .groups = "drop"
    )
  
  # warn if any CaCodes map to multiple NVC codes
  multi <- cacode_map %>%
    count(CaCode_norm, name = "n_map") %>%
    filter(n_map > 1)
  
  if (nrow(multi) > 0) {
    cli_alert_warning(
      "Some CaCode values map to multiple NVC codes in cacode_map ({nrow(multi)} CaCodes). Using the first NVC code for now to avoid row duplication."
    )
    cli_text(paste0("- ", head(multi$CaCode_norm, 10)))
  }
  
  classification_norm <- classification %>%
    mutate(CaCode_norm = str_squish(str_to_lower(CaCode))) %>%
    left_join(cacode_map_1to1 %>% select(CaCode_norm, NVC_norm), by = "CaCode_norm") %>%
    mutate(comm_code_norm = NVC_norm) %>%
    left_join(cc_lookup, by = "comm_code_norm") %>%
    mutate(vb_cc_code = cc_code)
  
  class_nas <- classification_norm %>% 
    filter(is.na(vb_cc_code)) %>% 
    select(Alliance, Association, CaCode, NVC_norm) %>% 
    distinct()
  
  write.csv(class_nas, "data/cacode-nolookup.csv", row.names = F)
  
  # warn if lots of NAs
  na_ct <- sum(is.na(classification_norm$vb_cc_code))
  if (na_ct > 0) {
    cli_alert_warning("vb_cc_code is NA for {na_ct} rows (out of {nrow(classification_norm)}).")
  }
  
  classification_norm
}

join_classifications <- function(classification_with_cc, plots_conf, projects_proj){
  out <- classification_with_cc %>%
    left_join(plots_conf, by = "SurveyID") %>%
    left_join(projects_proj, by = "ProjectCode")

  if (nrow(out) != nrow(classification_with_cc)) {
    stop(glue(
      "Row count changed after joins: {nrow(classification_with_cc)} -> {nrow(out)}. ",
      "This indicates duplicates in join keys (SurveyID or ProjectCode)."
    ))
  }
  
  out
}

community_loader <- function(in_dir, out_dir){
  
  out <- load_community_files(in_dir)
  
  list2env(out, envir = environment())
  
  projects_proj <- normalize_projects_classification(projects, in_dir)
  plots_conf <- normalize_class_confidence(plots)
  
  refs <- load_reference_tables(in_dir)
  
  classification_with_cc <- assign_vb_cc_code(
    classification = classification,
    cacode_map = refs$cacode_map,
    cc_lookup = refs$cc_lookup
  ) 
    # TODO: drop GXXX values in CaCode
    # TODO: drop NAs and n/a's in CaCode
    # they don't get inserted anyway but might as well be explicit about it
  
  no_match <- classification_with_cc %>% 
    filter(is.na(vb_cc_code)) %>% 
    select(CaCode, Alliance, Association) %>% 
    filter(!is.na(CaCode)) %>% 
    distinct()
  
  class_cc_proj <- join_classifications(
    classification_with_cc = classification_with_cc,
    plots_conf = plots_conf,
    projects_proj = projects_proj
  )
  
  class_cc_proj <- class_cc_proj %>% 
    mutate(multivariate_analysis = if_else(!(multivariate_analysis %in% c("TRUE", "FALSE")), NA, multivariate_analysis))
  
  # TODO: possibly improve messaging here?
  stopifnot(nrow(class_cc_proj) == nrow(community_LT))
  stopifnot(all(names(c("class_notes","inspection","multivariate_analysis","class_confidence","vb_cc_code")) %in% names(class_cc_proj)))
  
  community_LT$user_ob_code <- class_cc_proj$SurveyID
  community_LT$user_cl_code <- 1:nrow(class_cc_proj)
  community_LT$class_notes <- class_cc_proj$class_notes
  community_LT$inspection <- class_cc_proj$inspection
  community_LT$multivariate_analysis <- class_cc_proj$multivariate_analysis
  community_LT$class_confidence <- class_cc_proj$class_confidence
  community_LT$vb_cc_code <- class_cc_proj$vb_cc_code
  
  # save filled in loader table
  out_path <- file.path(out_dir, "communityClassificationsLT.csv")
  cli::cli_alert_success("Writing output file to:")
  cli::cli_ul(out_path)
  
  write_csv(community_LT, out_path)
}
