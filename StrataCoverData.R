library(tidyverse)
library(here)
library(stringr)
library(vegbankr)
library(stringdist)
library(fuzzyjoin)
library(taxize)
library(vctrs)
library(glue)
source('R/build_loader_table.R')

# load in CDFW data -------------------------------------------------------
# RAPlants
# csv_path <- here("data", "RAPlants.csv")
# plants <- read_csv(csv_path, show_col_types = FALSE)

# loading CA lookup tables
csv_path <- here("data", "USDA_PLANTS.csv")
plants_lookup <- read_csv(csv_path, show_col_types = FALSE) # ignore parsing issue

# read in RAReleve data
folder_1 <- '/var/data/curation/vegbank/VegBankProject_StdDatasets1_RAReleve_20250915'
plots_1 <- read_csv(here(folder_1, 'RAPlants.csv'))

# read in AARecon data
folder_2 <- '/var/data/curation/vegbank/VegBankProject_StdDatasets2_AARecon_20251204'
plots_2 <- read_csv(here(folder_2, 'RAPlants.csv'))

# merge RAReleve and AARecon data
plants <- rbind(plots_1, plots_2)

# new AARecon data
# strata_cover_loader <- function(in_dir, out_dir) {
#   
#   sub_folders <- dir(in_dir, full.names = TRUE) %>% 
#     grep(pattern = "VegBankProject", value = TRUE)
#   
#   # read in RAPlants
#   project_files <- dir(sub_folders, full.names = TRUE) %>% 
#     grep(pattern = "RAPlants.csv", value = TRUE)
#   
#   cli::cli_alert_info(paste("Processing", length(project_files), "people/contributor tables from:"))
#   cli::cli_ul(project_files)
#   
#   projects_df_list <- lapply(project_files, read_csv, progress = FALSE, show_col_types = FALSE)
#   projects <- do.call(bind_rows, projects_df_list)
  
# creating loader table ---------------------------------------------------

strata_cover_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "StrataCoverData",
  source_df = plants
)

strata_cover_LT <- strata_cover_template_fields$template

# Checking values ---------------------------------------------------------

# making sure values that should be from SpeciesName are found in the USDA plants table.
# Some values are not found directly in the table.

plants %>%
  filter(!SpeciesName %in% plants_lookup$ScientificName) %>%
  distinct(SpeciesName)

# making sure values from Stratum fit the CA Description
unique(plants$Stratum)
# It looks like there are some values that do not fit the CA Description:
# GitHub Issue #15

# tidying CDFW data -----------------------------------------------------------

vb_set_base_url("https://api-dev.vegbank.org")    # (Run this before running functions from vegbankr)
# CREATING DF BY LOOPING THROUGH "PAGES" OF VALUES
# saved as csv so commented the code

# Adaptive, resumable pager for VegBank plant concepts

# page_init <- 5000 # shrink this if there is an error
# page_min <- 500 # don't go smaller than this
# max_pages <- 500 # hard stop
# sleep_sec <- 0.05 # brief pause to avoid error
# keep_cols <- c("pc_code","plant_name", "current_accepted")
# checkpoint <- "pc_all_checkpoint.rds" # just in case something fails
# save_every <- 10
# 
# out <- list()
# seen_codes <- character(0)
# limit <- page_init
# 
# for (i in seq_len(max_pages)) {
#   offset <- (i - 1L) * limit
#   message(sprintf("Page %d | limit=%d | offset=%d", i, limit, offset))
# 
# #  try once; on failure (e.g., 504), halve the limit and retry
#   chunk <- tryCatch(
#     vb_get_plant_concepts(limit = limit, offset = offset),
#     error = function(e) {
#       message("  Request failed: ", conditionMessage(e))
#       limit <<- max(page_min, floor(limit/2))
#       message("  Reducing limit and retrying with limit=", limit)
#       tryCatch(vb_get_plant_concepts(limit = limit, offset = offset),
#                error = function(e2) { message("  Retry failed."); NULL })
#     }
#   )
#   if (is.null(chunk) || !nrow(chunk)) { message("  No rows returned; stopping."); break }
# 
#   keep <- intersect(keep_cols, names(chunk))
#   if (length(keep)) chunk <- chunk[, keep, drop = FALSE]
# 
#   if ("pc_code" %in% names(chunk)) {
#     new <- !chunk$pc_code %in% seen_codes
#     if (!any(new)) { message("  All rows seen already; stopping."); break }
#     seen_codes <- c(seen_codes, chunk$pc_code[new])
#     chunk <- chunk[new, , drop = FALSE]
#   }
# 
#   out[[length(out) + 1L]] <- chunk
#   total <- sum(vapply(out, nrow, integer(1)))
#   message(sprintf("  +%d new rows (total: %d)", nrow(chunk), total))
# 
#   if (nrow(chunk) < limit) { message("  Short page; done."); break }
# 
#   if (save_every > 0 && (i %% save_every == 0)) {
#     tmp <- dplyr::bind_rows(out) %>% distinct()
#     saveRDS(tmp, checkpoint)
#     message(sprintf("  Saved checkpoint (%d rows) -> %s", nrow(tmp), checkpoint))
#   }
# 
#   if (sleep_sec > 0) Sys.sleep(sleep_sec)
# }
# 
# pc_all <- bind_rows(out) %>% distinct()
# message(sprintf("Finished. Total plant concepts: %d", nrow(pc_all)))
# 
# write_csv(pc_all, here("data", "pc_all.csv"))
# 
# pc_lookup <- pc_all %>%
#   mutate(name_clean = gsub("^\\[|\\]$", "", plant_name)) %>%
#   separate_rows(name_clean, sep = "\\s*\\+\\s*") %>%
#   mutate(plant_name_norm = str_squish(str_to_lower(name_clean))) %>%
#   filter(plant_name_norm != "") %>%
#   group_by(plant_name_norm) %>%
#   summarise(pc_code = first(pc_code), .groups = "drop")
# 
# mapping_values <- plants %>%
#   mutate(
#     authorPlantName = str_squish(coalesce(SpeciesName, "")),
#     author_norm = str_squish(stringr::str_to_lower(coalesce(SpeciesName, "")))
#   ) %>%
#   left_join(pc_lookup, by = c("author_norm" = "plant_name_norm")) %>%
#   transmute(
#     authorPlantName = na_if(authorPlantName, ""),  # turn "" back to NA if you want
#     vb_pc_code = pc_code
#   )
# 
# nrow(mapping_values) == nrow(plants) # end of commenting

# ----------------------- vb_pc_code -----------------------------------------
# read in csv
csv_path <- here("data", "pc_all.csv")
pc_all <- read_csv(csv_path, show_col_types = FALSE)

# filter current_accepted
pc_current <- pc_all %>% 
  filter(current_accepted == TRUE)

# match records of pc_current with mapping_values' vb_pc_code
# creating a column where it marks TRUE for a match
mapping_values <- mapping_values %>%
  mutate(match_flag = authorPlantName %in% pc_current$plant_name)

# if match_flag == false, turn vb_pc_code into NA
mapping_values <- mapping_values %>%
  mutate(vb_pc_code2 = ifelse(match_flag, vb_pc_code, NA))

# removing duplicates
plants2 <- plants %>% 
  distinct(SpeciesName, .keep_all = TRUE)

# join CodeSpecies to mapping_values
mapping_values3 <- mapping_values %>% 
  left_join(plants2, by = c("authorPlantName" = "SpeciesName")) %>% 
  select(CodeSpecies, authorPlantName, match_flag, vb_pc_code2) # error

# # ----------------------------------------------------------------------------

# troubleshooting false matches
# making a cross-join comparison table
# make all names lowercase and trimmed
mapping_values_clean <- mapping_values %>%
  mutate(mapping_values_index = row_number()) %>%
  mutate(authorPlantName = str_trim(tolower(authorPlantName))) %>%
  select(mapping_values_name = authorPlantName, mapping_values_index)
pc_current_clean <- pc_current %>%
  mutate(pc_current_index = row_number()) %>%
  mutate(plant_name = str_trim(tolower(plant_name))) %>%
  select(pc_current_name = plant_name, pc_current_index)

# cross join
comparison_table <- stringdist_inner_join(
  mapping_values_clean,
  pc_current_clean,
  by = c("mapping_values_name" = "pc_current_name"),
  method = "cosine",
  max_dist = 0.1,
  distance_col = "similarity",
) %>%
  mutate(similarity = 1 - similarity) %>%
  filter(similarity < 1) %>%
  arrange(desc(similarity)) %>%
  distinct(pc_current_index, .keep_all = TRUE)

# trying different approach
# find closest matches using fuzzy matching
find_matches <- function(wrong_name, correct_list, max_distance = 3) {
  distances <- stringdist(wrong_name, correct_list, method = "lv")
  top_matches <- order(distances)[1:5]

  data.frame(
    original = wrong_name,
    suggested_match = correct_list[top_matches],
    distance = distances[top_matches]
  )
}

# get unmatched plant names
unmatched_plants <- mapping_values %>%
  filter(is.na(vb_pc_code) | match_flag == FALSE) %>%
  distinct(authorPlantName)

# reference list
reference_names <- pc_current$plant_name

# function for finding the closest names
find_closest_matches <- function(name_to_match, reference_list, top_n = 2) {

  # calculate string distances
  distances <- stringdist(name_to_match, reference_list, method = "jw")

  # get top 3 closest matches
  top_indices <- order(distances)[1:2]

  data.frame(
    unmatched_name = name_to_match,
    suggested_match = reference_list[top_indices],
    similarity_score = 1 - distances[top_indices],
    stringsAsFactors = FALSE
  )
}

# apply to all unmatched names
all_suggestions <- do.call(rbind, lapply(unmatched_plants$authorPlantName,
                                         find_closest_matches,
                                         reference_names))

# filter similarity (change the number according to your need)
# 0.9259259 has been suggested for spelling errors
# manual troubleshooting
high_similarity <- all_suggestions %>%
  filter(similarity_score >= 0.9259259) %>%
  arrange(unmatched_name, desc(similarity_score))

# uploading new plant concepts
low_similarity <- all_suggestions %>%
  filter(similarity_score < 0.9259259) %>%
  arrange(unmatched_name)

# function to detect localized differences
has_minor_differences <- function(str1, str2, max_consecutive_diff = 2) {

  # handle NA or empty strings
  if (is.na(str1) || is.na(str2) || str1 == "" || str2 == "") return (FALSE)

  # use stringdist with method "lv" to get edit distance
  dist <- stringdist(str1, str2, method = "lv")

  # if edit distance is large, likely not minor
  if (dist > 3) return (FALSE)

  # check character-by-character alignment
  chars1 <- strsplit(str1, "")[[1]]
  chars2 <- strsplit(str2, "")[[1]]

  # for different lengths, pad the shorter one
  max_len <- max(length(chars1), length(chars2))
  if (length(chars1) < max_len) chars1 <- c(chars1, rep("", max_len - length(chars1)))
  if (length(chars2) < max_len) chars2 <- c(chars2, rep("", max_len - length(chars2)))

  # find positions where characters differ
  diffs <- which(chars1 != chars2)

  if (length(diffs) == 0) return (TRUE)
  if (length(diffs) == 1) return (TRUE)

  # check if differences are consecutive (indicating substring replacement)
  consecutive_runs <- rle(diff(diffs) == 1)
  if (any(consecutive_runs$values)) {
    max_consecutive <- max(consecutive_runs$lengths[consecutive_runs$values])
  } else {
    max_consecutive <- 0
  }

  # return TRUE if differences are not in long consecutive streaks
  return(max_consecutive <= max_consecutive_diff)
}

# create correction mapping
corrections <- high_similarity %>%
  group_by(unmatched_name) %>%
  slice_max(similarity_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(unmatched_name, suggested_match, similarity_score) %>%
  mutate(
    length_diff = abs(nchar(unmatched_name) - nchar(suggested_match)),
    is_minor_diff = mapply(has_minor_differences, unmatched_name, suggested_match)
  ) %>%
  filter(length_diff <= 2, is_minor_diff == TRUE)

# changing high_similarity$unmatched_name into a vector
high_unmatched <- high_similarity %>%
  pull(unmatched_name)

# verifying names
verified_names <- gna_verifier(high_unmatched)

# filtering verified_names
verified_names <- verified_names %>%
  select(submittedName, matchedName, currentName, currentCanonicalSimple,
         currentCanonicalFull)

# extract verified corrections
verified_corrections <- verified_names %>%
  filter(!is.na(currentCanonicalFull)) %>%
  select(unmatched_name = submittedName,
         verified_name = currentCanonicalFull)

# # check if verified names exist in pc_current (reference)
# valid_corrections <- verified_names %>%
#   inner_join(
#     pc_current %>% select(plant_name, pc_code),
#     by = c("currentCanonicalFull" = "plant_name")
#   )

# # merge back into mapping_values
# mapping_values_updated <- mapping_values %>%
#   left_join(valid_corrections,
#             by = c("authorPlantName" = "matchedName")) %>%
#   mutate(
#     vb_pc_code3 = if_else(is.na(vb_pc_code) & !is.na(pc_code),
#                           pc_code,
#                           vb_pc_code),
#     match_flag = !is.na(vb_pc_code)
#   )

# extract high_similarity$suggested_match
fuzzy_correction <- high_similarity %>%
  select(unmatched_name, suggested_match)

# cross-match verified_corrections and high_similarity$suggested_match
cross_matched <- verified_corrections %>%
  inner_join(fuzzy_correction, by = "unmatched_name",
             relationship = "many-to-many") %>%
  mutate(
    matches_agree = (unmatched_name == verified_name),
    matches_agree2 = (unmatched_name == suggested_match),
    char_length = (nchar(unmatched_name) == nchar(verified_name)),
    char_length2 = (nchar(unmatched_name) == nchar(suggested_match)),
    length_diff = abs(nchar(unmatched_name) - nchar(suggested_match)),
    similar_length = (length_diff <= 2),
    length_diff2 = abs(nchar(unmatched_name) == nchar(verified_name)),
    similar_length2 = (length_diff <= 2)
  )

# if both agreed matches are FALSE and the character length matches, use that match
# if both agreed matches are FALSE and no character length matches, do not change
# if one is TRUE, use that match

# reviewing if algorithm works
review <- cross_matched %>%
  filter(matches_agree == FALSE,
         matches_agree2 == FALSE,
         char_length == FALSE,
         char_length2 == FALSE,
         length_diff <= 3,
         length_diff2 <= 3)
review2 <- cross_matched %>%
  filter(matches_agree == FALSE,
         matches_agree2 == FALSE,
         char_length == FALSE,
         char_length2 == FALSE,
         length_diff > 2)

# if there is a suggested_match and it works, use it
# if suggested_match and verified_name doesn't match, upload new plant concept

# remaining problems:
# not all taxize resolved names match
# merging doesn't work

# filters currently accepted
mapping_unique <- mapping_values %>% 
  filter(match_flag == TRUE)

# USDA Troubleshooting
result <- plants %>% 
  
  # joins by SpeciesName to authorPlantName that have been accepted
  left_join(mapping_unique,
            by = c("SpeciesName" = "authorPlantName"),
            relationship = "many-to-many") %>%
  
  # marked by Species_name due to there being 4,743 entries (greatest value)
  distinct(Species_name, .keep_all = TRUE) %>% 
  
  # only relevant
  select(CodeSpecies, SpeciesName, vb_pc_code) %>% 
  
  # NA vb_pc_code
  filter(is.na(vb_pc_code))

# try comparison on SpeciesName with CodeSpecies
small1 <- review %>% 
  left_join(plants %>% select(CodeSpecies, SpeciesName) %>% distinct(),
            by = c("unmatched_name" = "SpeciesName")) %>%
  left_join(plants %>% select(CodeSpecies, SpeciesName) %>% distinct(),
            by = c("suggested_match" = "SpeciesName")) %>% 
  select(CodeSpecies.x, unmatched_name, CodeSpecies.y,
         everything(), -verified_name)
  
# if there is a missing CodeSpecies.y or CodeSpecies.x and -.y match,
# replace it as the original
# filter out plants that are not in the review data set as well as the ones
# that have a CodeSpecies in small1 and label them as plants that need new
# plant concepts

# missing CodeSpecies filter for manual evaluation of algorithm
small1_missing_filter <- small1 %>% 
  filter(!is.na(small1$CodeSpecies.y))

# CodeSpecies algorithm
# small1_algo <- small1 %>% 
#   mutate(suggested_match = case_when(
#     is.na(CodeSpecies.y) ~ unmatched_name,
#     CodeSpecies.y == CodeSpecies.x ~ unmatched_name,
#     TRUE ~ suggested_match
#   ))
small1_algo <- small1 %>% 
  mutate(
    turn_flag = is.na(CodeSpecies.y) | (CodeSpecies.y == CodeSpecies.x)
  )

# get the rows where turn_flag is TRUE
rows_to_update <- small1_algo %>%
  filter(turn_flag == TRUE) %>% 
  select(CodeSpecies.x, unmatched_name, suggested_match)

# update mapping_values based on these flagged rows
mapping_values2 <- mapping_values %>%
  left_join(rows_to_update, by = c("authorPlantName" = "unmatched_name")) %>%
  mutate(authorPlantName = coalesce(suggested_match, authorPlantName)) %>%
  select(-suggested_match)

# match records of pc_current with mapping_values' vb_pc_code
# creating a column where it marks TRUE for a match
mapping_values2 <- mapping_values2 %>%
  mutate(match_flag = authorPlantName %in% pc_current$plant_name)

# if match_flag == false, turn vb_pc_code into NA
mapping_values2 <- mapping_values2 %>%
  mutate(vb_pc_code2 = ifelse(match_flag, vb_pc_code, NA))

# checking if there are duplicates for assigning columns to LT
strata_cover_LT %>% 
  count(vb_pc_code) %>% 
  filter(n > 1) # no duplicates
mapping_values2 %>% 
  count(vb_pc_code2) %>% 
  filter(n > 1) # has duplicates

# see which rows in mapping_values2 don't match with strata_cover_LT
list <- mapping_values2 %>% 
  anti_join(strata_cover_LT, by = "vb_pc_code")

# adding row identifiers
strata_cover_LT2 <- strata_cover_LT %>% 
  mutate(row_id_LT = row_number())
mapping_values22 <- mapping_values2 %>% 
  mutate(row_id_map = row_number())
extra_rows <- mapping_values22 %>% 
  anti_join(strata_cover_LT, by = "authorPlantName")


# Assigning columns to loader table ---------------------------------------

strata_cover_LT$user_sr_code <- plants$Stratum
strata_cover_LT$authorPlantName <- plants$SpeciesName
strata_cover_LT$cover <- plants$Species_cover
strata_cover_LT$vb_pc_code <- mapping_values2$vb_pc_code2

# All variables besides authorPlantName, cover, and user_sre_code were not matched and are left as 'NA'
# }
