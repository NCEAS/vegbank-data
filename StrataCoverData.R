library(tidyverse)
library(here)
library(stringr)
library(vegbankr)
library(stringdist)
library(fuzzyjoin)
library(taxize)
source('Build_Loader_Table.R')

# load in CDFW data -------------------------------------------------------
# RAPlants
csv_path <- here("data", "RAPlants.csv")
plants <- read_csv(csv_path, show_col_types = FALSE)

# loading CA lookup tables
csv_path <- here("data", "USDA_PLANTS.csv")
plants_lookup <- read_csv(csv_path, show_col_types = FALSE) # ignore parsing issue

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

set_vb_base_url("https://api-dev.vegbank.org")    # (Run this before running functions from vegbankr)
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
#     get_all_plant_concepts(limit = limit, offset = offset),
#     error = function(e) {
#       message("  Request failed: ", conditionMessage(e))
#       limit <<- max(page_min, floor(limit/2))
#       message("  Reducing limit and retrying with limit=", limit)
#       tryCatch(get_all_plant_concepts(limit = limit, offset = offset),
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

# ----------------------------------------------------------------------------

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
  
  # use stringdist with method "lv" to get edit operations
  dist <- stringdist(str1, str2, method = "lv")
  
  if (dist > 3) return (FALSE)
  
  # check character-by-character alignment
  chars1 <- strsplit(str1, "")[[1]]
  chars2 <- strsplit(str2, "")[[2]]
  
  # for different lengths, pad the shorter one
  max_len <- max(length(chars1), length(chars2))
  if (length(chars1) < max_len) chars1 <- c(chars1,
                                            rep("", max_len - length(chars1)))
  if (length(chars2) < max_len) chars2 <- c(chars2,
                                            rep("", max_len - length(chars2)))
  
  # find positions where characters differ
  diffs <- which(chars1 != chars2)
  
  if (length(diffs) == 0) return (TRUE)
  
  # check if differences are consecutive (indicating substring replacement)
  consecutive_diffs <- max(diff(diffs) == 1)
  max_consecutive <- max(rle(diff(diffs) == 1)$lengths[rle(diff(diffs) == 1)$values], 0)
  
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

# next step: fix function

# Assigning columns to loader table ---------------------------------------

strata_cover_LT$user_sr_code <- plants$Stratum
strata_cover_LT$authorPlantName <- plants$SpeciesName
strata_cover_LT$cover <- plants$Species_cover
strata_cover_LT$vb_pc_code <- mapping_values$vb_pc_code2

# All variables besides authorPlantName, cover, and user_sre_code were not matched and are left as 'NA'
