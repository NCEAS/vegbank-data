library(tidyverse)
library(here)
library(stringr)
library(vegbankr)

# load in CDFW data -------------------------------------------------------
# RAPlants
csv_path <- here("data", "RAPlants.csv")
plants <- read_csv(csv_path, show_col_types = FALSE)

# loading CA lookup tables
csv_path <- here("data", "USDA_PLANTS.csv")
plants_lookup <- read_csv(csv_path, show_col_types = FALSE)

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

# set_vb_base_url("https://api-dev.vegbank.org")    (Run this before running functions from vegbankr)
# CREATING DF BY LOOPING THROUGH "PAGES" OF VALUES
# saved as csv so commented the code

# Adaptive, resumable pager for VegBank plant concepts

# page_init <- 5000 # shrink this if there is an error
# page_min <- 500 # don't go smaller than this
# max_pages <- 500 # hard stop
# sleep_sec <- 0.05 # brief pause to avoid error
# keep_cols <- c("pc_code","plant_name")
# checkpoint <- "pc_all_checkpoint.rds" # just in case something fails
# save_every <- 10

# out <- list()
# seen_codes <- character(0)
# limit <- page_init

# for (i in seq_len(max_pages)) {
#   offset <- (i - 1L) * limit
#   message(sprintf("Page %d | limit=%d | offset=%d", i, limit, offset))
  
  # try once; on failure (e.g., 504), halve the limit and retry
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
  
#   keep <- intersect(keep_cols, names(chunk))
#   if (length(keep)) chunk <- chunk[, keep, drop = FALSE]
  
#   if ("pc_code" %in% names(chunk)) {
#     new <- !chunk$pc_code %in% seen_codes
#     if (!any(new)) { message("  All rows seen already; stopping."); break }
#     seen_codes <- c(seen_codes, chunk$pc_code[new])
#     chunk <- chunk[new, , drop = FALSE]
#   }
  
#   out[[length(out) + 1L]] <- chunk
#   total <- sum(vapply(out, nrow, integer(1)))
#   message(sprintf("  +%d new rows (total: %d)", nrow(chunk), total))
  
#   if (nrow(chunk) < limit) { message("  Short page; done."); break }
  
#   if (save_every > 0 && (i %% save_every == 0)) {
#     tmp <- dplyr::bind_rows(out) %>% distinct()
#     saveRDS(tmp, checkpoint)
#     message(sprintf("  Saved checkpoint (%d rows) -> %s", nrow(tmp), checkpoint))
#   }
  
#   if (sleep_sec > 0) Sys.sleep(sleep_sec)
# }

# pc_all <- bind_rows(out) %>% distinct()
# message(sprintf("Finished. Total plant concepts: %d", nrow(pc_all)))

csv_path <- here("data", "pc_all.csv")
pc_all <- read_csv(csv_path, show_col_types = FALSE)

pc_lookup <- pc_all %>%
  mutate(name_clean = gsub("^\\[|\\]$", "", plant_name)) %>%
  separate_rows(name_clean, sep = "\\s*\\+\\s*") %>%
  mutate(plant_name_norm = str_squish(str_to_lower(name_clean))) %>%
  filter(plant_name_norm != "") %>%
  group_by(plant_name_norm) %>%
  summarise(pc_code = first(pc_code), .groups = "drop")

mapping_values <- plants %>%
  mutate(
    authorPlantName = str_squish(coalesce(SpeciesName, "")),
    author_norm = str_squish(stringr::str_to_lower(coalesce(SpeciesName, "")))
  ) %>%
  left_join(pc_lookup, by = c("author_norm" = "plant_name_norm")) %>%
  transmute(
    authorPlantName = na_if(authorPlantName, ""),  # turn "" back to NA if you want
    vb_pc_code = pc_code
  )

nrow(mapping_values) == nrow(plants)

# Assigning columns to loader table ---------------------------------------

strata_cover_LT$user_sr_code <- plants$Stratum
strata_cover_LT$authorPlantName <- plants$SpeciesName
strata_cover_LT$cover <- plants$Species_cover
strata_cover_LT$vb_pc_code <- mapping_values$vb_pc_code

# All variables besides authorPlantName, cover, and user_sre_code were not matched and are left as 'NA'
