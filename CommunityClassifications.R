library(tidyverse)
library(here)
library(stringr)
source("Build_Loader_Table.R")

# load in CDFW data -------------------------------------------------------
# RAProjects
csv_path <- here("data", "RAProjects.csv")
projects <- read_csv(csv_path, show_col_types = FALSE)
folder <- 'data'
plots <- read_csv(here(folder, 'RAPlots.csv'), 
                  col_types = cols(.default = col_guess(), 
                                   `PlotOther5` = col_character()))
classification <- read_csv(here(folder, "RAClassification.csv"), show_col_types = FALSE)

# creating loader table ---------------------------------------------------


community_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "CommunityClassifications",
  source_df = classification
)

community_LT <- community_template_fields$template

# Checking values ---------------------------------------------------------

# ClassificationDescription (RAProjects) - inspection (CommunityClassifications) + multivariateAnalysis (CommunityClassifications) + tableAnalysis (CommunityClassifications)
# Values must be manually matched to 1 of 3 fields: inspection, multivariateAnalysis, tableAnalysis
unique(projects$ClassificationDescription)
class(projects$ClassificationDescription) # character

# Confidence_ID (RAPlots) - class_confidence (CommunityClassifications)
# Convert L, M, H to Low, Medium, High
unique(plots$Confidence_ID)
class(plots$Confidence_ID) # character

# Tidying CDFW data -------------------------------------------------------

### inspection (CommunityClassifications) + multivariateAnalysis (CommunityClassifications)
# Manually assigning ClassificationDescription values to inspection or multivariateAnalysis
# None mapped to tableAnalysis

text_map <- c(
  "inspectionText" = "surveys were keyed using",
  "multivariateAnalysisText" = paste(
    'See "Vegetation Map and Classification of Fish',
    'These data were analyzed using multivariate cluster analysis, performed',
    'See: Classification of the Vegetation Alliances',
    'These data were analyzed using a number of statistical methods, chiefly an',
    'CNPS analyzed the species cover data using PC-Ord and R software',
    sep = "|"
  )
)

projects_proj <- projects %>%
  mutate(
    inspectionText = if_else(
      coalesce(str_detect(ClassificationDescription, text_map["inspectionText"]), FALSE),
      ClassificationDescription, NA_character_
    ),
    multivariateAnalysisText = if_else(
      coalesce(str_detect(ClassificationDescription, text_map["multivariateAnalysisText"]), FALSE),
      ClassificationDescription, NA_character_
    )
  ) %>%
  group_by(ProjectCode) %>%
  summarise(
    ClassificationTool       = first(ClassificationTool),
    inspectionText           = first(inspectionText),
    multivariateAnalysisText = first(multivariateAnalysisText),
    .groups = "drop"
  )

# Confidence_ID (RAPlots) - class_confidence (CommunityClassifications)
# Convert L, M, H to Low, Medium, High
# Convert Not recorded to NA

plots_conf <- plots %>%
  transmute(
    SurveyID,
    class_confidence = case_when(
      Confidence_ID == "Not recorded" ~ "",
      is.na(Confidence_ID)            ~ "",
      Confidence_ID == "H"            ~ "High",
      Confidence_ID == "M"            ~ "Medium",
      Confidence_ID == "L"            ~ "Low",
      TRUE                            ~ NA_character_
    )
  ) %>%
  group_by(SurveyID) %>%
  summarise(
    class_confidence = first(class_confidence),
    .groups = "drop"
  )

# vb_cc_code

# set_vb_base_url("https://api-dev.vegbank.org")

# saved as csv so commented the code

# page_init  <- 5000   # starting page size (can shrink if error)
# page_min   <- 500    # don't go smaller than this
# max_pages  <- 500    # hard stop
# sleep_sec  <- 0.05   # brief pause to avoid error
# checkpoint <- "cc_all_checkpoint.rds" # just in case something fails
# save_every <- 10

# out        <- list()
# seen_codes <- character(0)
# limit      <- page_init

# for (i in seq_len(max_pages)) {
#   offset <- (i - 1L) * limit
#   message(sprintf("Page %d | limit=%d | offset=%d", i, limit, offset))
  
#   chunk <- tryCatch(
#     get_all_community_concepts(limit = limit, offset = offset),
#     error = function(e) {
#       message("  Request failed: ", conditionMessage(e))
#       limit <<- max(page_min, floor(limit / 2))
#       message("  Reducing limit and retrying with limit = ", limit)
#       tryCatch(
#         get_all_community_concepts(limit = limit, offset = offset),
#         error = function(e2) {
#           message("  Retry failed."); 
#           NULL
#         }
#      )
#     }
#   )
  
#   if (is.null(chunk) || !nrow(chunk)) {
#     message("  No rows returned; stopping.")
#     break
#   }
#   
#   if ("cc_code" %in% names(chunk)) {
#     new <- !chunk$cc_code %in% seen_codes
#     if (!any(new)) {
#       message("  All rows seen already; stopping.")
#       break
#    }
#     seen_codes <- c(seen_codes, chunk$cc_code[new])
#     chunk <- chunk[new, , drop = FALSE]
#   }
  
#   out[[length(out) + 1L]] <- chunk
#   total <- sum(vapply(out, nrow, integer(1)))
#   message(sprintf("  +%d new rows (total: %d)", nrow(chunk), total))
  
#   if (nrow(chunk) < limit) {
#     message("  Short page; done.")
#     break
#   }
  
#   if (save_every > 0 && (i %% save_every == 0)) {
#     tmp <- bind_rows(out) %>% distinct()
#     saveRDS(tmp, checkpoint)
#     message(sprintf("  Saved checkpoint (%d rows) -> %s", nrow(tmp), checkpoint))
#   }
  
#   if (sleep_sec > 0) Sys.sleep(sleep_sec)
# }

# cc_all <- bind_rows(out) %>% distinct()
# message(sprintf("Finished. Total community concepts: %d", nrow(cc_all)))

# write_csv(cc_all, here("data", "cc_all.csv"))

cc_all <- read_csv(here("data", "cc_all.csv"), show_col_types = FALSE)

cc_current <- cc_all %>%
  filter(current_accepted == TRUE)

classification_norm <- classification %>%
  mutate(
    CaCode_norm = str_squish(str_to_lower(CaCode))
  )

classification_norm$CaCode_norm

cc_lookup <- cc_current %>%
  mutate(
    ca_code_norm = str_squish(str_to_lower(as.character(comm_code)))
  ) %>%
  filter(!is.na(ca_code_norm), ca_code_norm != "") %>%
  select(cc_code, ca_code_norm)

class_with_cc <- classification_norm %>%
  left_join(
    cc_lookup,
    by = c("CaCode_norm" = "ca_code_norm")
  ) %>%
  mutate(
    vb_cc_code = cc_code
  )

class_with_cc
table(is.na(class_with_cc$vb_cc_code))

# Join classification with plots and projects
      
class_with_cc_conf <- class_with_cc %>%
  left_join(plots_conf, by = "SurveyID")

class_cc_proj <- class_with_cc_conf %>%
  left_join(
    projects_proj,
    by = "ProjectCode"
  )

# Exploring unmatched values

unmatched <- class_with_cc %>%
  filter(is.na(vb_cc_code))

nrow(unmatched)

# number of unmatched rows that dont have CaCode
unmatched %>%
  count(is.na(CaCode), name = "n_rows")

# only rows that have a CaCode
unmatched_nonmissing <- unmatched %>%
  filter(!is.na(CaCode) & CaCode != "")

nrow(unmatched_nonmissing)

# each CaCode and how much unmathched rows it created
unmatched_nonmissing %>%
  count(CaCode, sort = TRUE)

unmatched_nonmissing %>%
  count(CaCode_norm, sort = TRUE)

unmatched_with_context <- class_cc_proj %>%
  filter(is.na(vb_cc_code)) %>%
  select(
    SurveyID,
    ProjectCode,
    CaCode,
    CaCode_norm,
    ClassificationTool,
    inspectionText,
    multivariateAnalysisText
  )

head(unmatched_with_context, 20)

# Assigning columns to loader table ---------------------------------------

community_LT$expert_system <- class_cc_proj$ClassificationTool
community_LT$inspection <- class_cc_proj$inspectionText
community_LT$multivariate_analysis <- class_cc_proj$multivariateAnalysisText
community_LT$class_confidence <- class_cc_proj$class_confidence
community_LT$vb_cc_code <- class_cc_proj$vb_cc_code

write_csv(community_LT, here('loader_tables', 'CommunityClassificationsLT.csv'))
