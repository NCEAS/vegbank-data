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

# creating loader table ---------------------------------------------------


# RAPlots and RAProjects needs to be merged
plots_merged <- plots

projects <- projects %>% 
  left_join(plots_merged, by = "ProjectCode")

community_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "CommunityClassifications",
  source_df = projects
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

### inspection (CommunityClassifications) + multivariateAnalysis (CommunityClassifications) + tableAnalysis (CommunityClassifications ###
# Manually assigning ClassificationDescription values to inspection, multivariateAnalysis, or tableAnalysis based on the described methods.
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

projects <- projects %>%
  mutate(
    inspectionText = if_else(
      coalesce(str_detect(ClassificationDescription, text_map["inspectionText"]), FALSE),
      ClassificationDescription, NA_character_
    ),
    multivariateAnalysisText = if_else(
      coalesce(str_detect(ClassificationDescription, text_map["multivariateAnalysisText"]), FALSE),
      ClassificationDescription, NA_character_
    )
  )

# Confidence_ID (RAPlots) - class_confidence (CommunityClassifications)
# Convert L, M, H to Low, Medium, High
# Convert Not recorded to NA

projects <- projects %>% 
  mutate(
    class_confidence = case_when(
      Confidence_ID == "Not recorded" ~ NA,
      Confidence_ID == "H" ~ "High",
      Confidence_ID == "M" ~ "Medium",
      Confidence_ID == "L" ~ "Low",
      
      TRUE ~ NA_character_
    )
  )

# Assigning columns to loader table ---------------------------------------

community_LT$expert_system <- projects$ClassificationTool
community_LT$inspection <- projects$inspectionText
community_LT$multivariate_analysis <- projects$multivariateAnalysisText
community_LT$class_confidence <- projects$class_confidence # Error, need to fix

# All variables besides expertSystem, inspection, multivariateAnalysis, and tableAnalysis were not matched and are left as 'NA'
write_csv(community_LT, here('loader_tables', 'CommunityClassificationsLT.csv'))
