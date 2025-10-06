library(tidyverse)
library(here)
library(stringr)
source("Build_Loader_Table.R")

# load in CDFW data -------------------------------------------------------
# RAProjects
csv_path <- here("data", "RAProjects.csv")
projects <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------
test <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "CommunityClassifications",
  source_df = projects
)
test$template
test$fields
# create a header csv with only variable names
csv_path <- here("loader_tables", "CommunityClassification_Header.csv")
template <- read_csv(csv_path, show_col_types = FALSE)

# Tidying CDFW data -------------------------------------------------------
# Manually assigning ClassificationDescription values to inspection, multivariateAnalysis, or tableAnalysis based on the described methods.
# None mapped to tableAnalysis

projects <- projects %>%
  mutate(
    inspectionText = if_else(
      coalesce(str_detect(ClassificationDescription, "surveys were keyed using"), FALSE),
      ClassificationDescription, NA_character_
    ),
    multivariateAnalysisText = if_else(
      coalesce(str_detect(
        ClassificationDescription,
        'See "Vegetation Map and Classification of Fish|These data were analyzed using multivariate cluster analysis, performed|See: Classification of the Vegetation Alliances|These data were analyzed using a number of statistical methods, chiefly an|CNPS analyzed the species cover data using PC-Ord and R software'
      ), FALSE),
      ClassificationDescription, NA_character_
    )
  )

# Assigning columns to loader table ---------------------------------------

CommunityClassifications_LT <- bind_rows(
  template,
  tibble(expertSystem = projects$ClassificationTool,
         inspection = projects$inspectionText,
         multivariateAnalysis = projects$multivariateAnalysisText
  )
)
CommunityClassifications_LT
# All variables besides expertSystem, inspection, multivariateAnalysis, and tableAnalysis were not matched and are left as 'NA'

# Assigning columns to loader table (Test Automation) ---------------------
test$template$expert_system <- projects$ClassificationTool
test$template$inspection = projects$inspectionText
test$template$multivariate_analysis = projects$multivariateAnalysisText

test$template
