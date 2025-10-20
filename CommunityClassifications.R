library(tidyverse)
library(here)
library(stringr)
source("Build_Loader_Table.R")

# load in CDFW data -------------------------------------------------------
# RAProjects
csv_path <- here("data", "RAProjects.csv")
projects <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------
community_template_fields <- build_loader_table(
  sheet_url = "https://docs.google.com/spreadsheets/d/1ORubguw1WDkTkfiuVp2p59-eX0eA8qMQUEOfz1TWfH0/edit?gid=2109807393#gid=2109807393",
  sheet = "CommunityClassifications",
  source_df = projects
)

community_LT <- community_template_fields$template
  
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

community_LT$expert_system = projects$ClassificationTool
community_LT$inspection = projects$inspectionText
community_LT$multivariate_analysis = projects$multivariateAnalysisText

# All variables besides expertSystem, inspection, multivariateAnalysis, and tableAnalysis were not matched and are left as 'NA'


