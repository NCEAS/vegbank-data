library(tidyverse)
library(here)
library(stringr)

# load in CDFW data -------------------------------------------------------
# RAProjects
csv_path <- here("data", "RAProjects.csv")
projects <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------
# create a header csv with only variable names
csv_path <- here("loader_tables", "CommunityClassification_Header.csv")
template <- read_csv(csv_path, show_col_types = FALSE)

# Tidying CDFW data -------------------------------------------------------
# Separating ClassificationDescription into boolean fields (multivariate analysis, table analsyis, and inspection)

projects <- projects %>%
  mutate(
    method_inspection = str_detect(str_to_lower(ClassificationDescription), "inspection"),
    method_multivar = str_detect(str_to_lower(ClassificationDescription), "multivariate"),
    method_table = str_detect(str_to_lower(ClassificationDescription), "table")
  )

# Assigning columns to loader table ---------------------------------------
# Changing column types to support to data
template <- template %>%
  mutate(
    inspection = as.logical(inspection),
    multivariateAnalysis = as.logical(multivariateAnalysis),
    tableAnalysis = as.logical(tableAnalysis)
  )

CommunityClassifications_LT <- bind_rows(
  template,
  tibble(expertSystem = projects$ClassificationTool,
         inspection = projects$method_inspection,
         multivariateAnalysis = projects$method_multivar,
         tableAnalysis = projects$method_table
  )
)

# All variables besides expertSystem, inspection, multivariateAnalysis, and tableAnalysis were not matched and are left as 'NA'

