library(tidyverse)
library(here)
library(stringr)

# load in CDFW data -------------------------------------------------------
# RAClassification
csv_path <- here("data", "RAClassification.csv")
classification <- read_csv(csv_path, show_col_types = FALSE)

# loading CA lookup tables
csv_path <- here("data", "MCV2019_Alliance.csv")
alliances <- read_csv(csv_path, show_col_types = FALSE)
csv_path <- here("data", "MCV2019_Association.csv")
associations <- read_csv(csv_path, show_col_types = FALSE)
csv_path <- here("data", "MCV2019_Group.csv")
groups <- read_csv(csv_path, show_col_types = FALSE)
csv_path <- here("data", "MCV2019_Macrogroup.csv")
macrogroups <- read_csv(csv_path, show_col_types = FALSE)

# creating loader table ---------------------------------------------------
# create a header csv with only variable names
csv_path <- here("loader_tables", "CommunityConcept_Header.csv")
template <- read_csv(csv_path, show_col_types = FALSE)

# Checking values ---------------------------------------------------------
# Checking that each community name is contained within the MCV names for it's community level
# There are values for each level that do not exactly match the MCV names.

# Alliances
classification %>%
  # making sure that nothing gets flagged for a capitalization difference
  mutate(Alliance = str_to_upper(Alliance)) %>%
  filter(!Alliance %in% alliances$Alliance) %>%
  distinct(Alliance)
# Associations
classification %>%
  mutate(Association = str_to_upper(Association)) %>%
  filter(!Association %in% associations$Association) %>%
  distinct(Association)
# Groups/Macrogroups
classification %>%
  mutate(MCVAboveAlliance = str_to_upper(MCVAboveAlliance)) %>%
  filter(!MCVAboveAlliance %in% c(str_to_upper(groups$Group), str_to_upper(macrogroups$Macrogroup))) %>%
  distinct(MCVAboveAlliance)

# Checking that the community levels are in the list of accepted community levels.
# There are some values that are not shown in the list of community levels.
classification %>%
  mutate(ClassificationLevel = str_to_upper(ClassificationLevel)) %>%
  filter(!ClassificationLevel %in% 
         c("ALLIANCE", "ASSOCIATION", "PROVISIONAL ALLIANCE", "PROVISIONAL ASSOCIATION", "PROVISONAL SEMI-NATURAL ALLIANCE", "PROVISIONAL SEMI-NATURAL ASSOCIATION", "SEMI-NATURAL ALLIANCE", "SEMI-NATURAL ASSOCIATION", "GROUP", "MACROGROUP")) %>%
  distinct(ClassificationLevel)

# Checking if any records with a CaCode value do not have alliance or association information.
# There are records with CaCodes that do not have alliance or assocation info, may need to add to VegBank as new communities.
classification <- classification %>%
  mutate(
    CaCode = str_squish(as.character(CaCode)),
    CaCode = if_else(str_to_lower(CaCode) %in% c("", "na", "n/a", "none", "null"),
                     NA_character_, CaCode)
  )
classification %>%
  filter(!is.na(CaCode) & is.na(Alliance) & is.na(Association))

# Tidying CDFW data -------------------------------------------------------
# The most specific level available will be assigned to commName.

classification <- classification %>%
  mutate(
    commName = coalesce(Association, Alliance, MCVAboveAlliance)
  )

# Assigning columns to loader table ---------------------------------------
comm_LT <- bind_rows(
  template,
  tibble(commName = classification$commName,
         commLevel = classification$ClassificationLevel)
)

# All variables besides commName and commLevel were not matched and are left as 'NA'

