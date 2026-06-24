# this script uploads new community concepts from the MCV to vegbank that can then be applied to community classifications

library(vegbankr)
library(dplyr)
vb_set_base_url("https://api-dev.vegbank.org")
vb_set_token(tokens = token)

community_concepts <- read.csv("data/loader-tables/communityConceptsLT.csv")
community_names <- read.csv("data/loader-tables/communityNamesLT.csv")

ref <- data.frame(user_rf_code = "MCV - CDFW CNPS",
                  short_name = "MCV - CDFW CNPS",
                  full_citation = "CNPS. 2026. A Manual of California Vegetation, Online Edition. http://www.cnps.org/cnps/vegetation/; searched on 2026-02-06.",
                  url = "http://www.cnps.org/cnps/vegetation/")

party <- data.frame(user_py_code = "CDFW CNPS",
                    organization_name = "California Native Plant Society (CNPS) and California Department of Fish and Wildlife (CDFW)")

vb_upload("community-concepts",
          community_concepts = community_concepts,
          community_names = community_names,
          references = ref,
          parties = party,
          dry_run = FALSE)
