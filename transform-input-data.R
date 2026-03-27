library(tidyverse)
library(stringr)
library(cli)
library(DT)
library(downloadthis)
library(vctrs)
library(sf)
library(tigris)
library(rnaturalearth)
library(here)
library(vegbankr)

# load in R functions
file_path <- "R"
r_scripts_source <- list.files(file_path, recursive = FALSE, full.names = TRUE, pattern = "\\.[Rr]$")
invisible(lapply(r_scripts_source, source))

# set directory to read from and write to
# change in_dir to point to your real data
in_dir <- 'data/example-data'
out_dir <- 'data/loader-tables'
if (!dir.exists(here(out_dir))){
  dir.create(here(out_dir))
}

# set vegbank instance to read from, and whether to renew cache
vb_set_base_url("https://api-dev.vegbank.org")

# run the loader functions
project_loader(in_dir, out_dir)
party_contributor_loader(in_dir, out_dir)
disturbance_loader(in_dir, out_dir)
plots_loader(in_dir, out_dir, renew_cache = FALSE)
community_loader(in_dir, out_dir, renew_cache = FALSE, cacode_nvc_lookup = "data/lookup-tables/VegBank_CrosswalkHierarchyMCV.csv")
stratacover_taxon_loader(in_dir, out_dir, renew_cache = FALSE)
stratadefinitions_loader(in_dir, out_dir)
soil_loader(in_dir, out_dir)

