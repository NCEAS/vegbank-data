# vegbank-data

This repository processes vegetation plot data from the CDFW Vegcamp project and uploads it to VegBank.

## Overview

There are two main steps:
1. **Transform** data from CDFW format (e.g., RAPlots, RAClassification) to the VegBank loader table format
2. **Validate and upload** data to VegBank

For more information on the VegBank upload process, see the [data upload vignette](https://nceas.github.io/vegbankr/articles/upload.html) on the [`vegbankr`](https://nceas.github.io/vegbankr/) website.

## Main Scripts

- **`transform-input-data.R`** - Reads raw data files from your input directory and transforms them into VegBank loader table format. This handles field mapping, data cleaning, and normalization. This script sources and runs all of the functions in the `R/` directory, which read from both the specified input data and the lookup tables in `data/lookup-tables`.
  
- **`upload-plot-observations.R`** - Validates and uploads plot observation data (including plots, projects, parties, soils, disturbances, strata, and species data) to VegBank. Reads from the loader table CSVs written by `transform-input-data.R`.

- **`upload-community-classifications.R`** - Uploads community classification data for existing plots to VegBank. Reads from the loader table CSVs written by `transform-input-data.R`.

## Typical Workflow

1. Run `transform-input-data.R` to convert your raw data into loader tables
2. Examine the output and validation messages
3. Debug input data, code, and/or lookup tables as needed
4. Re-run `transform-input-data.R` with corrected data/code/lookup tables
5. Repeat steps 2-4 until validation in `upload-plot-oservations.R` step passes
6. Run `upload-plot-observations.R` to upload plot data to VegBank
7. (Optional) Run `upload-community-classifications.R` if uploading classifications to existing plots
