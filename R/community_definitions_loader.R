

# example data for CommunityConcepts table

#$user_cc_code        <chr> "21.100.00" (CaCode column)
#$ name                <chr> ""Abronia latifolia – Ambrosia chamissonis" (Alliance or Association column depending on file)
#$ user_rf_code        <chr> MVC 2019" (same for all rows) 
#$ user_status_rf_code <chr> "MVC 2019" (same for all rows) 
#$ comm_concept_status <chr> "accepted" (same for all rows)
#$ user_parent_cc_code <chr> NA
#$ comm_level          <chr> "association" (normalized from the MVC level column)
#$ start_date          <date> 2019-01-01 same for all rows
#$ vb_status_py_code   <chr> "py.512" (same for all rows)

# example data for CommunityNames table
# for each row in the table above, there are two rows in the communityNames table

# row 1: scientific name
#$ user_cc_code     <chr> "21.100.00" (CaCode column)
#$ name_type        <chr> "Scientific" (same for all rows)
#$ name             <chr> "Abronia latifolia – Ambrosia chamissonis" (Alliance or Association column depending on file)
#$ name_status      <chr> "Standard" (same for all rows)
#$ usage_start      <date> 2019-01-01 same for all rows
#$ vb_usage_py_code <chr> "py.512" (same for all rows)

# row 2: code
#$ user_cc_code     <chr> "21.100.00" (CaCode column)
#$ name_type        <chr> "Code" (same for all rows)
#$ name             <chr> "21.100.00" (CaCode column)
#$ name_status      <chr> "Standard" (same for all rows)
#$ usage_start      <date> 2019-01-01 same for all rows
#$ vb_usage_py_code <chr> "py.512" (same for all rows)

community_definitions_loader <- function(in_dir, out_dir){
  alliance <- read.csv(file.path(in_dir, "lookup-tables/MCV2019_Alliance.csv"))
  association <- read.csv(file.path(in_dir, "lookup-tables/MCV2019_Association.csv"))
  # this is the data we need to model to the form above
  mcv <- bind_rows(alliance, association)
}

