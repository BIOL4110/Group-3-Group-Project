#taxonomic harmonization
#oct - 09- 2024

source("r-scripts/01-data-cleaning.R")

### looking at neha's approach ----
# https://files.slack.com/files-pri/T0506EF6KLJ-F07P9U7QYH1/taxadb_functions.r
# Set a timeout longer than 60 seconds in case a database connection fails
options(timeout = 120)

# Load taxonomic databases
load_databases <- function(){
  td_create("gbif")
  td_create("itis")
  td_create("col")
  td_create("slb")
  td_create("fb")
}

##double check that genus species has the upper and lower case correct spelling
