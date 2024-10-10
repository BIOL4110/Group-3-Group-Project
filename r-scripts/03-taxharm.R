#taxonomic harmonization
#oct - 09- 2024

source("r-scripts/01-data-cleaning.R")

### looking at neha's approach ----
# https://files.slack.com/files-pri/T0506EF6KLJ-F07P9U7QYH1/taxadb_functions.r
make_sp_list <- function(df) {
  sp <- unique(df$genus_species) # Get unique species names in the dataframe
  has_subsp <- sp[grepl("^\\w+ \\w+ \\w+$", sp)] # Find species names that are at a subspecies level
  # This analysis is only considering taxa at a species level, so names in other datasets only have to match at a species level
  # Both levels will be queried in case other datasets have data for the same subspecies
  
  genus_ep_only <- sapply(has_subsp, function(x) { # Take subspecies names and convert them to species level
    word_list <- strsplit(x, " ")[[1]]
    paste(word_list[1:2], collapse = " ") # Only keep first two words, which should be genus and epithet
  })
  
  gt_sp_combined <- unique(c(sp, genus_ep_only))
  
  return(gt_sp_combined)
}

# Load taxonomic databases
load_databases <- function(){
  td_create("gbif")
  td_create("itis")
  td_create("col")
}