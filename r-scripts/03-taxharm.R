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

#what were working with
head(BioTime_processed)
head(globTherm_processed)

# Capitalize genus in species name
capitalize <- function(x){
  first <- toupper(substr(x, start=1, stop=1)) # capitalize first letter
  rest <- tolower(substr(x, start=2, stop=nchar(x)))   # make everything else lowercase
  paste0(first, rest)
}
##double check that genus species has the upper and lower case correct spelling + remove any extra characters 
biotime <- BioTime_processed %>%
  mutate(genus_species = str_replace_all(genus_species, "_", " "), # replace _ with a space
         genus_species = str_replace_all(genus_species, "<.*>?>", ""), # remove any <> characters
         genus_species = str_replace_all(genus_species, "  ", " "),
         genus_species = str_replace_all(genus_species, " spp.", ""), 
         genus_species = str_replace_all(genus_species, " spp", ""),
         genus_species = str_squish(genus_species), # remove white spaces
         genus_species = sapply(genus_species, capitalize)) # capitalize genus
  #mutate(Scientific_Name = str_to_sentence(Scientific_Name))

head(biotime)

# Input a list of species names to search them in gbif, itis, and col
# Results in a dataframe that will act as a key for the species in a dataset with corresponding IDs and matching names
harmonize <- function(names) {
  # Initialize dataframe
  names_db <- data.frame(
    input_name = character(), # names in original dataset
    db = character(), # database that found a match
    acc_id = character(), # id associated with the accepted name
    acc_name = character(), # names matched in a taxonomic database
    kingdom = character(),
    phylum = character(),
    order = character(),
    class = character(),
    family = character(),
    genus = character(),
    syn_id = character(),
    syn_name = character(),
    stringsAsFactors = FALSE
  )
  
  # Initialize a list to store sp without accepted names found
  not_found <- c()
}

#mutate(id = get_ids(Scientific_Name, "gbif")

#inner join to look at how many species overlap between datasets         
ufish_sp_all_overlap <- inner_join(gl_sp_all, ufish_sp_list, by = "Scientific_Name")