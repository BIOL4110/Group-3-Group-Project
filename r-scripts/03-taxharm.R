#taxonomic harmonization
#oct - 09- 2024

source("r-scripts/01-data-cleaning.R")

#loading in packages -----
library(taxadb) # Creates a local database of many commonly used taxonomic authorities and provides functions that can quickly query this data
library(dplyr)
library(tidyverse)
library(parallel) # Support for parallel computation, including by forking and random-number generation.
library(furrr) # combine purrr's family of mapping functions with future's parallel processing capabilities.
library(taxize) # allows users to search over many taxonomic data sources for species names.
library(stringr)

# setting functions, do not need to copy again -----
# code source: neha patel 
# Load taxonomic databases - gbif only - fb/slb were not working 
load_databases <- function(){
  td_create("gbif")
}

# Capitalize genus in species name 
capitalize <- function(x){
  first <- toupper(substr(x, start=1, stop=1)) # capitalize first letter
  rest <- tolower(substr(x, start=2, stop=nchar(x)))   # make everything else lowercase
  paste0(first, rest)
}

# Input a list of species names to search them in gbif
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
  
  # Loop through the list, begin with searching the name in gbif
  for (name in names) {
    gbif <- filter_name(name, "gbif") %>%
      filter(taxonomicStatus == "accepted")
    
    if (nrow(gbif) > 0) { # If there is an accepted match, put this info in the dataframe # nolint: line_length_linter.
      gbif <- gbif %>%
        mutate(db = "gbif", input_name = name) %>%
        dplyr::select(input_name, db, acceptedNameUsageID, scientificName,
                      kingdom, phylum, order, class, family, genus) %>%
        rename(acc_name = scientificName, acc_id = acceptedNameUsageID)
      
      names_db <- bind_rows(names_db, gbif)
      next # If there is no match, go on to next search
    }
    
    not_found <- c(not_found, name)
  }
  
  syn_not_found <- c()
  
  # Loop through the list, begin with searching the name in gbif
  for (name in not_found) {
    gbif <- filter_name(name, "gbif") %>%
      filter(taxonomicStatus == "synonym")
    
    if (nrow(gbif) > 0) { # If there is an accepted match, put this info in the dataframe
      gbif <- gbif %>%
        mutate(db = "gbif", input_name = name) %>%
        dplyr::select(input_name, db, acceptedNameUsageID, scientificName,
                      kingdom, phylum, order, class, family, genus) %>%
        rename(syn_name = scientificName, syn_id = acceptedNameUsageID)
      
      names_db <- bind_rows(names_db, gbif)
      next # If there is no match, go on to next search
    }
    name <- data.frame(name) %>% rename(input_name = name)
    names_db <- bind_rows(names_db, name)
  }
  
  return(names_db) # return the dataframe with the accepted id's and the list of species without matches
}

### BIOTIME - Ije  -----
head(BioTime_processed)
##double check that genus species has the upper and lower case correct spelling + remove any extra characters 
biotime <- BioTime_processed %>%
  mutate(genus_species = str_replace_all(genus_species, "_", " "), # replace _ with a space
         genus_species = str_replace_all(genus_species, "<.*>?>", ""), # remove any <> characters
         genus_species = str_replace_all(genus_species, "  ", " "),
         genus_species = str_replace_all(genus_species, " spp.", ""), 
         genus_species = str_replace_all(genus_species, " spp", ""),
         genus_species = str_replace_all(genus_species, " sp", ""),
         genus_species = str_replace_all(genus_species, " sp.", ""),
         genus_species = str_replace_all(genus_species, " unknown", ""),
         genus_species = str_squish(genus_species), # remove white spaces
         genus_species = sapply(genus_species, capitalize)) # capitalize genus

## filters genus_species to select those that have 2 words
biotime2 <- biotime %>% 
  filter(str_count(genus_species, "\\w+") == 2)

#subset of unique species 
subset_biotime <- unique(biotime2$genus_species) # 2318 unique species names

# harmonize subset_biotime list in sets of 5 (takes ~ 11.16 minutes per list)
list1 <- subset_biotime[1:500]
harm_biotime1 <- harmonize(list1)

list2 <- subset_biotime[501:1000]
harm_biotime2 <- harmonize(list2)

list3 <- subset_biotime[1001:1500]
harm_biotime3 <- harmonize(list3)

list4 <- subset_biotime[1501:2000]
harm_biotime4 <- harmonize(list4)

list5 <- subset_biotime[2001:length(subset_biotime)] # from row 2001 : end of the list
harm_biotime5 <- harmonize(list5)

##NEXT rbind to stack on top FOR HARM
## left join to biotime by input_name (rename genus_species) - or the column input name is equal to genus sopecies
## then Nehas code to join into biotime - on slack 


### GLOBTHERM - Celeste ----
head(globTherm_processed)

## Merging together - Ije -----

#inner join to look at how many species overlap between datasets         
overlap <- inner_join(gl_sp_all, ufish_sp_list, by = "Scientific_Name")