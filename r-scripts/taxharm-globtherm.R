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
    
    if (nrow(gbif) > 0) { # If there is an accepted match, put this info in the dataframe 
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

### Globtherm harmonization -- Celeste and Mya ----
library(readr)
globTherm_processed <- read_csv("data-processed/globTherm_processed.csv")
View(globTherm_processed)

#Creating new column joining species and genus columns
globTherm_processed2 <- globTherm_processed %>% 
mutate(genus_species = if_else(is.na(species), genus, paste(genus, species, sep = "_")))
View(globTherm_processed2)

##double check that genus species has the upper and lower case correct spelling + remove any extra characters 
globtherm <- globTherm_processed2 %>%
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
globtherm2 <- globtherm %>% 
  filter(str_count(genus_species, "\\w+") == 2)

#subset of unique species 
subset_globtherm <- unique(globtherm2$genus_species) # 1184 unique species names

# harmonize subset_globtherm list in sets of 5 (takes ~ 11.16 minutes per list)
list1b <- subset_globtherm[1:500]
harm_globtherm1 <- harmonize(list1b)

list2b <- subset_globtherm[501:1000]
harm_globtherm2 <- harmonize(list2b)

list3b <- subset_globtherm[1001:length(subset_globtherm)] # from row 1001 : end of the list
harm_globtherm3 <- harmonize(list3b)

##rbind to stack all the dataframes together "on top" of eachother
harmonized_globtherm <- do.call("rbind", list(harm_globtherm1, harm_globtherm2, harm_globtherm3))

# Merge dataset with species name key for the purpose of matching with other datasets by ID
merge_df_with_key <- function(df, df_sp_key) {
  df_sp_key <- df_sp_key %>% distinct()
  df_key_merge <- df %>%
    left_join(df_sp_key, by = c("genus_species" = "input_name"), relationship = "many-to-many") %>%
    mutate(
      sp_id = coalesce(acc_id, syn_id, genus_species),
      sp_name_for_matching = coalesce(acc_name, syn_name, genus_species),
      match_source = case_when(
        !is.na(acc_id) ~ "accepted",
        !is.na(syn_id) ~ "synonym",
        TRUE ~ "original"
      )
    ) %>%
    select(-acc_name, -syn_name, -acc_id, -syn_id)
  
  return(df_key_merge)
}

mergedb <- merge_df_with_key(globtherm, harmonized_globtherm)

# returns TRUE,  means both columns contain the same values, regardless of order.
setequal(mergedb$genus_species, mergedb$sp_name_for_matching)

#kingdom: animalia only 
unique(mergedb$kingdom)

harmonized_globtherm_processed <- mergedb %>%
  rename(input_name = genus_species,
         genus = genus.y,
         th_genus_species = sp_name_for_matching) %>% #th stands for the taxonomic harmonized species
  select(-genus.x, -match_source, -kingdom, -db) %>%
  select(1:4, region, th_genus_species, sp_id, input_name, phylum, order, class, family, genus, species, everything())

write_csv(harmonized_globtherm_processed, "data-processed/harmonized_globtherm.csv")
