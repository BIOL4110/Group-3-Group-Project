##01-data-wrangling
#cleaning biotime and fishbase datasets + tax harmonization - IN/SB
# Oct - 09 - 2024 

##adding in BioTime datasets
library(janitor)
library(readr)
library(tidyverse)
library(stringr)
library(dplyr)

## this data is actually pulled from the OSF project
studies_data <- read_csv("data-raw/biotime.csv") %>%
  clean_names()

#Biotime cleaning ----
#1963 - 2014
#filter out unwanted columns + anything where abundance is = 0 or NA
temperate_latitudes_df <- studies_data %>%
  filter(latitude > 23.5 & latitude <= 66.5) %>%
  filter(!is.na(abundance) & abundance > 0) %>%
  select(-biomas, 
         -plot,
         -depth,
         -sample_desc,
         -study_id)

ggplot(temperate_latitudes_df, aes(x = year, y = abundance)) + geom_col() +
  labs(title = "Total Abundance Data over Years in Temperate Latitudes",
       x = "Year", y = "Abundance") +
  theme_minimal()

#2001-2010, 
## fix the years so that they all show 
tropical_latitudes_df <- studies_data %>%
  filter(latitude >= 0 & latitude <= 23.5) %>%
  filter(!is.na(abundance) & abundance > 0) %>%
  mutate(year = as.factor(year)) %>% 
  dplyr::select(-biomas, -plot, -depth, -sample_desc, -study_id)

ggplot(tropical_latitudes_df, aes(x = year, y = abundance)) + geom_col() +
  labs(title = "Total Abundance Data over Years in Tropical Latitudes",
       x = "Year", y = "Abundance") +
  theme_minimal()

#sep genus and species into separate columns + make new joined on
trop_df <- tropical_latitudes_df %>%
  separate(genus_species, into = c("genus", "species"), sep = " ", fill = "right") %>%
  mutate(genus_species = if_else(is.na(species), genus, paste(genus, species, sep = "_"))) %>%
  mutate(region = "Tropical",
         year = as.numeric(as.character(year)))

temp_df <- temperate_latitudes_df %>%
  separate(genus_species, into = c("genus", "species"), sep = " ", fill = "right") %>%
  mutate(genus_species = if_else(is.na(species), genus, paste(genus, species, sep = "_"))) %>%
  mutate(region = "Temperate",
         year = as.numeric(as.character(year)))

BioTime_processed <- bind_rows(trop_df, temp_df)

#write_csv(BioTime_processed, "data-processed/BioTime_processed.csv")

#Biotime Tax Harm -----
library(taxadb) # Creates a local database of many commonly used taxonomic authorities and provides functions that can quickly query this data
library(parallel) # Support for parallel computation, including by forking and random-number generation.
library(furrr) # combine purrr's family of mapping functions with future's parallel processing capabilities.
library(taxize) # allows users to search over many taxonomic data sources for species names.
library(stringr)

# setting functions -  code source: neha patel 
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

##rbind to stack all the dataframes together "on top" of eachother
harmonized_biotime <- do.call("rbind", list(harm_biotime1, harm_biotime2, harm_biotime3, harm_biotime4, harm_biotime5))

merged <- merge_df_with_key(biotime, harmonized_biotime)

# returns TRUE,  means both columns contain the same values, regardless of order.
setequal(merged$genus_species, merged$sp_name_for_matching)

#kingdom: animalia only 
unique(merged$kingdom)

harmonized_biotime_processed <- merged %>%
  rename(input_name = genus_species,
         genus = genus.y,
         th_genus_species = sp_name_for_matching) %>% #th stands for the taxonomic harmonized species
  select(-genus.x, -match_source, -kingdom, -db) %>%
  select(1:4, region, th_genus_species, sp_id, input_name, phylum, order, class, family, genus, species, everything())

# write_csv(harmonized_biotime_processed, "data-processed/harmonized_biotime.csv")

# compare accuracy in genus species names from th_genus_species and input_name 
{
  harmonized_biotime_processed2 <- harmonized_biotime_processed %>%
    mutate(is_match = input_name == th_genus_species)
  
  # calculate accuracy as the proportion of rows where genus_species matches th_genus_species
  accuracy <- mean(harmonized_biotime_processed2$is_match)
  print(paste("Accuracy:", accuracy * 100, "%"))
  }
#fishbase + tax harm -----
# Load necessary libraries
library("rfishbase")
library(readr)
library(dplyr)

# Load data
biotime <- read.csv("data-processed/BioTime_processed.csv")

# Clean species names in biotime to match format in rfishbase (remove underscores)
biotime <- biotime %>%
  mutate(species_clean = gsub("_", " ", genus_species))

# Create species_list from unique values in biotime$species_clean
species_list <- unique(biotime$species_clean)

# Create table of all synonyms on FishBase for names in BioTime
specsynon <- synonyms(species_list)

# Match BioTime names to FishBase accepted names
synonym_mapping <- specsynon %>%
  group_by(synonym) %>%
  filter(if (any(Status == "accepted name", na.rm = TRUE)) Status == "accepted name" else Status == "synonym") %>%
  ungroup() %>%
  filter(!is.na(Status)) %>%
  select(biotime_name = synonym, fishbase_name = Species) %>%
  distinct(fishbase_name, .keep_all = TRUE)

# Get table with species occurences
ecoltbl <- ecology(synonym_mapping$fishbase_name)

# Filter for species that are marine (Neritic or Oceanic)
marine_species <- ecoltbl %>%
  filter(Neritic == -1 | Oceanic == -1) %>%
  select(Species)

# Update species_list to include only marine species
species_list <- marine_species$Species

# Create table of Tmax and Tmin for each species
temptbl <- stocks(species_list, c("TempMin", "TempMax", "Species"))

# Remove rows with NA values in temptbl$TempMin or temptbl$TempMax, then keep only the first occurrence if duplicates exist
temptbl <- temptbl %>%
  filter(!is.na(TempMin) & !is.na(TempMax)) %>%
  distinct(Species, .keep_all = TRUE)  

# Join temptbl to synonym_mapping based on Species and fishbase_name
synonym_mapping <- synonym_mapping %>%
  left_join(temptbl, by = c("fishbase_name" = "Species"))

# Join the temperature data with biotime and store the result in btfb_with_NA
btfb_with_NA <- biotime %>%
  left_join(synonym_mapping, by = c("species_clean" = "biotime_name"))

# Organize & clean table
btfb_with_NA <- btfb_with_NA %>%
  select(-species_clean) %>%
  rename(temp_min = TempMin, temp_max = TempMax) %>%
  select(abundance:genus_species, fishbase_name, everything())

# Filter out rows with NAs in temptbl$temp_min or temptbl$temp_max
btfb_without_NA <- btfb_with_NA %>%
  filter(!is.na(temp_min) & !is.na(temp_max))

# Save new tables to CSVs
#write.csv(btfb_with_NA, "data-processed/btfb_with_NA.csv", row.names = FALSE)
#write.csv(btfb_without_NA, "data-processed/btfb_without_NA.csv", row.names = FALSE)
