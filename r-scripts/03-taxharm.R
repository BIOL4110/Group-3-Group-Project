#taxonomic harmonization
#oct - 09- 2024

source("r-scripts/01-data-cleaning.R")

library(taxadb)
library(dplyr)
library(tidyverse)
library(parallel)
library(furrr)
library(taxize)


### looking at neha's approach ----
# https://files.slack.com/files-pri/T0506EF6KLJ-F07P9U7QYH1/taxadb_functions.r
# Set a timeout longer than 60 seconds in case a database connection fails
options(timeout = 200)

# Load taxonomic databases
load_databases <- function(){
  td_create("gbif")
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

# Harmonize the taxonomy using taxize
harmonize_taxonomy2 <- function(df) {
  harmonized_df <- df %>%
    mutate(
      gbif_id = sapply(genus_species, function(name) {
        tryCatch({
          res <- get_gbifid(name, rows = 1)  # Query GBIF for species
          if (length(res$gbifid) > 0) {
            return(res$gbifid)
          } else {
            return(NA)  # Return NA if no match found
          }
        }, error = function(e) {
          return(NA)  # Handle errors
        })
      }),
      slb_id = sapply(genus_species, function(name) {
        tryCatch({
          res <- get_slbid(name, rows = 1)  # Query SLB for species
          if (length(res$slbid) > 0) {
            return(res$slbid)
          } else {
            return(NA)
          }
        }, error = function(e) {
          return(NA)
        })
      }),
      fb_id = sapply(genus_species, function(name) {
        tryCatch({
          res <- get_fbid(name, rows = 1)  # Query FishBase for species
          if (length(res$fbid) > 0) {
            return(res$fbid)
          } else {
            return(NA)
          }
        }, error = function(e) {
          return(NA)
        })
      })
    )
  
  return(harmonized_df)
}

harm_biotime <- harmonize_taxonomy2(biotime)

# Harmonize the taxonomy using taxize
harmonize_taxonomy2 <- function(df) {
  harmonized_df <- df %>%
    mutate(
      gbif_id = sapply(genus_species, function(name) {
        tryCatch({
          res <- get_gbifid(name, rows = 1)  # Query GBIF for species
          if (length(res$gbifid) > 0) {
            return(res$gbifid)
          } else {
            return(NA)  # Return NA if no match found
          }
        }, error = function(e) {
          return(NA)  # Handle errors
        })
      }),
      slb_id = sapply(genus_species, function(name) {
        tryCatch({
          res <- get_slbid(name, rows = 1)  # Query SLB for species
          if (length(res$slbid) > 0) {
            return(res$slbid)
          } else {
            return(NA)
          }
        }, error = function(e) {
          return(NA)
        })
      }),
      fb_id = sapply(genus_species, function(name) {
        tryCatch({
          res <- get_fbid(name, rows = 1)  # Query FishBase for species
          if (length(res$fbid) > 0) {
            return(res$fbid)
          } else {
            return(NA)
          }
        }, error = function(e) {
          return(NA)
        })
      })
    )
  
  return(harmonized_df)
}

harm_biotime <- harmonize_taxonomy2(biotime)

# Batch processing harmonization function
harmonize_in_batches <- function(df, batch_size = 20) {
  # Split the dataframe into batches
  batches <- split(df, ceiling(seq_along(df$genus_species) / batch_size))
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop through each batch
  for (i in seq_along(batches)) {
    cat("Processing batch", i, "of", length(batches), "\n")  # To track progress
    batch <- batches[[i]]
    
    # Harmonize the current batch
    harmonized_batch <- harmonize_taxonomy(batch)
    
    # Store the result in the list
    results_list[[i]] <- harmonized_batch
    
    # Optional: Add a delay to avoid API rate limits if needed
    Sys.sleep(2)
  }
  
  # Combine all batches back into a single dataframe
  harmonized_df <- do.call(rbind, results_list)
  
  return(harmonized_df)
}

harm_biotime_batches <- harmonize_in_batches(biotime, batch_size = 50)


# Input a list of species names to search them in gbif, itis, and col
# Results in a dataframe that will act as a key for the species in a dataset with corresponding IDs and matching names
harmonize <- function(names, time_limit = 120) {
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
    macthed = character(),
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
    return(names_db) 
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
  } else {  # If no synonym is found, add to dataframe as not matched
    name_df <- data.frame(input_name = name, matched = "Not Matched", stringsAsFactors = FALSE)
    names_db <- bind_rows(names_db, name_df)
  }
}

return(names_db)  # Return the dataframe with matching status
}

harmonize_in_batches <- function(sp_list, batch_size = 20) {
  # Some datasets have 1000s of species, so query the databases a few at a time
  sp_batches <- split(sp_list, ceiling(seq_along(sp_list) /batch_size))
  h_batches <- future_map(sp_batches, function(batch) {
    result <- bind_rows(lapply(batch, harmonize))
    Sys.sleep(2)
    return(result)
  })
  
  sp_key <- bind_rows(h_batches)
  return(sp_key)
}

species_names_biotime <- unique(biotime$genus_species)

harmonized_biotime_batches <- harmonize_in_batches(species_names_biotime)

# Harmonize species names for both datasets using the harmonize function
harmonized_biotime <- harmonize(unique(biotime$genus_species))
harmonized_globTherm <- harmonize(unique(globTherm_processed$genus_species))

# Check the results
head(harmonized_biotime)
head(harmonized_globTherm)


#mutate(id = get_ids(Scientific_Name, "gbif")

#inner join to look at how many species overlap between datasets         
ufish_sp_all_overlap <- inner_join(gl_sp_all, ufish_sp_list, by = "Scientific_Name")