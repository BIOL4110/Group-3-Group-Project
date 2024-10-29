#taxonomic harmonization
#oct - 09- 2024

source("r-scripts/01-data-cleaning.R")

library(taxadb)
library(dplyr)
library(tidyverse)
library(parallel)
library(furrr)
library(taxize)

# Load taxonomic databases
load_databases <- function(){
  td_create("gbif")
}

load_databases()

#without fishbase/slb
# load_databases <- function(){
td_create("gbif")
td_create("fb")
}

# Capitalize genus in species name
capitalize <- function(x){
  first <- toupper(substr(x, start=1, stop=1)) # capitalize first letter
  rest <- tolower(substr(x, start=2, stop=nchar(x)))   # make everything else lowercase
  paste0(first, rest)
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

##filter out anything that is just genus 
library(stringr)
## filters genus_species for those that have both words
biotime2 <- biotime %>% 
  filter(str_count(genus_species, "\\w+") == 2)

unique(biotime2$genus_species)

# set function to Harmonize the genus_species column using taxize
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
      }))
  return(harmonized_df)
}

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
    } else {
      not_found <- c(not_found, name) # Add to not_found if no match found
    }
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
    } else {
      # If no synonym match, append to names_db with input_name only
      names_db <- bind_rows(names_db, data.frame(
        input_name = name,
        db = NA,
        acc_id = NA,
        acc_name = NA,
        kingdom = NA,
        phylum = NA,
        order = NA,
        class = NA,
        family = NA,
        genus = NA,
        syn_id = NA,
        syn_name = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(names_db) # Return the dataframe with matches and unmatched names
}

## Error in FUN(left, right) : non-numeric argument to binary operator

harmonize2 <- function(names) {
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

biotime_preview <- head(biotime2, 200)
harm_biotime <- harmonize2(biotime_preview) ## full species harmonization -dont run!!

#subset of unique species 
subset_biotime <- unique(biotime2$genus_species)

list1 <- subset_biotime[1:500]
harm_biotime <- harmonize2(list1)

list2 <- subset_biotime[501:1000]

list3 <- subset_biotime[1001:1500]
list4 <- subset_biotime[1501:2000]
list5 <- subset_biotime[2001:length(subset_biotime)]


##NEXT rbind to stack on top FOR HARM
## left join to biotime by input_name (rename genus_species) - or the column input name is equal to genus sopecies
## then Nehas code to join into biotime - on slack 

harm_biotime <- harmonize2(subset_biotime)

# Batch processing harmonization function with 
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
    harmonized_batch <- harmonize_taxonomy2(batch)
    
    # Store the result in the list
    results_list[[i]] <- harmonized_batch
    
    # Optional: Add a delay to avoid API rate limits if needed
    Sys.sleep(2)
  }
  
  # Combine all batches back into a single dataframe
  harmonized_df <- do.call(rbind, results_list)
  
  return(harmonized_df)
}

harm_biotime_batches <- harmonize_in_batches2(biotime, batch_size = 2) ##make size smaller ran for over 1 hour

harmonize_taxonomy3 <- function(df) {
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
      })
    )
  
  return(harmonized_df)
}

harmonize_in_batches2 <- function(df, batch_size = 5) {
  # Split the dataframe into batches
  batches <- split(df, ceiling(seq_along(df$genus_species) / batch_size))
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop through each batch
  for (i in seq_along(batches)) {
    cat("Processing batch", i, "of", length(batches), "\n")  # To track progress
    batch <- batches[[i]]
    
    # Harmonize the current batch
    harmonized_batch <- harmonize_taxonomy3(batch)
    
    # Store the result in the list
    results_list[[i]] <- harmonized_batch
    
    # Optional: Add a delay to avoid API rate limits if needed
    Sys.sleep(2)
  }
}


harmonize_taxonomy5 <- function(df) {
  # Clean up genus_species before processing
  df <- df %>%
    mutate(genus_species = str_squish(genus_species))  # Remove extra spaces
  
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

biotime_preview <- head(biotime, 200)

harm_preview <- harmonize_taxonomy5(biotime_preview)
##everything returns NA possibly bc of the way the column genus_species has a space between?
#how is the harmonization code not working based on what is defined and how the genus_species column is laid out?

### GLOBTHERM - Celeste ----
head(globTherm_processed)- c-

### looking at neha's approach ----
# https://files.slack.com/files-pri/T0506EF6KLJ-F07P9U7QYH1/taxadb_functions.r
# Input a list of species names to search them in gbif, itis, and col
# Results in a dataframe that will act as a key for the species in a dataset with corresponding IDs and matching names

## error stuff 
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


#inner join to look at how many species overlap between datasets         
overlap <- inner_join(gl_sp_all, ufish_sp_list, by = "Scientific_Name")