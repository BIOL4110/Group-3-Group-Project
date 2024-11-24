#
# Synonym checking will need to make use of synonyms(species_list) from the rfishbase library
#
# Load necessary libraries
library("rfishbase")
library(readr)
library(dplyr)

# Load data
biotime <- read.csv("data-processed/BioTime_processed.csv")

# Clean species names in biotime to match format in rfishbase (remove underscores)
biotime <- biotime %>%
  mutate(species_clean = gsub("_", " ", genus_species))

# Create species_list from unique values in the species_clean column
species_list <- unique(biotime$species_clean)

synonyms(species_list) 

# Get table with species occurences
ecoltbl <- ecology(species_list)

# Filter species that are marine (Netric or Oceanic)
marine_species <- ecoltbl %>%
  filter(Neritic == -1 | Oceanic == -1) %>%
  select(Species)

# Keep only marine species in species_list
species_list <- marine_species$Species

# Create table of Tmax and Tmin for each species
temptbl <- stocks(species_list, c("TempMin", "TempMax", "Species"))

# Keep only the first occurrence for each Species
temptbl <- temptbl %>%
  distinct(Species, .keep_all = TRUE)

# Join the temperature data with biotime and store the result in btfb_with_NA
btfb_with_NA <- biotime %>%
  left_join(temptbl, by = c("species_clean" = "Species"))

# Filter out rows with NAs in TempMin or TempMax
btfb_without_NA <- btfb_with_NA %>%
  filter(!is.na(TempMin) & !is.na(TempMax))

# Save new tables to CSVs
write.csv(btfb_with_NA, "data-processed/btfb_with_NA.csv", row.names = FALSE)
write.csv(btfb_without_NA, "data-processed/btfb_without_NA.csv", row.names = FALSE)