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
write.csv(btfb_with_NA, "data-processed/btfb_with_NA.csv", row.names = FALSE)
write.csv(btfb_without_NA, "data-processed/btfb_without_NA.csv", row.names = FALSE)
