##01 - worling on adding datasets
library(traitdata)
library(ggplot2)

#overview of all variables 
data(trait_glossary)

vignette("access-data")
data("globTherm")

#vignette examples
vignette("island-birds") # species richness, identifying species on areas, merge with globTherm identify traits
vignette("migbehav_birds") # understanding different types of migratory behaviour
vignette("morpho-indices") #depict ecological differences between feeding guids and species

##adding in BioTime datasets
library(janitor)
library(readr)
library(tidyverse)
library(stringr)

# JUST NORTHERN HEMISPHERE
data2 <- read_csv("data-raw/studies_data_ije.csv") %>%
  clean_names()

tropicaldata <- data2 %>%
  filter(latitude %in% c(0:23.5))

temperate <- data2 %>%
  filter(latitude %in% c(23.5:66.5))

data <- read_csv("data-raw/studies_data.csv") %>%
  clean_names()
# all of it is - 17.5 to -149 ; SOUTH PACIFIC OCEAN

#separting sample_desc into all the variables
dc <- data %>%
  mutate(sample_desc = str_replace_all(sample_desc, "Fringing_Reef", "Fringing-Reef")) %>%
  select(-latitude, -longitude, -year)%>%
  separate_wider_delim(
    sample_desc, 
    delim = "_", 
    names = c("latitude", "longitude", "LTER", "LTER_num", "Habitat", 
              "Fish", "Transect", "transect_num", "swath", "swath_num", 
              "m", "day", "month", "year"),
    names_sep = "_new_"  # Adds a suffix to disambiguate new columns
  )

#sep genus and species into separate columns + make new joined on
dc2 <- dc %>%
  separate(genus_species, into = c("genus", "species"), sep = " ") %>%
  mutate(genus_species = paste(genus, species, sep = "_")) %>%
  # Remove the prefix from sample_desc columns and repair duplicate names
  rename_with(~ str_replace(., "sample_desc_new_", ""), starts_with("sample_desc_new_")) %>%
  rename_with(make.names, everything())


# GlobTherm -------
##now add in global therm data 
data("globTherm")

# repeat mutating genus and species to be separated by "_"
cl_gt <- globTherm %>%
  clean_names() %>%
  mutate(genus_species=paste(genus, species, sep = "_"))

#quickly joining the datasets together
#by genus there are options
merged_genus <- inner_join(dc2, cl_gt, by = "genus")
# no matches for genus by species
merged_gs <- inner_join(dc2, cl_gt, by = "genus_species")

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


## stuarts code adding to mine ------

#data sorting for tropical diversity
tropical_fish_diversity <- data %>%
  filter(latitude %in% c(-23.5:23.5)) %>%
  #filters out any observations recorded in latitudes outside the tropics (tropic of capricorn to tropic of cancer)
  select(abundance, genus_species, year) %>%
  #limiting the data set to relevant data for finding species richness (R)
  group_by(genus_species,year) %>%
  summarise(total_abundance = sum(abundance, na.rm = FALSE)) %>%
  #the two previous arguments sum abundance counts for all observations by species and year
  ungroup() %>%
  complete(genus_species, year = full_seq(year, 1), fill = list(total_abundance = 0))

#the final two arguments return values of zero for years where species are not observed
#generating a sum of the total number of species present per year
R_trop_fish_div <- tropical_fish_diversity %>%
  filter(total_abundance > 0) %>%
  group_by(YEAR) %>%
  summarise(num_species_present = n_distinct(GENUS_SPECIES))
#plotting species richness in tropical latitudes
R_trop_fish_div %>%
  ggplot(aes(YEAR, num_species_present)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Year") +
  ylab("Number of species (R)") +
  theme_classic()
#running a linear model to test if there is a significant relationship between species richness and year
R_regression_trop <- lm(num_species_present ~ YEAR, R_trop_fish_div)
summary(R_regression_trop)
confint(R_regression_trop,"(Intercept)",level=0.95)
confint(R_regression_trop,"YEAR",level=0.95)
#data sorting for temperate fish diversity
temperate_fish_diversity <- ocean_fish %>%
  filter(LATITUDE %in% c(-66.5:-23.5, 23.5:66.5)) %>%
  #filters out any observations recorded in latitudes outside the tropics (tropic of capricorn to tropic of cancer)
  select(ABUNDANCE, GENUS_SPECIES, YEAR) %>%
  #limiting the data set to relevant data for finding species richness (R)
  group_by(GENUS_SPECIES,YEAR) %>%
  summarise(total_abundance = sum(ABUNDANCE, na.rm = FALSE)) %>%
  #the two previous arguments sum abundance counts for all observations by species and year
  ungroup() %>%
  complete(GENUS_SPECIES, YEAR = full_seq(YEAR, 1), fill = list(total_abundance = 0))
#the final two arguments return values of zero for years where species are not observed
#generating a sum of the total number of species present per year
R_temp_fish_div <- temperate_fish_diversity %>%
  filter(total_abundance > 0) %>%
  group_by(YEAR) %>%
  summarise(num_species_present = n_distinct(GENUS_SPECIES))

#plotting species richness in tropical latitudes
R_temp_fish_div %>%
  ggplot(aes(YEAR, num_species_present)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Year") +
  ylab("Number of species (R)") +
  theme_classic()

#running a linear model to test if there is a significant relationship between species richness and year
R_regression_temp <- lm(num_species_present ~ YEAR, R_temp_fish_div)
summary(R_regression_temp)
confint(R_regression_temp,"(Intercept)",level=0.95)
confint(R_regression_temp,"YEAR",level=0.95)
