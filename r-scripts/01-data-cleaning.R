##01-data-cleaning
#cleaning globtherm and biotime datasets - IN/SB
# Oct - 09 - 2024 

library(traitdata)
library(ggplot2)

#vignette examples
vignette("island-birds") # species richness, identifying species on areas, merge with globTherm identify traits
vignette("migbehav_birds") # understanding different types of migratory behaviour
vignette("morpho-indices") #depict ecological differences between feeding guids and species

##adding in BioTime datasets
library(janitor)
library(readr)
library(tidyverse)
library(stringr)

studies_data <- read_csv("data-raw/studies_data_ije.csv") %>%
  clean_names()

#1963 - 2014
#filter out unwanted columns + anything where abundance is = 0 or NA
temperate_latitudes_df <- studies_data %>%
  filter(latitude > 23.5 & latitude <= 66.5) %>%
  filter(!is.na(abundance) & abundance > 0) %>%
  select(-biomas, 
         -plot,
         -depth,
         -sample_desc)

ggplot(temperate_latitudes_df, aes(x = year, y = abundance)) + geom_col() +
  labs(title = "Total Abundance Data over Years in Temperate Latitudes",
       x = "Year", y = "Abundance") +
  theme_minimal()

#2001-2010, 
## fix the years so that they all show 
tropical_latitudes_df <- studies_data %>%
  filter(latitude >= 0 & latitude <= 23.5) %>%
  filter(!is.na(abundance) & abundance > 0) %>%
  filter(year >= 2001 & year <= 2010) %>%  # Filter to ensure years are between 2001 and 2010
  mutate(year = as.factor(year)) %>% 
  select(-biomas, 
         -plot,
         -depth,
         -sample_desc,
         -study_id)

ggplot(tropical_latitudes_df, aes(x = year, y = abundance)) + geom_col() +
  labs(title = "Total Abundance Data over Years in Tropical Latitudes",
       x = "Year", y = "Abundance") +
  theme_minimal()

#sep genus and species into separate columns + make new joined on
trop_df <- tropical_latitudes_df %>%
  separate(genus_species, into = c("genus", "species"), sep = " ", fill = "right") %>%
  mutate(genus_species = if_else(is.na(species), genus, paste(genus, species, sep = "_")))

temp_df <- temperate_latitudes_df %>%
  separate(genus_species, into = c("genus", "species"), sep = " ", fill = "right") %>%
  mutate(genus_species = if_else(is.na(species), genus, paste(genus, species, sep = "_")))

## plotting species richness over time ------
# tropical biodiversity
trop_fish_diversity <- trop_df %>%
  select(abundance, genus_species, year) %>%
  #limiting the data set to relevant data for finding species richness (R)
  group_by(genus_species,year) %>%
  summarise(total_abundance = sum(abundance, na.rm = FALSE)) %>%
  #the two previous arguments sum abundance counts for all observations by species and year
  ungroup() %>%
  complete(genus_species, year = full_seq(year, 1), fill = list(total_abundance = 0))

#the final two arguments return values of zero for years where species are not observed
#generating a sum of the total number of species present per year
R_trop_fish_div <- trop_fish_diversity %>%
  filter(total_abundance > 0) %>%
  group_by(year) %>%
  summarise(num_species_present = n_distinct(genus_species))

#plotting species richness in tropical latitudes 
#fix the year so it properly shows all individual years + add title + make line puprple
R_trop_fish_div %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, num_species_present)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Year") +
  ylab("Number of species (R)") +
  theme_classic()

R_trop_fish_div %>%
  mutate(year = as.numeric(year)) %>%  
  ggplot(aes(year, num_species_present)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple") +  
  scale_x_continuous(breaks = seq(min(R_trop_fish_div$year), max(R_trop_fish_div$year), by = 1)) + 
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  xlab("Year") +
  ylab("Number of species (R)") +
  ggtitle("Tropical Fish Species Diversity Over Time") + 
  theme_classic()

# temperate biodiversity
temp_fish_diversity <- temp_df %>%
  select(abundance, genus_species, year) %>%
  #limiting the data set to relevant data for finding species richness (R)
  group_by(genus_species,year) %>%
  summarise(total_abundance = sum(abundance, na.rm = FALSE)) %>%
  #the two previous arguments sum abundance counts for all observations by species and year
  ungroup() %>%
  complete(genus_species, year = full_seq(year, 1), fill = list(total_abundance = 0))

#the final two arguments return values of zero for years where species are not observed
#generating a sum of the total number of species present per year
R_temp_fish_div <- temp_fish_diversity %>%
  filter(total_abundance > 0) %>%
  group_by(year) %>%
  summarise(num_species_present = n_distinct(genus_species))

#plotting species richness in tropical latitudes 
# HOW DO I ADD SLOPE 
R_temp_fish_div %>%
  ggplot(aes(year, num_species_present)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkgreen") +
  xlab("Year") +
  ylab("Number of species (R)") +
  ggtitle("Temperate Fish Species Diversity Over Time") + 
  theme_classic()

library(ggpmisc)

R_temp_fish_div %>%
  ggplot(aes(year, num_species_present)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkgreen") +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +  # Add slope and R^2 to the plot
  xlab("Year") +
  ylab("Number of species (R)") +
  ggtitle("Temperate Fish Species Diversity Over Time") + 
  theme_classic()

## now add in global therm data ----
data(trait_glossary)
vignette("access-data")

data("globTherm")

columns_keep <- c("genus", "species","n_3", "tmax","tmax_2","lat_max","long_max", "n_22", "tmin", "n_29", "tmin_2", "lat_min", "long_min","phylum", "class", "order", "family", "scientific_name_std")

cl_gt <- globTherm %>%
  clean_names() %>%
  select(all_of(columns_keep)) %>%
  mutate(across(
    !c(genus, species, phylum, class, order, family, scientific_name_std),  # Exclude these columns
    as.numeric
  ))

##merge all the n, tmin, tmax together where the NAs of one match up with values from another 
cl_gt2 <- cl_gt %>%
  mutate(
    n = coalesce(n_3, n_22, n_29),              # Merge n_3, n_22, n_29 into 'n'
    tmax = coalesce(tmax, tmax_2),              # Merge tmax and tmax_2 into 'tmax'
    tmin = coalesce(tmin, tmin_2),              # Merge tmin and tmin_2 into 'tmin'
    lat = coalesce(lat_max, lat_min),           # Merge lat_max and lat_min into 'lat'
    long = coalesce(long_max, long_min)         # Merge long_max and long_min into 'long'
  ) %>%
  select(-n_3, -n_22, -n_29, -tmax_2, -tmin_2, -lat_max, -lat_min, -long_max, -long_min)

#subsetted globTherm data (Tmax, Tmin, lat, long, n) for tropical / temperate latitudes
tropical_therm <- cl_gt2 %>%
  filter(lat >= 0 & lat <= 23.5)

temperate_therm <- cl_gt2 %>%
  filter(lat > 23.5 & lat <= 66.5)