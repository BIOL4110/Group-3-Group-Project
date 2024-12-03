# not used for analysis

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
  ggtitle("B)") + 
  theme_classic() +
  theme(
    plot.title = element_text(size = 16),  # Title size
    axis.title.x = element_text(size = 14), # X-axis label size
    axis.title.y = element_text(size = 14), # Y-axis label size
    axis.text.x = element_text(size = 12),  # X-axis text size
    axis.text.y = element_text(size = 12),  # Y-axis text size
  )

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
library(ggpmisc)

R_temp_fish_div %>%
  ggplot(aes(year, num_species_present)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkgreen") +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +  # Add slope and R^2 to the plot
  xlab("Year") +
  ylab("Number of species (R)") +
  ggtitle("A)") + 
  theme_classic()+
  theme(
    plot.title = element_text(size = 16),  # Title size
    axis.title.x = element_text(size = 14), # X-axis label size
    axis.title.y = element_text(size = 14), # Y-axis label size
    axis.text.x = element_text(size = 12),  # X-axis text size
    axis.text.y = element_text(size = 12),  # Y-axis text size
  )

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
  filter(lat >= 0 & lat <= 23.5) %>%
  mutate(region = "Tropical")

temperate_therm <- cl_gt2 %>%
  filter(lat > 23.5 & lat <= 66.5) %>%
  mutate(region = "Temperate")

# Combine the two data frames by region 
globTherm_processed <- bind_rows(tropical_therm, temperate_therm)

# csv of filtered globtherm data 
#write_csv(globTherm_processed, "data-processed/globTherm_processed.csv")


### GLOBTHERM-- Celeste and Mya ----
library(readr)
globTherm_processed <- read_csv("data-processed/globTherm_processed.csv")

#Creating new column joining species and genus columns
globTherm_processed2 <- globTherm_processed %>% 
  mutate(genus_species = if_else(is.na(species), genus, paste(genus, species, sep = "_"))) %>%
  select(-scientific_name_std)

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

##downloaded harmonized ds
harmonized_globtherm <- read_csv("data-processed/harmonized_globtherm.csv") 

# Merge dataset with species name key for the purpose of matching with other datasets by ID
mergedb <- merge_df_with_key(globtherm, harmonized_globtherm)

# returns TRUE,  means both columns contain the same values, regardless of order.
setequal(mergedb$genus_species, mergedb$sp_name_for_matching)

## merge phylum class order family genus

#kingdom: animalia only 
mergedb <- mergedb %>%
  filter(kingdom %in% c("Animalia", NA))

#coalasece line up with input name if it = NA or if db is NA 
mergedb2 <- mergedb %>%
  mutate(phylum.y = coalesce(phylum.y, phylum.x)) %>%
  mutate(order.y = coalesce(order.y, order.x)) %>%
  mutate(class.y = coalesce(class.y, class.x)) %>%
  mutate(family.y = coalesce(family.y, family.x)) %>%
  mutate(genus.y = coalesce(genus.y, genus.x)) %>%
  select(-c(phylum.x, order.x, class.x, family.x, genus.x, db, match_source, kingdom)) 

harmonized_globtherm_processed <- mergedb2 %>%
  rename_with(~ str_remove(., "\\.y$"), 9:13) %>%
  rename(input_name = genus_species,
         th_genus_species = sp_name_for_matching) %>%
  select(2:7,th_genus_species, sp_id, input_name, phylum, order, class, family, genus, species, everything())

harmonized_globtherm_processed %>%
  filter(phylum == "Chordata")

#write_csv(harmonized_globtherm_processed, "data-processed/harmonized_globtherm2.csv")


# beta diversity - stuart ----
bio_time <- read.csv("data-processed/BioTime_processed.csv")

# Filter data for the two latitude ranges
df_trop <- bio_time %>% 
  subset(!is.na(species)) %>%
  filter(latitude >= 0 & latitude <= 23.5) %>% 
  group_by(genus_species,year) %>% 
  summarise(total_abundance = sum(abundance, na.rm = FALSE)) %>% 
  select(genus_species,year)

df_temp <- bio_time %>%
  subset(!is.na(species)) %>% 
  filter(latitude > 23.5 & latitude <= 66.5 & year > 2000 & year < 2011) %>% 
  group_by(genus_species,year) %>% 
  summarise(total_abundance = sum(abundance, na.rm = FALSE)) %>% 
  select(genus_species,year)

# Calculate Jaccard similarity index for each year
jaccard_similarity <- function(year) {
  species_trop <- df_trop %>% filter(year == !!year) %>% pull(genus_species) %>% unique()
  species_temp <- df_temp %>% filter(year == !!year) %>% pull(genus_species) %>% unique()
  
  intersection <- length(intersect(species_trop, species_temp))
  union <- length(union(species_trop, species_temp))
  
  if (union == 0) return(NA) else return(intersection / union)
}

years <- intersect(df_trop$year, df_temp$year)

jaccard_indices <- map_dbl(years, jaccard_similarity)

jaccard_results <- data.frame(year = years, jaccard_index = jaccard_indices)
jaccard_results

#plot the results of jacaard
jaccard_results %>% 
  ggplot(aes(year,jaccard_index)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylab("Jaccard index") + 
  xlab("Year") +
  theme_classic()

jaccard_model <- lm(jaccard_index ~ year, data = jaccard_results)

jaccard_model$coefficients
summary(jaccard_model)$adj.r.squared
summary(jaccard_model)
