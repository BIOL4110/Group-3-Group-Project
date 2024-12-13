# not used for analysis
# saving code that was helpful for us / steps we took but did not use in the final analysis
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

# tax harmonization ----
library(taxadb) # Creates a local database of many commonly used taxonomic authorities and provides functions that can quickly query this data
library(dplyr)
library(tidyverse)
library(parallel) # Support for parallel computation, including by forking and random-number generation.
library(furrr) # combine purrr's family of mapping functions with future's parallel processing capabilities.
library(taxize) # allows users to search over many taxonomic data sources for species names.
library(stringr)

# setting functions, do not need to copy again
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

# BIOTIME - Ije 
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

# logged abundance code- eh not really working ---- 

# Step 1: Calculate Topt as the midpoint of Tmin and Tmax (or customize as needed)
df2 <- df1 %>%
  mutate(Topt = (temp_min + temp_max) / 2)

# Step 2: Log-transform abundance
df2 <- df2 %>%
  mutate(log_abundance = log(abundance + 1))  # Avoid log(0)

# Step 3: Fit a TPC curve (e.g., Gaussian or quadratic fit)
tpc_fits <- df2 %>%
  group_by(region, genus_species) %>%
  filter(n_distinct(mean_sst) > 2) %>%  # Ensure at least 3 unique points for a quadratic fit
  summarise(fit = list(lm(log_abundance ~ poly(mean_sst, 2))), .groups = "drop")

# Step 2: Extract the fitted TPC curves
tpc_predictions <- tpc_fits %>%
  mutate(predictions = map(fit, ~ {
    data.frame(mean_sst = seq(min(df2$mean_sst, na.rm = TRUE), 
                              max(df2$mean_sst, na.rm = TRUE), length.out = 100),
               fitted_values = predict(.x, 
                                       newdata = data.frame(mean_sst = seq(min(df2$mean_sst, na.rm = TRUE), 
                                                                           max(df2$mean_sst, na.rm = TRUE), length.out = 100))))
  })) %>%
  unnest(predictions)

df2_topt <- df2 %>%
  group_by(region, genus_species) %>%
  summarise(
    t_opt = mean(mean_sst[log_abundance == max(log_abundance, na.rm = TRUE)]),
    .groups = "drop"
  )

# Step 2: Merge Topt with the original dataframe
df2 <- df2 %>%
  left_join(df2_topt, by = c("region", "genus_species"))

# Step 3: Create the plot
df2 %>%
  ggplot(aes(x = mean_sst, y = log_abundance, color = genus_species)) +
  geom_point(alpha = 0.6, size = 3) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linewidth = 1) +  # Quadratic fit
  geom_vline(aes(xintercept = t_opt), linetype = "dashed", color = "orange", linewidth = 1) +  # Topt line
  facet_wrap(~region, scales = "free", ncol = 1) +  # Facet by region and species
  labs(x = "Temperature (°C)", 
       y = "Log Abundance", 
       title = "Abundance vs. Temperature with Topt") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(strip.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none")




# HYPOTHESIS ONE - done not using ----
## total abundance change over time by region - done not using 
{
  df1 %>%
    filter(total_abundance <= 5000) %>% # Filter outliers - heighest is 40 000 but are major outliers
    ggplot(aes(x = factor(year), y = total_abundance, color = region, group = region)) + 
    geom_point(size = 2.5, alpha = 0.7) +
    geom_smooth(method = "loess") +
    facet_wrap(~region, scales = "free") + 
    scale_color_brewer(palette = "Set2") +
    labs(x = "Year", 
         y = "Total Abundance",
         color = "Region") +
    theme_minimal(base_size = 14)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 30), 
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 30),
          legend.position = "none") #+ ggsave("figures/TotalAbundance.png", width = 25, height = 15, dpi = 300)
}
#Compare beta diversity to thermal tolerance of different species , veirfy if works ------
# Assuming 'beta_diversity' has species and community data, 
beta_diversity2 <- df1 %>%
  group_by(region, year, genus_species) %>%
  summarize(bc_dist_mean = mean(as.vector(vegdist(as.matrix(total_abundance), method = "bray"))),
            .groups = "drop") %>%
  drop_na(bc_dist_mean)

# and 'thermal_tolerance' contains 'species' and 'CTmax' columns
thermal_tolerance <- df1 %>%
  select(c(7,10:11))

# Merge the datasets by species - redo with species richness
merged_data <- left_join(beta_diversity2, df1shannon, thermal_tolerance, by = c("genus_species")) %>% drop_na()

plot_data <- merged_data %>%
  filter(year >= 1960 & year <= 2010) %>%  # Optional: filter for relevant years
  group_by(region, genus_species, year) %>% 
  summarise(
    temp_min = mean(temp_min, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE),
    mean_sst = mean(mean_sst, na.rm = TRUE),
    bc_dist_mean = mean(bc_dist_mean, na.rm = TRUE)) %>% 
  ungroup()

## remove data if 
ggplot(plot_data, aes(x = genus_species)) +
  geom_segment(aes(xend = genus_species, y = temp_min, yend = temp_max),
               color = "skyblue", linewidth = 1.5) +
  geom_point(aes(y = mean_sst), color = "orange", size = 3) +
  #geom_point(aes(y = species_richness), color = "red", size = 2, alpha = 0.7) +
  facet_wrap(~region, scales = "free_x") +  # Facet by region
  labs(x = "Species",
       y = "Temperature (°C) / Bray-Curtis Distance",
       title = "Thermal Breadth, Mean SST, and Bray-Curtis Distance by Species",
       caption = "Blue bars: Thermal breadth (Temp Min to Max)\nOrange points: Mean SST\nRed points: Bray-Curtis Distance") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12))

{
  # 3. Statistical analysis
  # Compute correlation between beta diversity and thermal tolerance
  cor_test <- cor.test(merged_data$bc_dist_mean, merged_data$temp_max, method = "pearson")
  
  # Output the correlation result
  cat("Correlation between Beta Diversity and Thermal Tolerance:", cor_test$estimate, "\n")
  cat("p-value:", cor_test$p.value, "\n")
  
  # Optional: Regression analysis
  lm_model <- lm(bc_dist_mean ~ temp_max, data = merged_data)
  summary(lm_model)  # Get the summary of the regression model
  }
