library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)


btfbsst_analysis <- read_csv("data-processed/btfbsst_without_NA.csv")

# HYPOTHESIS TWO  - compare thermal tolerance to species abundance
# phylogentic computation to fill in missing ctmax data based on phylogeny of the species
## use ctmax for same genus or family - match at genus level
#scatter plot works better for speciees richness
{
  # Compute genus-level averages for CTmax
  genus_ctmax <- btfbsst_analysis %>%
    group_by(genus) %>%
    summarise(genus_avg_ctmax = mean(temp_max, na.rm = TRUE))
  # Merge genus-level CTmax back into the main dataset
  btfbsst_analysisV2 <- btfbsst_analysis %>%
    left_join(genus_ctmax, by = "genus")
  btfbsst_analysisV2 <- btfbsst_analysis %>%
    mutate(temp_max_filled = ifelse(is.na(temp_max), genus_avg_ctmax, temp_max))
  # Scatterplot: Abundance vs. CTmax
  ggplot(btfbsst_analysisV2, aes(x = temp_max_filled, y = abundance)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    facet_wrap(~region) +
    theme_minimal() +
    labs(
      x = "Thermal Tolerance (CTmax)",
      y = "Species Abundance",
      title = "Relationship between CTmax and Species Abundance")
  # Linear regression model
  model <- lm(abundance ~ temp_max_filled, data = btfbsst_analysisV2)
  summary(model) }

## Create new column thermal tolerance range of species 

h2_data <- btfbsst_analysisV2 %>% 
  mutate('tolerance_range' = temp_max - temp_min)

# Aggregate the abundance data by species and year, calculating the mean abundance for each species-year combination
h2_data_aggregated_1 <- h2_data %>%
  group_by(fishbase_name, year, region, tolerance_range) %>%
  summarise(sum_abundance = sum(abundance, na.rm = TRUE)) %>%
  ungroup()


# Identify abundance at first and last years

h2_data_aggregated <- h2_data_aggregated_1 %>%
  group_by(fishbase_name, region, tolerance_range) %>%
  arrange(year) %>%  
  summarise(
    first_year_abundance = first(sum_abundance),  
    last_year_abundance = last(sum_abundance),    
    first_year = first(year),                 
    last_year = last(year)) %>% 
  mutate(abundance_change = (last_year_abundance - first_year_abundance) / first_year_abundance * 100)


## Make two seperate datasets for tropical and temperate fish to make graphing simpler

tropical_h2_data <- h2_data_aggregated %>% 
  filter(region == "Tropical")

temperate_h2_data <- h2_data_aggregated %>% 
  filter(region == "Temperate")

## Plot

##Tropical
ggplot(tropical_h2_data, aes(x = fishbase_name, y = abundance_change, fill = abundance_change)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Change in Abundance from First to Last Year",
    x = "Species",
    y = "Abundance Change (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##Temperate

ggplot(temperate_h2_data, aes(x = fishbase_name, y = abundance_change, fill = abundance_change)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Change in Abundance from First to Last Year",
    x = "Species",
    y = "Abundance Change (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## unsure why some abundance changes are absolutely massive... 
  



