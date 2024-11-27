library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
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

## Create new column the tolerance range of species 

h2_data <- btfbsst_analysisV2 %>% 
  mutate('thermal_tolerance' = temp_max - temp_min)


## Define a categorical variable classifying tolerance range 

h2_data <- h2_data %>%
  mutate(tolerance_category = case_when(
    thermal_tolerance <= 5 ~ "narrow",
    thermal_tolerance > 5 & thermal_tolerance <= 10 ~ "medium",  
    thermal_tolerance > 10  ~ "broad " ))

## Seperate tropical and temperate species to make plotting simpler

tropical_h2_data <- h2_data %>% 
  filter(region == "Tropical")

temperate_h2_data <- h2_data %>% 
  filter(region == "Temperate")

## Plot abundance over time for each region

#Tropical 

ggplot(tropical_h2_data, aes(x = year, y = abundance)) +
  geom_jitter(alpha = 0.6, size = 0.5) +  
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  scale_y_log10() +
  labs(title = "Abundance Over Time For Tropical Species", 
       x = "year", 
       y = "abundance") +
  facet_wrap(~tolerance_category) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Temperate 

ggplot(temperate_h2_data, aes(x = year, y = abundance)) +
  geom_jitter(alpha = 0.6, size = 0.5) +  
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  scale_y_log10() +
  labs(title = "Abundance Over Time For Tropical Species", 
       x = "year", 
       y = "abundance") +
  facet_wrap(~tolerance_category) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# visualizing distribution of thermal tolerances 








