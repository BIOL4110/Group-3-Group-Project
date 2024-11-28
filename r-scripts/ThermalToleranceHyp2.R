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
View(h2_data)

tropical_fish <- h2_data %>% 
  filter(region == 'Tropical')
View(tropical_fish)

# Prepare data in long format for ggplot
fb_trop <- tropical_fish %>%
  select(abundance, year, fishbase_name, region, temp_min, temp_max, temp_max_filled, thermal_tolerance, mean_summer_sst_degC) %>%
  pivot_longer(cols = c(temp_max, temp_min), names_to = "Thermal Tolerance", values_to = "Temperature")
View(fb_trop)

#Create the plot
ggplot(fb_trop, aes(x = fishbase_name, y = Temperature, color = thermal_tolerance)) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(fishbase_name, thermal_tolerance)), linetype = "dotted") +
  labs(title = "Thermal Tolerance of Selected Tropical Fish Species",
       x = "Tropical Fish Species",
       y = "Temperature (°C)",
       color = "Thermal Tolerance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Temperate Data
temperate_fish <- h2_data %>% 
  filter(region == 'Temperate')
View(temperate_fish)

# Prepare data in long format for ggplot - Temperate
fb_temp <- temperate_fish %>%
  select(abundance, year, fishbase_name, region, temp_min, temp_max, temp_max_filled, thermal_tolerance, mean_summer_sst_degC) %>%
  pivot_longer(cols = c(temp_max, temp_min), names_to = "Thermal Tolerance", values_to = "Temperature")
View(fb_temp)

#Create the plot
ggplot(fb_temp, aes(x = fishbase_name, y = Temperature, color = thermal_tolerance)) +
  geom_point(size = 1) +
  geom_line(aes(group = interaction(fishbase_name, thermal_tolerance)), linetype = "dotted") +
  labs(title = "Thermal Tolerance of Selected Temperate Fish Species",
       x = "Temperate Fish Species",
       y = "Temperature (°C)",
       color = "Thermal Tolerance") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 90, hjust = 1))

#Plotting SST over time
ggplot(fb_trop, aes(x = year, y = mean_summer_sst_degC)) +
  geom_line(color = "blue", size = 1) +  # Original line plot
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +  # Linear trendline
  scale_x_continuous(breaks = seq(min(fb_trop$year), max(fb_trop$year), by = 1)) +
  labs(
    title = "Mean Sea Surface Temperature Over Time",
    x = "Year",
    y = "Mean SST (°C)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1))

# Plotting abundance over time
ggplot(fb_trop, aes(x = year, y = abundance, color = fishbase_name)) +
  geom_jitter(size = 1) +
  labs(
    title = "Species Abundance Over Time",
    x = "Year",
    y = "Abundance",
    color = "Fish Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


