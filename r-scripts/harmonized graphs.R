# Load data
harmonized_btgt <- read.csv("data-processed/full_harmonized_btgt.csv")

head(harmonized_btgt)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Filter the dataset for class "Actinopterygii" before performing the aggregation
harmonized_btgt_agg <- harmonized_btgt %>%
  filter(class == "Actinopterygii") %>%  # Filter for only Actinopterygii
  group_by(genus, species, year) %>%  # Group by genus, species, and year
  summarise(total_abundance = sum(abundance), .groups = 'drop') %>%
  group_by(species) %>%
  mutate(abundance_percentage = (total_abundance / max(total_abundance)) * 100) %>%
  ungroup() %>%
  mutate(genus_species = paste(genus, species))  # Combine genus and species names

# Create scatter plot with lines connecting points and species names
ggplot(harmonized_btgt_agg, aes(x = year, y = abundance_percentage, color = genus_species, group = genus_species)) +
  geom_point(alpha = 0.7) +  # Scatter points with slight transparency
  geom_line(alpha = 0.7) +   # Lines connecting points for each species
  labs(title = "Relative Total Abundance of Each Species per Year (Percentage) - Actinopterygii",
       x = "Year",
       y = "Abundance (% of Maximum Total)",
       color = "Genus and Species") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = rainbow(length(unique(harmonized_btgt_agg$genus_species))))  # Custom colors for each species

# Filter for species "Clupea harengus" and calculate average latitude per year
clupea_latitude_agg <- harmonized_btgt %>%
  filter(species == "harengus") %>%  # Filter for Clupea harengus
  group_by(year) %>%  # Group by year
  summarise(avg_latitude = mean(latitude, na.rm = TRUE), .groups = 'drop')  # Calculate average latitude

# Create plot of average latitude over time for Clupea harengus
ggplot(clupea_latitude_agg, aes(x = year, y = avg_latitude)) +
  geom_line(color = "blue", size = 1) +  # Line plot with blue color
  geom_point(color = "red", size = 2) +  # Add points for each year
  labs(title = "Average Latitude of Clupea harengus Over Time",
       x = "Year",
       y = "Average Latitude") +
  theme_minimal()

# Filter for species in the class "Actinopterygii" and calculate average latitude per year
actinopterygii_latitude_agg <- harmonized_btgt %>%
  filter(class == "Actinopterygii") %>%  # Filter for only Actinopterygii class
  group_by(species, year) %>%  # Group by species and year
  summarise(avg_latitude = mean(latitude, na.rm = TRUE), .groups = 'drop')  # Calculate average latitude

# Create plot of average latitude over time for each species in Actinopterygii class
ggplot(actinopterygii_latitude_agg, aes(x = year, y = avg_latitude, color = species, group = species)) +
  geom_line(size = 1) +  # Line plot for each species with size 1
  geom_point(size = 2) +  # Points for each year for each species
  labs(title = "Average Latitude of Species in Actinopterygii Over Time",
       x = "Year",
       y = "Average Latitude",
       color = "Species") +  # Legend title
  theme_minimal() +  # Minimal theme
  theme(legend.position = "bottom")  # Position the legend at the bottom