# Load data
biotime <- read.csv("data-processed/BioTime_processed.csv")

head(biotime)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the species of interest
species_of_interest <- c("Hippoglossoides_platessoides", "Gadus_morhua", "Melanogrammus_aeglefinus", 
                         "Merluccius_bilinearis", "Myoxocephalus_octodecemspinosus", "Amblyraja_radiata", 
                         "Urophycis_tenuis", "Limanda_ferruginea", "Glyptocephalus_cynoglossus", 
                         "Clupea_harengus")

# Filter the dataset to include only the specified species
biotime_filtered <- biotime %>%
  filter(genus_species %in% species_of_interest) %>%
  filter(year > 1985) %>%
  filter(year < 2009)

# Calculate the average latitude per year for each of these species
biotime_latitude_agg <- biotime_filtered %>%
  group_by(genus_species, year) %>%  # Group by species and year
  summarise(avg_latitude = mean(latitude, na.rm = TRUE), .groups = 'drop')  # Calculate average latitude

# Create the plot
ggplot(biotime_latitude_agg, aes(x = year, y = avg_latitude, color = genus_species, group = genus_species)) +
  geom_line(size = 1) +  # Line plot for each species with size 1
  geom_point(size = 2) +  # Points for each year for each species
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", size = 0.9) +  # Trendlines as dotted lines
  labs(title = "Average Latitude of Selected Species Over Time",
       x = "Year",
       y = "Average Latitude",
       color = "Species") +  # Legend title
  theme_minimal() +  # Minimal theme
  theme(legend.position = "bottom")  # Position the legend at the bottom

# Define the species of interest
species_of_interest <- c("Hippoglossoides_platessoides", "Gadus_morhua", "Melanogrammus_aeglefinus", 
                         "Merluccius_bilinearis", "Myoxocephalus_octodecemspinosus", "Amblyraja_radiata", 
                         "Urophycis_tenuis", "Limanda_ferruginea", "Glyptocephalus_cynoglossus", 
                         "Clupea_harengus")

# Filter the dataset to include only the specified species
biotime_filtered <- biotime %>%
  filter(genus_species %in% species_of_interest) %>%
  filter(year > 1985) %>%
  filter(year < 2009)

# Calculate the average latitude per year across all selected species
average_latitude_per_year <- biotime_filtered %>%
  group_by(year) %>%
  summarise(avg_latitude_all_species = mean(latitude, na.rm = TRUE), .groups = 'drop')

# Calculate the average latitude per year for each individual species
biotime_latitude_agg <- biotime_filtered %>%
  group_by(genus_species, year) %>%
  summarise(avg_latitude_species = mean(latitude, na.rm = TRUE), .groups = 'drop')

# Merge the datasets to calculate the deviation for each species from the average
biotime_latitude_agg <- biotime_latitude_agg %>%
  left_join(average_latitude_per_year, by = "year") %>%
  mutate(deviation = avg_latitude_species - avg_latitude_all_species)  # Calculate deviation

# Plot the data
ggplot() +
  # Plot each species' deviation as a thinner line
  geom_line(data = biotime_latitude_agg, 
            aes(x = year, y = deviation, color = genus_species, group = genus_species), 
            size = 0.7, linetype = "solid") +  # Thinner dashed lines for deviations
  
  # Add labels and themes
  labs(title = "Deviation in Latitude of Selected Species from Average Over Time",
       x = "Year",
       y = "Deviation from Average Latitude",
       color = "Species") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend at the bottom

# Calculate standard deviation per year across all selected species
std_dev_per_year <- biotime_filtered %>%
  group_by(year) %>%
  summarise(std_dev_all_species = sd(latitude, na.rm = TRUE), .groups = 'drop')

# Calculate standard deviation per year for each individual species
std_dev_per_species <- biotime_filtered %>%
  group_by(genus_species, year) %>%
  summarise(std_dev_species = sd(latitude, na.rm = TRUE), .groups = 'drop')

# Merge the datasets to calculate the deviation in standard deviations
std_dev_agg <- std_dev_per_species %>%
  left_join(std_dev_per_year, by = "year") %>%
  mutate(std_dev_deviation = std_dev_species - std_dev_all_species)  # Deviation in standard deviations

# Plot the deviation in standard deviations
ggplot(std_dev_agg, aes(x = year, y = std_dev_deviation, color = genus_species, group = genus_species)) +
  geom_line(size = 0.7, linetype = "solid") +  # Thinner solid lines for deviations
  labs(title = "Deviation in Standard Deviations of Latitude Over Time",
       x = "Year",
       y = "Deviation from Overall Standard Deviation",
       color = "Species") +
  theme_minimal() +
  theme(legend.position = "bottom")