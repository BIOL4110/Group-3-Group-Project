##06 - data-analysis of Globtherm data - Celeste and Mya

## Load packages

library(taxadb) # Creates a local database of many commonly used taxonomic authorities and provides functions that can quickly query this data
library(dplyr)
library(tidyverse)
library(parallel) # Support for parallel computation, including by forking and random-number generation.
library(furrr) # combine purrr's family of mapping functions with future's parallel processing capabilities.
library(taxize) # allows users to search over many taxonomic data sources for species names.
library(stringr)

library(janitor)
library(readr)
library(stringr)
library(traitdata)
library(ggplot2)

#Load databases
globtherm_analysis <- read_csv("data-processed/harmonized_globtherm2.csv")
colnames(globtherm_analysis)
head(globtherm_analysis)

globtherm_analysis %>% select(class) %>% print(n=50)
unique_class <- globtherm_analysis %>% distinct(class)
print(unique_class)

unique_order <- globtherm_analysis %>% distinct(order)
print(unique_order) %>% print(n=45)

filtered_globtherm <- globtherm_analysis %>% filter(class == "Actinopterygii")
print(filtered_globtherm) %>% print(n=39) %>% View

globtherm_fish <- filtered_globtherm
View(globtherm_fish)


# Filter out data to show solely the tropical species present
print(globtherm_fish)
globtherm_tropicalfish <- globtherm_fish %>% filter(region == "Tropical")
View(globtherm_tropicalfish)

# Filter out data to show solely the temperate species present
globtherm_temperatefish <- globtherm_fish %>% filter(region == "Temperate")
View(globtherm_temperatefish)

### Write csv for new globtherm df with only class: Actinopterygii
write_csv(globtherm_fish, "data-processed/globtherm_fish.csv")

## Plot the data!!!

globtherm_fish %>% ggplot(aes(tmax, th_genus_species, color = region)) + geom_jitter(aes()) +
  scale_color_manual(values = c("Temperate" = "blue", "Tropical" = "green"))

#Plotting tmax and tmin for tropical fish
# Load ggplot2
library(ggplot2)
library(dplyr)

###TROPICAL DATA
# Prepare data in long format for ggplot
globtherm_tropgraphed<- globtherm_tropicalfish %>%
  select(th_genus_species, tmax, tmin) %>%
  pivot_longer(cols = c(tmax, tmin), names_to = "Thermal_Tolerance", values_to = "Temperature")
View(globtherm_tropgraphed)

#Filter out NA
globtherm_tropgraphed <- globtherm_tropicalfish %>%
  select(th_genus_species, tmax, tmin) %>%
  filter(!is.na(tmax) & !is.na(tmin)) %>%
  pivot_longer(cols = c(tmax, tmin), names_to = "Thermal_Tolerance", values_to = "Temperature")

# Create the plot
ggplot(globtherm_tropgraphed, aes(x = th_genus_species, y = Temperature, color = Thermal_Tolerance)) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(th_genus_species, Thermal_Tolerance)), linetype = "dotted") +
  labs(title = "Thermal Tolerance of Selected Tropical Fish Species",
       x = "Tropical Fish Species",
       y = "Temperature (°C)",
       color = "Thermal Tolerance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Box plot
ggplot(globtherm_tropgraphed, aes(x = th_genus_species, y = Temperature, fill = Thermal_Tolerance)) +
  geom_boxplot() +
  labs(title = "Thermal Tolerance for Tropical Fish Species",
       x = "Species",
       y = "Temperature (°C)",
       fill = "Thermal Tolerance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###TEMPERATE DATA
# Prepare data in long format for ggplot
globtherm_tempgraphed<- globtherm_temperatefish %>%
  select(th_genus_species, tmax, tmin) %>%
  pivot_longer(cols = c(tmax, tmin), names_to = "Thermal_Tolerance", values_to = "Temperature")
View(globtherm_tempgraphed)

#Filter out NA
globtherm_tempgraphed <- globtherm_temperatefish %>%
  select(th_genus_species, tmax, tmin, region) %>%
  filter(!is.na(tmax) & !is.na(tmin)) %>%
  pivot_longer(cols = c(tmax, tmin), names_to = "Thermal_Tolerance", values_to = "Temperature")
View(globtherm_tempgraphed)

# Create the plot
ggplot(globtherm_tempgraphed, aes(x = th_genus_species, y = Temperature, color = Thermal_Tolerance)) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(th_genus_species, Thermal_Tolerance)), linetype = "dotted") +
  labs(title = "Thermal Tolerance of Selected Temperate Fish Species",
       x = "Temperate Fish Species",
       y = "Temperature (°C)",
       color = "Thermal Tolerance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Box plot
ggplot(globtherm_tempgraphed, aes(x = th_genus_species, y = Temperature, fill = Thermal_Tolerance)) +
  geom_boxplot() +
  labs(title = "Thermal Tolerance for Temperate Fish Species",
       x = "Species",
       y = "Temperature (°C)",
       fill = "Thermal Tolerance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Graph both Tropical and Temperate fish species 

#Filter out NA
globtherm_fishclean <- globtherm_fish %>%
  select(th_genus_species, tmax, tmin, region) %>%
  filter(!is.na(tmax) & !is.na(tmin)) %>%
  pivot_longer(cols = c(tmax, tmin), names_to = "Thermal_Tolerance", values_to = "Temperature")
View(globtherm_fishclean)

# Create the plot
ggplot(globtherm_fishclean, aes(x = th_genus_species, y = Temperature, color = Thermal_Tolerance)) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(th_genus_species, Thermal_Tolerance)), linetype = "dotted") +
  labs(title = "Thermal Tolerance of Selected Fish Species",
       x = "Temperate and Tropical Fish Species",
       y = "Temperature (°C)",
       color = "Thermal Tolerance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




