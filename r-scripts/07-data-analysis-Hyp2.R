#Data Analysis for Hypothesis 2: Comparing Thermal Tolerance to Species Abundance

#Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

btfbsst_analysis <- read_csv("data-processed/btfbsst_without_NA.csv") 
str(btfbsst_analysis)

# Step 1: Check for NAs, create thermal tolerance range, seperate tropical and temperate data, create thermal tolerance plots

#Check for NA Values in the data
anyNA(btfbsst_analysis)
colSums(is.na(btfbsst_analysis))
btfbsst_analysis[rowSums(is.na(btfbsst_analysis)) > 0, ]
anyNA(btfbsst_analysis$temp_max)
anyNA(btfbsst_analysis$temp_min)

## Create new column thermal tolerance range of species 

 btfbsst_wthermtol<- btfbsst_analysis %>% 
  mutate('thermal_range' = temp_max - temp_min)

## Create two seperate datasets for tropical and temperate fish
 ###TROPICAL
 tropical_h2_data <- btfbsst_wthermtol %>% 
   filter(region == "Tropical")
 
 #Determine number of unique tropical species
 num_species <- tropical_h2_data %>%
   summarise(unique_species_count = n_distinct(fishbase_name))
 num_species
 
 ###TEMPERATE
  temperate_h2_data <- btfbsst_wthermtol %>% 
   filter(region == "Temperate") 
 
   #Determine number of unique temperate species
  num_species2 <- temperate_h2_data %>%
    summarise(unique_species_count = n_distinct(fishbase_name))
  num_species2
 
 # Create Thermal Tolerance Range Plots
 library(viridis)
 
 #Create the thermal tolerance range plot for tropical species
 fb_trop <- tropical_h2_data %>%
   mutate(fishbase_name = reorder(fishbase_name, thermal_range, FUN = max, order = TRUE))
 
 # Prepare data in long format for ggplot
 fishbase_tropgraphed <- fb_trop %>%
   select(fishbase_name, temp_max, temp_min) %>%
   pivot_longer(cols = c(temp_max, temp_min), names_to = "thermal_tolerance", values_to = "Temperature")
 
 # Plot the tropical thermal tolerance graph
 # Add a gradient scale for temperature
 ggplot(fishbase_tropgraphed, aes(x = fishbase_name)) + # Add points for temp_min and temp_max
   geom_point(aes(y = Temperature, color = Temperature), size = 3, alpha = 0.8) + # Add lines for thermal tolerance range
   geom_line(aes(y = Temperature, group = fishbase_name, color = Temperature), size = 1) + # Set a three-color gradient for the legend
   scale_color_gradientn(
     colors = c("yellow", "orange", "red"),
     name = "Temperature (°C)",
     guide = guide_colorbar(direction = "vertical", barwidth = 0.8, barheight = 10)) +
   labs(
     title = "Thermal Tolerance of Selected Tropical Fish Species",
     x = "Tropical Fish Species",
     y = "Temperature (°C)") +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
     text = element_text(size = 14),
     legend.position = "right")

 #Create the thermal tolerance range plot for temperate species
 fb_temp <- temperate_h2_data %>%
   mutate(fishbase_name = reorder(fishbase_name, thermal_range, FUN = max, order = TRUE))

 #Prepare data in long format for ggplot
 fishbase_tempgraphed <- fb_temp %>%
   select(fishbase_name, temp_max, temp_min) %>%
   pivot_longer(cols = c(temp_max, temp_min), names_to = "thermal_tolerance", values_to = "Temperature")
 
 ggplot(fishbase_tempgraphed, aes(x = fishbase_name)) +
   # Add points for temp_min and temp_max
   geom_point(aes(y = Temperature, color = Temperature), size = 3, alpha = 0.8) +
   # Add lines for thermal tolerance range
   geom_line(aes(y = Temperature, group = fishbase_name, color = Temperature), size = 1) +
   # Set color gradient for the legend
   scale_color_gradient(
     low = "lightblue",
     high = "darkblue",
     name = "Temperature (°C)",
     guide = guide_colorbar(direction = "vertical", barwidth = 0.8, barheight = 10)) +
   labs(
     title = "Thermal Tolerance of Selected Temperate Fish Species",
     x = "Temperate Fish Species",
     y = "Temperature (°C)") +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
     text = element_text(size = 14),
     legend.position = "right")
  
# Step 2: Data Analysis for Hypothesis 2
  library(broom)
 
  ##Tropical Data
  # A. Clean data - create a new column with lat and long combined and round to 2 decimals - Tropical data
 
  tropical_h2_analysis <- tropical_h2_data %>%
    mutate(
      latitude = round(latitude, 2),       # Round latitude to 2 decimals
      longitude = round(longitude, 2),     # Round longitude to 2 decimals
      location = paste(latitude, longitude, sep = ", "))
  
  # Plot abundance over time for each location, adding regression lines
  ## Plots year on x-axis, abundance on y-axis and species name is grouped by colour
  ggplot(tropical_h2_analysis, aes(x = year, y = abundance, color = fishbase_name)) +
    geom_point(alpha = 0.6) +  # Scatterplot of abundance over time
    geom_smooth(method = "lm", se = FALSE, aes(group = fishbase_name), size = 1) +  # Add regression lines
    labs(
      title = "Abundance Slopes Over Time for Each Location for Tropical Region",
      x = "Year",
      y = "Abundance",
      color = "Species") +
    facet_wrap(~ location, scales = "free_y") +  # Create a separate panel for each location
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  # Visualize abundance over time for selected locations
  ggplot(tropical_h2_analysis, aes(x = year, y = abundance, group = location, color = location)) +
    geom_line(alpha = 0.6) +
    facet_wrap(~ location, scales = "free") +
    labs(
      title = "Abundance Trends Over Time by Location for Tropical Region",
      x = "Year",
      y = "Abundance") +
    theme_minimal()
  
  # B. Calculate the slope of abundance over time for each location (lat, long coordinate)
  ## Filter out groups (species-location combo) that may have too few observations - include only groups with >5 years of observations at that location. 
  ### Extract slope from each model (coefficient for year)
  
  slopes <- tropical_h2_analysis %>%
    group_by(location, fishbase_name) %>% # Group by location and species
    filter(n_distinct(year) > 5) %>%      # Only include groups with >5 unique years
    filter(sum(!is.na(abundance)) > 5) %>% # Ensure enough non-NA abundance values
    summarise(
      slope = coef(lm(abundance ~ year, data = pick(everything())))[2], # Extract slope
      thermal_range = first(thermal_range), # Include thermal range
      temp_max = first(temp_max),          # Include temp_max (CTmax)
      .groups = "drop")                  # Ungroup after summarising
  
  # Determine number of unique species we will be analyzing (just an FYI) - Tropical has 10 species total
  num_species2 <- slopes %>%
    summarise(unique_species_count = n_distinct(fishbase_name))
  num_species2
  
  # Merge slopes back into dataset - merge calculated slopes back into the original dataset to plot abundance trends over time
  
  abundance_with_slopes <- tropical_h2_analysis %>%
    left_join(slopes, by = c("location", "fishbase_name")) # Merge slopes
  
  # Plot Abundance Trends - abundance trends for each location with slopes as the color of the lines
  # Faceted plot for abundance trends by location
  ggplot(abundance_with_slopes, aes(x = year, y = abundance, group = fishbase_name, color = slope)) +
    geom_line(alpha = 0.7, size = 0.8) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                          name = "Abundance Slope") +
    labs(
      title = "Abundance Trends Over Time by Location for Tropical Region",
      x = "Year",
      y = "Abundance",
      color = "Slope") +
    facet_wrap(~ location, scales = "free") +  # Separate plots for each location
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom") # Will need to filter out groups with only one observation before plotting
  
  #Edited Plot of Abundance Trends
 ## Filer out groups with only 1 observation
  filtered_data <- abundance_with_slopes %>%
    group_by(location, fishbase_name) %>%
    filter(n_distinct(year) > 1) %>%
    ungroup()
  
  # Plot Abundance Trends - abundance at each location with slopes visualized as the colour of the lines
  ggplot(filtered_data, aes(x = year, y = abundance, group = interaction(location, fishbase_name), color = slope)) +
    geom_line(alpha = 0.7, size = 0.8) +
    scale_color_gradientn(colors = c("blue", "white", "red"), name = "Slope") +
    labs(
      title = "Abundance Trends Over Time by Location for Tropical Region",
      x = "Year",
      y = "Abundance",
      color = "Slope") +
    facet_wrap(~ location, scales = "free") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  # C. Combine predictor variables - Thermal Tolerance Range (temp_max-temp_min) and CTmax (temp_max)
  model_tropical <- slopes %>%
    left_join(select(slopes, location, fishbase_name, thermal_range, temp_max), 
              by = c("location", "fishbase_name")) 
  model_tropical
  
  model1 <- lm(slope ~ thermal_range.x + temp_max.x, data = model_tropical)
  summary(model1)
  
  # Visualize the slope relationships
  
  # Scatterplot of slope vs thermal range
  ggplot(slopes, aes(x = thermal_range, y = slope)) +
    geom_point(size = 2, color = "blue", alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = "Relationship Between Slope of Abundance and Thermal Tolerance Range",
      x = "Thermal Tolerance Range",
      y = "Slope of Abundance") +
    theme_minimal()
  
  
  # Scatterplot of slope vs CTmax
  ggplot(slopes, aes(x = temp_max, y = slope)) +
    geom_point(size = 2, color = "blue", alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = "Relationship Between Slope of Abundance and CTmax",
      x = "CTmax (°C)",
      y = "Slope of Abundance") +
    theme_minimal()
  
  #######################################
  
  # Step 2: Data Analysis for Hypothesis 2
  
  ##Temperate  Data
  # A. Clean data - create a new column with lat and long combined and round to 2 decimals - Temperate data
  
 temperate_h2_analysis <- temperate_h2_data %>%
    mutate(
      latitude = round(latitude, 2),       # Round latitude to 2 decimals
      longitude = round(longitude, 2),     # Round longitude to 2 decimals
      location = paste(latitude, longitude, sep = ", "))
  
  # Plot abundance over time for each location, adding regression lines
  ## Plots year on x-axis, abundance on y-axis and species name is grouped by colour
  ggplot(temperate_h2_analysis, aes(x = year, y = abundance, color = fishbase_name)) +
    geom_point(alpha = 0.6) +  # Scatterplot of abundance over time
    geom_smooth(method = "lm", se = FALSE, aes(group = fishbase_name), size = 1) +  # Add regression lines
    labs(
      title = "Abundance Slopes Over Time for Each Location for Temperate Region",
      x = "Year",
      y = "Abundance",
      color = "Species") +
    facet_wrap(~ location, scales = "free_y") +  # Create a separate panel for each location
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  # Visualize abundance over time for selected locations
  ggplot(temperate_h2_analysis, aes(x = year, y = abundance, group = location, color = location)) +
    geom_line(alpha = 0.6) +
    facet_wrap(~ location, scales = "free") +
    labs(
      title = "Abundance Trends Over Time by Location for Temperate Region",
      x = "Year",
      y = "Abundance") +
    theme_minimal()
  
  # B. Calculate the slope of abundance over time for each location (lat, long coordinate)
  ## Filter out groups (species-location combo) that may have too few observations - include only groups with >5 years of observations at that location. 
  ### Extract slope from each model (coefficient for year)
  
  slopes2 <- temperate_h2_analysis %>%
    group_by(location, fishbase_name) %>% # Group by location and species
    filter(n_distinct(year) > 5) %>%      # Only include groups with >5 unique years
    filter(sum(!is.na(abundance)) > 5) %>% # Ensure enough non-NA abundance values
    summarise(
      slope = coef(lm(abundance ~ year, data = pick(everything())))[2], # Extract slope
      thermal_range = first(thermal_range), # Include thermal range
      temp_max = first(temp_max),          # Include temp_max (CTmax)
      .groups = "drop")                  # Ungroup after summarising
  
  # Determine number of unique species we will be analyzing (just an FYI) - Temperate has X species total
  num_species3 <- slopes2 %>%
    summarise(unique_species_count = n_distinct(fishbase_name))
  num_species3
  
  # Merge slopes back into dataset - merge calculated slopes back into the original dataset to plot abundance trends over time
  
  abundance_with_slopes2 <- temperate_h2_analysis %>%
    left_join(slopes, by = c("location", "fishbase_name")) # Merge slopes
  
  # Plot Abundance Trends - abundance trends for each location with slopes as the color of the lines
  # Faceted plot for abundance trends by location
  ggplot(abundance_with_slopes2, aes(x = year, y = abundance, group = fishbase_name, color = slope)) +
    geom_line(alpha = 0.7, size = 0.8) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                          name = "Abundance Slope") +
    labs(
      title = "Abundance Trends Over Time by Location for Temperate Regions",
      x = "Year",
      y = "Abundance",
      color = "Slope") +
    facet_wrap(~ location, scales = "free") +  # Separate plots for each location
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom") # Will need to filter out groups with only one observation before plotting
  
  #Edited Plot of Abundance Trends
  ## Filer out groups with only 1 observation
  filtered_data2 <- abundance_with_slopes2 %>%
    group_by(location, fishbase_name) %>%
    filter(n_distinct(year) > 1) %>%
    ungroup()
  
  # Plot Abundance Trends - abundance at each location with slopes visualized as the colour of the lines
  ggplot(filtered_data2, aes(x = year, y = abundance, group = interaction(location, fishbase_name), color = slope)) +
    geom_line(alpha = 0.7, size = 0.8) +
    scale_color_gradientn(colors = c("blue", "white", "red"), name = "Slope") +
    labs(
      title = "Abundance Trends Over Time by Location for Temperate Regions",
      x = "Year",
      y = "Abundance",
      color = "Slope") +
    facet_wrap(~ location, scales = "free") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  # C. Combine predictor variables - Thermal Tolerance Range (temp_max-temp_min) and CTmax (temp_max)
  model_temperate <- slopes2 %>%
    left_join(select(slopes2, location, fishbase_name, thermal_range, temp_max), 
              by = c("location", "fishbase_name")) 
  model_temperate
  
  model2 <- lm(slope ~ thermal_range.x + temp_max.x, data = model_temperate)
  summary(model2)
  
  # Visualize the slope relationships
  
  # Scatterplot of slope vs thermal range
  ggplot(slopes2, aes(x = thermal_range, y = slope)) +
    geom_point(size = 2, color = "blue", alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = "Relationship Between Slope of Abundance and Thermal Tolerance Range",
      x = "Thermal Tolerance Range",
      y = "Slope of Abundance") +
    theme_minimal()
  
  
  # Scatterplot of slope vs CTmax
  ggplot(slopes2, aes(x = temp_max, y = slope)) +
    geom_point(size = 2, color = "blue", alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = "Relationship Between Slope of Abundance and CTmax",
      x = "CTmax (°C)",
      y = "Slope of Abundance") +
    theme_minimal()
  
  #NOTE: Same results when doing this with the do() function
  #Using the do() function to analyze data

  # Step 2: Data Analysis for Hypothesis 2
  library(broom)
  
  ##Tropical Data
  # A. Clean data - create a new column with lat and long combined and round to 2 decimals - Tropical data
  
  tropical_h2_analysis <- tropical_h2_data %>%
    mutate(
      latitude = round(latitude, 2),       # Round latitude to 2 decimals
      longitude = round(longitude, 2),     # Round longitude to 2 decimals
      location = paste(latitude, longitude, sep = ", "))
  
  #Fit a linear model for each location-species group
  abundance_models <- tropical_h2_analysis %>%
    group_by(location, fishbase_name) %>%
    filter(n_distinct(year) > 5, sum(!is.na(abundance)) > 5) %>%  # Ensure enough data
    do(model = lm(abundance ~ year, data = .))  # Apply the model
  
  # Extract slopes from the models
  slopes <- abundance_models %>%
    mutate(slope = coef(model)[["year"]]) %>%  # Extract slope (coefficient for year)
    select(location, fishbase_name, slope)     # Keep relevant columns
  
  # Visualize slopes by location
  ggplot(slopes, aes(x = location, y = slope, color = slope)) +
    geom_point(size = 3) +
    scale_color_gradientn(colors = c("blue", "white", "red"), name = "Slope") +
    labs(
      title = "Abundance Slopes by Location for Tropical Region",
      x = "Location",
      y = "Slope of Abundance"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  # B. Calculate the slope of abundance over time for each location (lat, long coordinate)
  ## Filter out groups (species-location combo) that may have too few observations - include only groups with >5 years of observations at that location. 
  ### Extract slope from each model (coefficient for year)
  
  slopes <- tropical_h2_analysis %>%
    group_by(location, fishbase_name) %>% # Group by location and species
    filter(n_distinct(year) > 5) %>%      # Only include groups with >5 unique years
    filter(sum(!is.na(abundance)) > 5) %>% # Ensure enough non-NA abundance values
    summarise(
      slope = coef(lm(abundance ~ year, data = pick(everything())))[2], # Extract slope
      thermal_range = first(thermal_range), # Include thermal range
      temp_max = first(temp_max),          # Include temp_max (CTmax)
      .groups = "drop")                  # Ungroup after summarising
  
  # C. Combine predictor variables - Thermal Tolerance Range (temp_max-temp_min) and CTmax (temp_max)
  model_tropical <- slopes %>%
    left_join(select(slopes, location, fishbase_name, thermal_range, temp_max), 
              by = c("location", "fishbase_name")) 
  model_tropical
  
  model1 <- lm(slope ~ thermal_range.x + temp_max.x, data = model_tropical)
  summary(model1)

  
  
  summary(model1)