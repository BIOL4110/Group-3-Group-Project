#Data Analysis for Hypothesis 2: Comparing Thermal Tolerance and Tmax to Species Abundance

#Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

btfbsst_analysis <- read_csv("data-processed/btfbsst_without_NA.csv") 
str(btfbsst_analysis)

# Step 1: Check for NAs, create thermal tolerance range, seperate tropical and temperate data, create thermal tolerance range plots

#Check for NA Values in the data
anyNA(btfbsst_analysis)
colSums(is.na(btfbsst_analysis))
btfbsst_analysis[rowSums(is.na(btfbsst_analysis)) > 0, ]
anyNA(btfbsst_analysis$temp_max)
anyNA(btfbsst_analysis$temp_min)

## Create new column thermal tolerance range of species 

 btfbsst_wthermtol<- btfbsst_analysis %>% 
  mutate('thermal_range' = temp_max - temp_min) 
 
 # Calculate the correlation between thermal_range and temp_max
 correlation <- cor(slopes$thermal_range, slopes$temp_max, use = "complete.obs")
 print(paste("Correlation between thermal_range and temp_max:", correlation))
 #High correlation: 0.949

## Create two seperate datasets for tropical and temperate fish
 ###TROPICAL
 tropical_h2_data <- btfbsst_wthermtol %>% 
   filter(region == "Tropical")
 
 # Calculate average thermal tolerance range for tropical region
 avg_trop_tolerance <- tropical_h2_data %>%
   summarise(avg_tolerance_range = mean(thermal_range, na.rm = TRUE))
 
 print(avg_trop_tolerance)
 
 #Determine number of unique tropical species
 num_species <- tropical_h2_data %>%
   summarise(unique_species_count = n_distinct(fishbase_name))
 num_species
 
 ###TEMPERATE
  temperate_h2_data <- btfbsst_wthermtol %>% 
   filter(region == "Temperate") 
  
  # Calculate average thermal tolerance range for tropical region
  avg_temp_tolerance <- temperate_h2_data %>%
    summarise(avg_tolerance_range = mean(thermal_range, na.rm = TRUE))
  
  print(avg_temp_tolerance)
 
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
 
 #Order fishbase_name alphabetically
 fishbase_tropgraphed <- fishbase_tropgraphed %>%
   mutate(fishbase_name = factor(fishbase_name, levels = sort(unique(fishbase_name))))
 
 # Plot the tropical thermal tolerance graph
 # Add a gradient scale for temperature
 thermal_tol_trop <- ggplot(fishbase_tropgraphed, aes(x = fishbase_name)) + # Add points for temp_min and temp_max
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

 ggsave("thermal_tol_tropFINAL.png", thermal_tol_trop) 
 
 #Create the thermal tolerance range plot for temperate species
 fb_temp <- temperate_h2_data %>%
   mutate(fishbase_name = reorder(fishbase_name, thermal_range, FUN = max, order = TRUE))

 #Prepare data in long format for ggplot
 fishbase_tempgraphed <- fb_temp %>%
   select(fishbase_name, temp_max, temp_min) %>%
   pivot_longer(cols = c(temp_max, temp_min), names_to = "thermal_tolerance", values_to = "Temperature")

  #Order fishbase_name alphabetically
 fishbase_tempgraphed <- fishbase_tempgraphed %>%
   mutate(fishbase_name = factor(fishbase_name, levels = sort(unique(fishbase_name))))
 
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
 
 ggsave("thermal_tol_tempFINAL.png", thermal_tol_temp) 
  
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
      title = "Abundance Over Time for Each Location for Tropical Region",
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
      abundance =first(abundance),
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
    scale_color_gradientn(colors = c("blue", "yellow", "green"), name = "Slope") +
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
  
  # D. Fit the model 
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
  
  # Graph for Paper - Scale colour changed, dates fixed and filtered to include only models with trendlines/abundance slopes
  # Merge slopes into the dataset for plotting
  abundance_with_slopes <- tropical_h2_analysis %>%
    left_join(slopes, by = c("location", "fishbase_name")) 
  
  # Plot abundance data and slope trend lines
  ggplot(abundance_with_slopes, aes(x = year, y = abundance.x, color = fishbase_name)) + # Points for abundance data
    geom_point(alpha = 0.6) +  # Line showing the slope
    geom_smooth(method = "lm", se = FALSE, aes(group = interaction(location, fishbase_name)), size = 1, linetype = "solid") +# Customize labels and legend
    scale_color_viridis_d(name = "Species") +
    scale_x_continuous(breaks = seq(floor(min(abundance_with_slopes$year)), ceiling(max(abundance_with_slopes$year)), 2)) + # Set breaks for whole years
    labs(
      title = "Abundance Trends Over Time for Tropical Species",
      x = "Year",
      y = "Abundance",
      color = "Species") + # Facet by location
    facet_wrap(~ location, scales = "free_y") + # Theme adjustments
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  # Filter locations that have valid slopes (non-NA)
  filtered_data_with_slopes <- abundance_with_slopes %>%
    filter(!is.na(slope)) # Remove rows where slope is NA
  
  # Plot Abundance Trends - Only locations with valid slopes
  
  trop_abundance_plot <- ggplot(filtered_data_with_slopes, aes(x = year, y = abundance.x, color = fishbase_name)) +
    geom_point(alpha = 0.6) +  
    geom_smooth(method = "lm", se = FALSE, aes(group = interaction(location, fishbase_name)), size = 1) +
    scale_color_viridis_d(name = "Species") +
    scale_x_continuous(breaks = seq(floor(min(filtered_data_with_slopes$year)), ceiling(max(filtered_data_with_slopes$year)), 1)) + # Whole years only
    labs(
      title = "Tropical Species Abundance Trends Over Time for Locations with Valid Slopes",
      x = "Year",
      y = "Abundance",
      color = "Species") +
    facet_wrap(~ location, scales = "free_y") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  # Determine p-values
  
  # Step 1: Calculate slopes and p-values for each group
  slopes_with_pvalues <- tropical_h2_analysis %>%
    group_by(location, fishbase_name) %>%
    filter(n_distinct(year) > 5) %>%      # Include groups with >5 years of data
    summarise(
      slope = coef(lm(abundance ~ year))[2],  # Extract slope
      p.value = summary(lm(abundance ~ year))$coefficients[2, 4], # Extract p-value for slope
      .groups = "drop")
  
  # Step 2: Determine the number of significant slopes
  significant_slopes <- slopes_with_pvalues %>%
    summarise(
      total = n(),                           # Total number of slopes
      significant = sum(p.value < 0.05),     # Count slopes with p-value < 0.05
      proportion_significant = significant / total * 100) # Percentage of significant slopes
  
  # Print results
  print(significant_slopes)
  
 ggsave("plot.png", trop_abundance_plotFINAL) 
  
  
  #######################################
  
  # Step 2: Data Analysis for Hypothesis 2
  
  ##Temperate  Data
  # A. Clean data - create a new column with lat and long combined and round to 2 decimals - Temperate data
  
 temperate_h2_analysis <- temperate_h2_data %>%
    mutate(
      latitude = round(latitude, 2),       # Round latitude to 2 decimals
      longitude = round(longitude, 2),     # Round longitude to 2 decimals
      location = paste(latitude, longitude, sep = ", "))
  
  # B. Calculate the slope of abundance over time for each location (lat, long coordinate)
  ## Filter out groups (species-location combo) that may have too few observations - include only groups with >3 years of observations at that location. 
  ### Extract slope from each model (coefficient for year)
  
  slopes2 <- temperate_h2_analysis %>%
    group_by(location, fishbase_name) %>% # Group by location and species
    filter(n_distinct(year) > 3) %>%      # Only include groups with >3 unique years
    filter(sum(!is.na(abundance)) > 3) %>% # Ensure enough non-NA abundance values
    summarise(
      slope = coef(lm(abundance ~ year, data = pick(everything())))[2], # Extract slope
      thermal_range = first(thermal_range), # Include thermal range
      temp_max = first(temp_max),          # Include temp_max (CTmax)
      .groups = "drop")                  # Ungroup after summarising
  
  # Determine number of unique species we will be analyzing (just an FYI) - Temperate has 23 species total
  num_species3 <- slopes2 %>%
    summarise(unique_species_count = n_distinct(fishbase_name))
  num_species3
  
  # Merge slopes back into dataset - merge calculated slopes back into the original dataset to plot abundance trends over time
  
  abundance_with_slopes2 <- temperate_h2_analysis %>%
    left_join(slopes2, by = c("location", "fishbase_name")) # Merge slopes
  
  # Filter the data to include only rows with valid slopes
  filtered_data2 <- abundance_with_slopes2 %>%
    filter(!is.na(slope))  # Keep only rows where slope is available
  
  # Plot Abundance trends - no trendline
  ggplot(filtered_data2, aes(x = year, y = abundance, group = interaction(location, fishbase_name), color = slope)) +
    geom_point(alpha = 0.6, size = 2) +  # Add points for observed abundance
    geom_line(alpha = 0.7, size = 0.8) +  # Line plot for abundance trends
    scale_color_gradientn(colors = c("blue", "white", "red"), name = "Slope") +  # Color by slope (negative to positive)
    labs(
      title = "Abundance Trends Over Time by Location for Temperate Regions",
      x = "Year",
      y = "Abundance",
      color = "Slope") +
    facet_wrap(~ location, scales = "free") +  # Separate plots for each location
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  # Plot Abundance Trends (only valid slopes) - abundance at each location with slopes visualized as the colour of the lines
  temp_abundance_plot <- ggplot(filtered_data2, aes(x = year, y = abundance, group = interaction(location, fishbase_name), color = slope)) +
    geom_point(alpha = 0.6, size = 2) +  # Add points for observed abundance
    geom_smooth(method = "lm", se = FALSE, aes(group = interaction(location, fishbase_name)), 
                size = 1, linetype = "solid") +  # Add linear regression trendline for each location
    scale_color_gradientn(colors = c("blue", "white", "red"), name = "Slope") +  # Color by slope (negative to positive)
    scale_x_continuous(breaks = seq(floor(min(filtered_data_with_slopes$year)), ceiling(max(filtered_data_with_slopes$year)), 2)) + # Whole years only
    labs(
      title = "Abundance Trends Over Time by Location for Temperate Regions",
      x = "Year",
      y = "Abundance",
      color = "Slope") +
    facet_wrap(~ location, scales = "free") +  # Separate plots for each location
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
  ggsave("plot2.png", temp_abundance_plot) 
  
  # Determine p-values
  
  # Calculate slopes and p-values for each group, excluding groups with perfect fits
  slopes_with_pvalues_temperate <- temperate_h2_analysis %>%
    group_by(location, fishbase_name) %>%  # Group by location and species
    filter(n_distinct(year) > 3) %>%      # Only include groups with >3 unique years
    do({lm_model <- lm(abundance ~ year, data = .)# Fit the linear model
      
   # Extract slope and p-value
      slope <- coef(lm_model)[2]
      p_value <- summary(lm_model)$coefficients[2, 4]
      
   # Return the results in a tibble
      tibble(slope = slope, p.value = p_value)}) %>%
      ungroup()
  
  # Determine the number of significant slopes
  significant_slopes_temperate <- slopes_with_pvalues_temperate %>%
    summarise(total = n(),                           # Total number of slopes
      significant = sum(p.value < 0.05, na.rm = TRUE),  # Count slopes with p-value < 0.05, ignoring NAs
      proportion_significant = significant / total * 100) # Percentage of significant slopes
  
  # Print results
  print(significant_slopes_temperate)
  
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
  
  # Graph for Paper (Temperate) - Scale colour changed, dates fixed and filtered to include only models with trendlines/abundance slopes
 
  # Step 1: Randomly sample 20 locations for visualization
  set.seed(123)  # Set seed for reproducibility
  sampled_locations <- temperate_h2_analysis %>%
    distinct(location) %>%  # Get unique locations
    sample_n(20)  # Randomly select 20 locations
  
  # Step 2: Filter your temperate dataset to only include the sampled locations
  filtered_temperate_data <- temperate_h2_analysis %>%
    filter(location %in% sampled_locations$location)
  
  # Step 3: Plot abundance trends for the sampled locations (with slopes)
  ggplot(filtered_temperate_data, aes(x = year, y = abundance, color = fishbase_name)) +
    geom_point(alpha = 0.6) +  # Scatterplot of abundance over time
    geom_smooth(method = "lm", se = FALSE, aes(group = fishbase_name), size = 1) +  # Add regression lines
    labs(
      title = "Abundance Slopes Over Time for Selected Locations in Temperate Region",
      x = "Year",
      y = "Abundance",
      color = "Species") +
    facet_wrap(~ location, scales = "free_y") +  # Create a separate panel for each location
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom")
  
 
  ##########################################

  #Further examination of abundance - are most abundance slopes increasing or decreasing for tropical and temperate regions?

  # Identify whether slopes are increasing and decreasing - TROPICAL
  # Add a column indicating if the slope is increasing or decreasing
  slopes_summary <- slopes %>%
    mutate(slope_direction = ifelse(slope > 0, "Increasing", "Decreasing"))
  
  # Count the number of increasing and decreasing slopes
  slope_summary <- slopes_summary %>%
    count(slope_direction)
  
  #Calculate the percentage of increasing and decreasing slopes
  slope_summary <- slope_summary %>%
    mutate(percent = n / sum(n) * 100) 
  
  print(slope_summary)
  
  # Identify whether slopes are increasing and decreasing
  # Add a column indicating if the slope is increasing or decreasing - Temperate
  slopes_summary2 <- slopes2 %>%
    mutate(slope_direction = ifelse(slope > 0, "Increasing", "Decreasing"))
  
  # Count the number of increasing and decreasing slopes
  slope_summary2 <- slopes_summary2 %>%
    count(slope_direction)
  
  #Calculate the percentage of increasing and decreasing slopes
  slope_summary2 <- slope_summary2 %>%
    mutate(percent = n / sum(n) * 100) 
  
  print(slope_summary2)
  
  