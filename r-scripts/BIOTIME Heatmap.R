# Load data
bio_timesst <- read.csv("data-processed/BioTime summer sst without NAs.csv")

head(bio_timesst)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(tibble)

# Aggregate data by year and rounded SST
bio_timesst_agg <- bio_timesst %>%
  mutate(rounded_sst = round(mean_summer_sst_degC)) %>%
  group_by(year, rounded_sst) %>%
  summarise(mean_abundance = mean(abundance), .groups = 'drop')

# Ensure data is ordered by year and rounded_sst
bio_timesst_agg <- bio_timesst_agg %>%
  arrange(rounded_sst, year)

# Reshape the data to a wide format for heatmap, filling NAs with 0 or another value
bio_timesst_matrix <- bio_timesst_agg %>%
  spread(key = year, value = mean_abundance, fill = 0) %>%
  column_to_rownames("rounded_sst") %>%
  as.matrix()

# Re-order the matrix rows and columns explicitly
bio_timesst_matrix <- bio_timesst_matrix[order(as.numeric(rownames(bio_timesst_matrix))), 
                                         order(colnames(bio_timesst_matrix))]

# Now you can plot the heatmap without clustering
heatmap(bio_timesst_matrix, 
        col = colorRampPalette(c("white", "black"))(100),  # Colors for the heatmap
        scale = "none",  # Do not scale the values
        main = "Fish Abundance Heatmap by Year and SST",
        xlab = "Year",
        ylab = "SST (°C)",
        Rowv = NA,  # Disable row clustering
        Colv = NA)  # Disable column clustering

# Filter for Temperate region
bio_timesst_temperate <- bio_timesst %>%
  filter(region == "Temperate")

# Aggregate data by year and rounded SST
bio_timesst_tempr_agg <- bio_timesst_temperate %>%
  mutate(rounded_sst = round(mean_summer_sst_degC)) %>%
  group_by(year, rounded_sst) %>%
  summarise(mean_abundance = mean(abundance), .groups = 'drop')

# Ensure data is ordered by year and rounded_sst
bio_timesst_tempr_agg <- bio_timesst_tempr_agg %>%
  arrange(rounded_sst, year)

# Reshape the data to a wide format for heatmap, filling NAs with 0 or another value
bio_timesst_tempr_matrix <- bio_timesst_tempr_agg %>%
  spread(key = year, value = mean_abundance, fill = 0) %>%
  column_to_rownames("rounded_sst") %>%
  as.matrix()

# Re-order the matrix rows and columns explicitly
bio_timesst_tempr_matrix <- bio_timesst_tempr_matrix[order(as.numeric(rownames(bio_timesst_tempr_matrix))), 
                                                     order(colnames(bio_timesst_tempr_matrix))]

# Now you can plot the heatmap without clustering
heatmap(bio_timesst_tempr_matrix, 
        col = colorRampPalette(c("white", "black"))(100),  # Colors for the heatmap
        scale = "none",  # Do not scale the values
        main = "Fish Abundance Heatmap by Year and SST in Temperate Latitudes",
        xlab = "Year",
        ylab = "SST (°C)",
        Rowv = NA,  # Disable row clustering
        Colv = NA)  # Disable column clustering

# Filter for Tropical region
bio_timesst_tropical <- bio_timesst %>%
  filter(region == "Tropical")

# Aggregate data by year and rounded SST
bio_timesst_trop_agg <- bio_timesst_tropical %>%
  mutate(rounded_sst = round(mean_summer_sst_degC)) %>%
  group_by(year, rounded_sst) %>%
  summarise(mean_abundance = mean(abundance), .groups = 'drop')

# Ensure data is ordered by year and rounded_sst
bio_timesst_trop_agg <- bio_timesst_trop_agg %>%
  arrange(rounded_sst, year)

# Reshape the data to a wide format for heatmap, filling NAs with 0 or another value
bio_timesst_trop_matrix <- bio_timesst_trop_agg %>%
  spread(key = year, value = mean_abundance, fill = 0) %>%
  column_to_rownames("rounded_sst") %>%
  as.matrix()

# Re-order the matrix rows and columns explicitly
bio_timesst_trop_matrix <- bio_timesst_trop_matrix[order(as.numeric(rownames(bio_timesst_trop_matrix))), 
                                                   order(colnames(bio_timesst_trop_matrix))]

# Now you can plot the heatmap without clustering
heatmap(bio_timesst_trop_matrix, 
        col = colorRampPalette(c("white", "black"))(100),  # Colors for the heatmap
        scale = "none",  # Do not scale the values
        main = "Fish Abundance Heatmap by Year and SST in Tropical Latitudes",
        xlab = "Year",
        ylab = "SST (°C)",
        Rowv = NA,  # Disable row clustering
        Colv = NA)  # Disable column clustering

# Aggregate data by year and rounded SST using sum of abundance
bio_timesst_sum <- bio_timesst %>%
  mutate(rounded_sst = round(mean_summer_sst_degC)) %>%
  group_by(year, rounded_sst) %>%
  summarise(sum_abundance = sum(abundance), .groups = 'drop')

# Ensure data is ordered by year and rounded_sst
bio_timesst_sum <- bio_timesst_sum %>%
  arrange(rounded_sst, year)

# Reshape the data to a wide format for heatmap, filling NAs with 0
bio_timesst_sum_matrix <- bio_timesst_sum %>%
  spread(key = year, value = sum_abundance, fill = 0) %>%
  column_to_rownames("rounded_sst") %>%
  as.matrix()

# Plot the heatmap
heatmap(bio_timesst_sum_matrix, 
        col = colorRampPalette(c("white", "black"))(100),
        scale = "none",
        main = "Sum of Fish Abundance by Year and SST",
        xlab = "Year",
        ylab = "SST (°C)",
        Rowv = NA,
        Colv = NA)

# Repeat similar steps for Temperate and Tropical regions

# For Temperate region
bio_timesst_tempr_sum <- bio_timesst_temperate %>%
  mutate(rounded_sst = round(mean_summer_sst_degC)) %>%
  group_by(year, rounded_sst) %>%
  summarise(sum_abundance = sum(abundance), .groups = 'drop') %>%
  arrange(rounded_sst, year)

bio_timesst_tempr_sum_matrix <- bio_timesst_tempr_sum %>%
  spread(key = year, value = sum_abundance, fill = 0) %>%
  column_to_rownames("rounded_sst") %>%
  as.matrix()

heatmap(bio_timesst_tempr_sum_matrix, 
        col = colorRampPalette(c("white", "black"))(100),
        scale = "none",
        main = "Sum of Fish Abundance by Year and SST in Temperate Latitudes",
        xlab = "Year",
        ylab = "SST (°C)",
        Rowv = NA,
        Colv = NA)

# For Tropical region
bio_timesst_trop_sum <- bio_timesst_tropical %>%
  mutate(rounded_sst = round(mean_summer_sst_degC)) %>%
  group_by(year, rounded_sst) %>%
  summarise(sum_abundance = sum(abundance), .groups = 'drop') %>%
  arrange(rounded_sst, year)

bio_timesst_trop_sum_matrix <- bio_timesst_trop_sum %>%
  spread(key = year, value = sum_abundance, fill = 0) %>%
  column_to_rownames("rounded_sst") %>%
  as.matrix()

heatmap(bio_timesst_trop_sum_matrix, 
        col = colorRampPalette(c("white", "black"))(100),
        scale = "none",
        main = "Sum of Fish Abundance by Year and SST in Tropical Latitudes",
        xlab = "Year",
        ylab = "SST (°C)",
        Rowv = NA,
        Colv = NA)
