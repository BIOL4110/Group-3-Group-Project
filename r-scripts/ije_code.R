# starting fresh 

library(dplyr)
library(vegan)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(readr)
library(stringr)
library(lme4)

full <- read_csv("data-processed/full_harm_all_data.csv")

full2 <- rename(full, mean_sst = mean_summer_sst_degC) %>%
  mutate(mean_sst = round(mean_sst, digits = 2))

species_richness <- full2 %>%
  group_by(region, year, mean_sst) %>%
  summarize(species_richness = n_distinct(species),
            total_abundance = sum(abundance),
            .groups = "drop")

df_combined <- left_join(full2, species_richness, by = c("year", "mean_sst", "region"))

temperate_data <- df_combined %>% filter(region == "Temperate")

temperate_data %>%
  ggplot(aes(x = th_genus_species, y = species_richness)) +
  geom_boxplot(stat = "boxplot")

# FIND TOTAL ABUNDANCE
# Model for abundance trends
abundance_model <- lmer(
  total_abundance ~ year * mean_sst + (1 | th_genus_species / region),
  data = temperate_data
)

summary(abundance_model) ## consider rescaling

# how to fix the linear model 
lm <- lm(species_richness ~ year + mean_sst, data = temperate_data)
summary(lm)


##beta diversity 
beta_diversity <- df_combined %>%
  group_by(region, year) %>%
  summarize(bc_dist = vegdist(as.matrix(total_abundance), method = "bray"))

# Visualize beta diversity over time
ggplot(beta_diversity, aes(x = year, y = bc_dist, color = region)) +
  geom_line() +
  labs(title = "Beta Diversity Over Time by Region",
       x = "Year", y = "Bray-Curtis Distance") +
  theme_minimal()


df2 <- df_combined %>% drop_na()
  
## DF combined - only temperate 
## total abundance change over time by region
df_combined %>%
  ggplot(aes(x = year, y = scale(total_abundance), color = region)) +
  geom_line() +
  labs(title = "Total Abundance Over Time by Region",
       x = "Year", y = "Total Abundance") +
  theme_minimal(base_size = 14)

# specis richness by region 
ggplot(df_combined, aes(x = region, y = species_richness, fill = region)) +
  geom_boxplot() +
  labs(title = "Species Richness by Region",
       x = "Region", y = "Species Richness") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set3")

beta_diversity <- df_combined %>%
  group_by(region, year) %>%
  summarize(bc_dist = as.matrix(vegdist(as.matrix(total_abundance), method = "bray"))) %>%
  pivot_longer(cols = starts_with("bc_dist"), names_to = "comparison", values_to = "dissimilarity")

ggplot(beta_diversity, aes(x = year, y = comparison, fill = dissimilarity)) +
  geom_tile() +
  labs(title = "Bray-Curtis Dissimilarity Over Time",
       x = "Year", y = "Comparison", fill = "Dissimilarity") +
  theme_minimal(base_size = 14)

# HYPOTHESIS ONE 
# facetwrap over years how it is changing over years 
## fit slopes with models of species richness /abundance overtime 
#fishbase merged with biotime  - has species clean been run through harmonization?
a <- read_csv("data-processed/btfb_without_NA.csv")

a2 <- a %>%
  group_by(region, year) %>%
  summarize(species_richness = n_distinct(species),
            total_abundance = sum(abundance),
            .groups = "drop")

a3 <- left_join(a, a2, by = c("year", "region"))

# Summarize the data by year - FILTER HERE TO ONLY LOOK AT 2000-2010
{
yearly_summary <- a3 %>%
  group_by(year) %>%
  summarise(
    mean_species_richness = mean(species_richness, na.rm = TRUE),
    total_abundance = sum(abundance, na.rm = TRUE)
  )

# Facet plot for species richness
ggplot(a3, aes(x = year, y = species_richness)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ region) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Species Richness",
    title = "Change in Species Richness Over Time by Region"
  )

# Facet plot for abundance
ggplot(a3, aes(x = year, y = abundance)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  facet_wrap(~ region) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Abundance",
    title = "Change in Abundance Over Time by Region"
  )

# Linear model for species richness over time
richness_model <- lm(species_richness ~ year, data = a3)
summary(richness_model)

# Linear model for abundance over time
abundance_model <- lm(abundance ~ year, data = a3)
summary(abundance_model)

# Extract slopes for each region
slopes <- a3 %>%
  group_by(region) %>%
  summarise(
    richness_slope = coef(lm(species_richness ~ year))[2],
    abundance_slope = coef(lm(abundance ~ year))[2]
  )

print(slopes) ## tropical all negative

# Plot slopes for species richness
ggplot(slopes, aes(x = region, y = richness_slope)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(
    x = "Region",
    y = "Slope (Species Richness vs. Year)",
    title = "Trends in Species Richness Over Time by Region"
  )

# Plot slopes for abundance
ggplot(slopes, aes(x = region, y = abundance_slope)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal() +
  labs(
    x = "Region",
    y = "Slope (Abundance vs. Year)",
    title = "Trends in Abundance Over Time by Region"
  )
}

# Calculate Jaccard similarity index for each year (presence/absence) + statistically significant 0.05 
# Jaccard similarity index compares two sets of data to see how similar they are. It might be anywhere between 0 and 1. The greater the number, the closer the two sets of data are
{
  trop <- a3 %>%
    filter(region == "Tropical")
  
  temp <- a3 %>%
    filter(region == "Temperate")
  
jaccard_similarity <- function(year) {
  species_trop <- trop %>% filter(year == !!year) %>% pull(genus_species) %>% unique()
  species_temp <- temp %>% filter(year == !!year) %>% pull(genus_species) %>% unique()
  
  intersection <- length(intersect(species_trop, species_temp))
  union <- length(union(species_trop, species_temp))
  
  if (union == 0) return(NA) else return(intersection / union)
}

years <- intersect(trop$year, temp$year)

jaccard_indices <- map_dbl(years, jaccard_similarity)

jaccard_results <- data.frame(year = years, jaccard_index = jaccard_indices)

#plot the results of jacaard
jaccard_results %>% 
  ggplot(aes(year,jaccard_index)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylab("Jaccard index") + 
  xlab("Year") +
  theme_classic()

jaccard_model <- lm(jaccard_index ~ year, data = jaccard_results)

summary(jaccard_model) # statisitcally significant!
}

# beta diversity (compare temp/tropical regions how similar) - based on abundance data bray-curtis
# Calculate beta diversity (Bray-Curtis) between regions or years
# FIX PCA PLOT
{
species_matrix <- a3 %>%
  group_by(region, year, genus_species) %>%
  summarise(total_abundance = sum(abundance)) %>%
  pivot_wider(names_from = genus_species, values_from = total_abundance, values_fill = 0)

# Compute beta diversity (e.g., Bray-Curtis)
beta_div <- vegdist(species_matrix[, -c(1:2)], method = "bray")

# Perform PCoA
pcoa <- cmdscale(beta_div, k = 2, eig = TRUE)

# Create a data frame for plotting
pcoa_df <- as.data.frame(pcoa$points)
colnames(pcoa_df) <- c("PC1", "PC2")
pcoa_df$region <- species_matrix$region
pcoa_df$year <- species_matrix$year

# PCoA plot FIXXXX
ggplot(pcoa_df, aes(x = PC1, y = PC2, color = region, shape = factor(year))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    x = paste("PC1 (", round(pcoa$eig[1] / sum(pcoa$eig) * 100, 1), "%)", sep = ""),
    y = paste("PC2 (", round(pcoa$eig[2] / sum(pcoa$eig) * 100, 1), "%)", sep = ""),
    color = "Year",
    shape = "Region",
    title = "PCoA of Beta Diversity (Bray-Curtis)"
  )
}

# Calculate Shannon diversity for each group (species richness over time) 
# Assuming you group data by region and year for diversity calculation
#FILTER YEAR 
{
shannon_div <- a3 %>%
  group_by(region, year) %>%
  summarise(Shannon = diversity(abundance, index = "shannon"))

ggplot(shannon_div, aes(x = factor(year), y = Shannon, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Shannon Diversity Index",
    fill = "Region",
    title = "Shannon Diversity by Region and Year"
  )
}

## SHOW HOW SEA SURFACE TEMPERATURES ARE RISING OVER THE YEARS USING MEAN SST !!

# HYPOTHESIS TWO  - compare thermal tolerance to species abundance 
# phylogentic computation to fill in missing ctmax data based on phylogeny of the species 
## use ctmax for same genus or family - match at genus level
#scatter plot works better for speciees richness
{
# Compute genus-level averages for CTmax
genus_ctmax <- a3 %>%
  group_by(genus) %>%
  summarise(genus_avg_ctmax = mean(TempMax, na.rm = TRUE))

# Merge genus-level CTmax back into the main dataset
a3 <- a3 %>%
  left_join(genus_ctmax, by = "genus")

a3 <- a3 %>%
  mutate(TempMax_filled = ifelse(is.na(TempMax), genus_avg_ctmax, TempMax))

# Scatterplot: Abundance vs. CTmax
ggplot(a3, aes(x = TempMax_filled, y = abundance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~region) +
  theme_minimal() +
  labs(
    x = "Thermal Tolerance (CTmax)",
    y = "Species Abundance",
    title = "Relationship between CTmax and Species Abundance")

# Linear regression model
model <- lm(abundance ~ TempMax_filled, data = a3)
summary(model)
}
