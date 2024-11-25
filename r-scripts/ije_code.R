
library(dplyr)
library(vegan)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(readr)
library(stringr)
library(lme4)

# START HERE ----
b <- read_csv("data-processed/btfbsst_without_NA.csv")

b2 <- rename(b, mean_sst = mean_summer_sst_degC) %>%
  mutate(mean_sst = round(mean_sst, digits = 2))

species_richness <- b2 %>%
  group_by(region, year, mean_sst) %>%
  summarize(species_richness = n_distinct(species),
            total_abundance = sum(abundance),
            .groups = "drop")

df1 <- left_join(b2, species_richness, by = c("year", "mean_sst", "region"))
#cross refence genus_species by fishbase_name - not all match so how do we fix?
## is it false bc it has an underscore - check 

##  SEA SURFACE TEMPERATURES RISING OVER THE YEARS ----
## only summer months
mean_sst_by_year <- df1 %>%
  group_by(year) %>%
  summarize(mean_sst = mean(mean_sst, na.rm = TRUE))

ggplot(mean_sst_by_year, aes(x = year, y = mean_sst)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red") +
  labs(title = "Mean Sea Surface Temperature Over the Years",
    x = "Year",
    y = "Mean SST (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## matching the species data - but can we also do up to 2024
ggplot(mean_sst_by_year, aes(x = year, y = mean_sst)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "darkgreen", linetype = "dashed") +
  labs(
    title = "Mean Sea Surface Temperature Over the Years (with Trendline)",
    x = "Year",
    y = "Mean SST (°C)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

sst_trend_model <- lm(mean_sst ~ year, data = mean_sst_by_year)
summary(sst_trend_model)

# Extract the slope (coefficient of year)
slope <- coef(sst_trend_model)["year"]
cat("The slope of the trend line is:", slope, "°C per year\n")
#positive slope od 0.213C per year - temp increasing that much every year 

# HYPOTHESIS ONE ----
# facetwrap over years how it is changing over years 
## fit slopes with models of species richness /abundance overtime 
#fishbase merged with biotime  - has species clean been run through harmonization?

## total abundance change over time by region ## crop out before 2000
df1 %>%
  filter(year > 2000) %>% 
  ggplot(aes(x = year, y = total_abundance, color = region)) +
  geom_line() +
  labs(title = "Total Abundance Over Time by Region",
       x = "Year", y = "Total Abundance") +
  theme_minimal(base_size = 14)

# specis richness by region 
ggplot(df1, aes(x = region, y = species_richness, fill = region)) +
  geom_boxplot() +
  labs(title = "Species Richness by Region",
       x = "Region", y = "Species Richness") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set3")

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
#qq residuals is good = normally distributed data 
richness_model <- lm(species_richness ~ year + mean_sst, data = df1)
summary(richness_model)

# 1. Linear model for abundance over time
abundance_model <- lm(abundance ~ year + mean_sst, data = df1)
summary(abundance_model)

# 2.  Model for abundance trends 
abundance_model <- lmer(
  total_abundance ~ year * mean_sst + (1 | genus_species / region),
  data = df1)

summary(abundance_model) ## consider rescaling

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
beta_diversity <- df1 %>%
  group_by(region, year) %>%
  summarize(bc_dist_mean = mean(as.vector(vegdist(as.matrix(total_abundance), method = "bray"))),
            .groups = "drop") %>%
  drop_na(bc_dist_mean)

ggplot(beta_diversity, aes(x = year, y = bc_dist_mean, color = region)) +
  geom_line() +
  labs(title = "Beta Diversity Over Time by Region",
       x = "Year", y = "Bray-Curtis Distance") +
  theme_minimal()

# Perform PCoA
pcoa <- cmdscale(beta_diversity, k = 2, eig = TRUE)

# Create a data frame for plotting
pcoa_df <- as.data.frame(pcoa$points)
colnames(pcoa_df) <- c("PC1", "PC2")
pcoa_df$region <- species_matrix$region
pcoa_df$year <- species_matrix$year

# PCoA plot FIXXXX
ggplot(pcoa_df, aes(x = PC1, y = PC2, color = region, shape = factor(year))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = paste("PC1 (", round(pcoa$eig[1] / sum(pcoa$eig) * 100, 1), "%)", sep = ""),
    y = paste("PC2 (", round(pcoa$eig[2] / sum(pcoa$eig) * 100, 1), "%)", sep = ""),
    color = "Year",
    shape = "Region",
    title = "PCoA of Beta Diversity (Bray-Curtis)")
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

# HYPOTHESIS TWO ----
# compare thermal tolerance to species abundance 
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
