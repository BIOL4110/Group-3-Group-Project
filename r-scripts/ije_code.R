
library(dplyr)
library(vegan)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(readr)
library(stringr)
library(lme4)

# SETUP ----
b <- read_csv("data-processed/btfbsst_without_NA.csv")

b2 <- rename(b, mean_sst = mean_summer_sst_degC) %>%
  mutate(mean_sst = round(mean_sst, digits = 2))

totalabundance <- b2 %>%
  group_by(region, year, mean_sst, genus_species) %>%
  summarize(total_abundance = sum(abundance),
            .groups = "drop")

species_richness <- b2 %>% 
  group_by(region, year, mean_sst) %>%
  summarize(species_richness = n_distinct(species),
            .groups = "drop")
df2<- left_join(species_richness, totalabundance, by = c("year", "mean_sst", "region"))

df1 <- left_join(b2,df2, by = c("year", "mean_sst", "region", "genus_species"))

#cross reference genus_species by fishbase_name - not all match so how do we fix?
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

## total abundance change over time by region
df1 %>%
  filter(year >= 2000 & year <= 2010) %>% 
  filter(total_abundance <= 20000) %>% # Filter outliers
  ggplot(aes(x = factor(year), y = total_abundance, color = region, group = region)) + 
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "loess") +
  facet_wrap(~region, scales = "free_y") + 
  scale_color_brewer(palette = "Set2") +
  labs(x = "Year", 
       y = "Total Abundance",
       color = "Region") +
  theme_minimal(base_size = 14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 30), 
      axis.text = element_text(size = 25), 
      axis.title = element_text(size = 30),      
      legend.position = "none") #+ ggsave("figures/TotalAbundanceOverTimebyRegion.png", width = 20, height = 10, dpi = 300)


# WORK ON THIS WITH GROUP
{
  ## TOTAL ABUNDANCE BY GENUS - HOW ELSE CAN WE ANALYZE THIS?
  df1 %>%
    filter(year >= 2000 & year <= 2010) %>% 
    filter(total_abundance <= 3000) %>% 
    group_by(genus) %>% 
    filter(n() >= 6) %>% # Keep only genera with 6 or more data points
    ungroup() %>%
    ggplot(aes(x = factor(year), y = total_abundance)) + 
    geom_point(size = 2.5)+
    geom_smooth(method = "loess")+
    facet_wrap(~genus) +
    labs(x = "Genus", y = "Total Abundance") +
    theme_minimal(base_size = 14)
  
  df1 %>%
    ggplot(aes(x = factor(year), y = species_richness, color = region, group = region)) + 
    geom_point(size = 2.5, alpha = 0.7) +
    geom_smooth(method = "loess") +
    facet_wrap(~region, scales = "free_y") + 
    scale_color_brewer(palette = "Set2") +
    labs(x = "Year", 
         y = "Species Richnes",
         color = "Region") +
    theme_minimal(base_size = 14)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 30), 
          axis.text = element_text(size = 25), 
          axis.title = element_text(size = 30),      
          legend.text = element_text(size = 25)) #+ ggsave("figures/TotalAbundanceOverTimebyRegion.png", width = 20, height = 10, dpi = 300)
  
# Facet plot for species richness
ggplot(df1, aes(x = year, y = species_richness)) +
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
ggplot(df1, aes(x = year, y = abundance)) +
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
  trop <- df1 %>%
    filter(region == "Tropical")
  
  temp <- df1 %>%
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
  ggplot(aes(factor(year),jaccard_index)) +
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

beta_diversity %>%
 filter(year >= 1970 & year <= 2010) %>% 
  ggplot(aes(x = factor(year), y = bc_dist_mean, color = region, group = region)) +
  geom_line(linewidth = 3) +
  geom_smooth(method = "lm")+
  facet_wrap(~region, scales = "free") +
  labs(x = "Year", y = "Bray-Curtis Distance") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()+
  theme_minimal(base_size = 14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 30), 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 30),      
        legend.position = "none") #+ ggsave("figures/BetaDiversityOverTimebyRegion.png", width = 20, height = 10, dpi = 300)

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
