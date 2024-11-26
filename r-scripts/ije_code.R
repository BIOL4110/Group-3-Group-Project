
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
  group_by(region, year) %>%
  summarize(species_richness = n_distinct(species),
            .groups = "drop")
df2<- left_join(species_richness, totalabundance, by = c("year", "region"))

df1 <- left_join(b2,df2, by = c("year", "mean_sst", "region", "genus_species"))

#cross reference genus_species by fishbase_name - not all match so how do we fix?
## is it false bc it has an underscore - check 


##  SEA SURFACE TEMPERATURES RISING OVER THE YEARS ----
## only summer months
sst <- read_csv("data-processed/extracted_sst.csv")

mean_sst_by_year <- sst %>%
  group_by(latitude, longitude, year) %>%
  summarize(mean_sst = mean(mean_summer_sst_degC, na.rm = TRUE))

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
  filter(total_abundance <= 5000) %>% # Filter outliers - heighest is 40 000 but are major outliers
  ggplot(aes(x = factor(year), y = total_abundance, color = region, group = region)) + 
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "loess") +
  facet_wrap(~region, scales = "free") + 
  scale_color_brewer(palette = "Set2") +
  labs(x = "Year", 
       y = "Total Abundance",
       color = "Region") +
  theme_minimal(base_size = 14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 30), 
      axis.text = element_text(size = 15), 
      axis.title = element_text(size = 30),      
      legend.position = "none") #+ ggsave("figures/TotalAbundanceOverTimebyRegion.png", width = 20, height = 10, dpi = 300)

# how to display slope line??
loess_slopes <- df1 %>%
  filter(total_abundance <= 5000) %>%
  group_by(region) %>%
  mutate(loess_model = list(loess(total_abundance ~ as.numeric(as.character(year)))),
         fitted = predict(loess_model[[1]], se = TRUE)$fit,
         slope = c(NA, diff(fitted) / diff(as.numeric(as.character(year))))) %>%
  ungroup()

loess_slopes %>%
  ggplot(aes(x = as.numeric(as.character(year)), y = total_abundance, color = region, group = region)) + 
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) + # LOESS smoothing
  facet_wrap(~region, scales = "free") + 
  scale_color_brewer(palette = "Set2") +
  labs(x = "Year", 
       y = "Total Abundance",
       color = "Region") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 30),      
        legend.position = "none") +
  geom_text(data = loess_slopes %>% group_by(region) %>% summarize(slope_label = paste("Slope:", round(mean(slope, na.rm = TRUE), 2))),
            aes(x = Inf, y = Inf, label = slope_label),
            hjust = 1.1, vjust = 1.1, inherit.aes = FALSE)

# WORK ON THIS WITH GROUP ------
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
  
## histogram of data
  df1 %>%
    ggplot(aes(x = factor(year), y = species_richness, color = region, group = region)) + 
    geom_point(size = 2.5, alpha = 0.7) +
    geom_smooth(method = "loess") +
    facet_wrap(~region, scales = "free") + 
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

  # species richness histogram
  df1 %>%
    ggplot(aes(x = species_richness, fill = region)) + 
    geom_histogram(binwidth = 5, alpha = 0.7, color = "black") + 
    facet_wrap(~region, scales = "free") + 
    scale_fill_brewer(palette = "Set2") +
    labs(x = "Species Richness", 
         y = "Count",
         fill = "Region") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 30), 
          axis.text = element_text(size = 25), 
          axis.title = element_text(size = 30),      
          legend.text = element_text(size = 25))
  
 # Facet plot for species richness
ggplot(df1, aes(x = year, y = species_richness)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ region, scales = "free") +
  theme_minimal() +
  labs(x = "Year",
    y = "Species Richness",
    title = "Change in Species Richness Over Time by Region")

# Linear model for species richness over time
#qq residuals is good = normally distributed data 
richness_model <- lm(species_richness ~ year + mean_sst, data = df1)

lm1 <- lm(species_richness ~ year + region, data = df1)
summary(lm1)
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
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 30),      
        legend.position = "none") #+ ggsave("figures/BetaDiversityOverTimebyRegion.png", width = 20, height = 10, dpi = 300)

str(beta_diversity)
# now PCOA
}

# Calculate Shannon diversity for each group (species richness over time) 
# Assuming you group data by region and year for diversity calculation
{
shannon_div <- df1 %>%
  group_by(region, year) %>%
  summarise(Shannon = diversity(abundance, index = "shannon"))

shannon_div %>% 
ggplot(aes(x = factor(year), y = Shannon, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~region, scales = "free") + 
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Year",
       y = "Shannon Diversity Index",
       fill = "Region")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 30),      
        legend.position = "none")#+ ggsave("figures/ShannonDiversitybyRegionandYear.png", width = 20, height = 10, dpi = 300)
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
}

# NEXT STEPS with DF1 ----
#1. compare species richness and abundance to thermal tolerance 
#2. complete regressions and other statistical models on change in species richness/abundance over time, with different regions, different species or tmax/tmin
#3. add slope to beta-diversity plot / species richness plot