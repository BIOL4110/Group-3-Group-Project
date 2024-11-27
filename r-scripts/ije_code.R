
library(dplyr)
library(vegan)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(readr)
library(stringr)
library(lme4)
library(ggpmisc)

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


##  SEA SURFACE TEMPERATURES RISING OVER THE YEARS - done ----
## only summer months
sst_trend_model <- lm(mean_sst ~ year, data = mean_sst)
summary(sst_trend_model)

# Extract the slope (coefficient of year)
slope <- coef(sst_trend_model)["year"]
cat("The slope of the trend line is:", slope, "°C per year\n")
#positive slope of 0.01C per year - temp increasing that much every year 

#stuarts data
sst_obs3 <- read_csv("data-processed/extracted_sst.csv")

mean_sst <- sst_obs3 %>% 
  #creat column identifying regions by latitude
  mutate(region = case_when(latitude >= 0 & latitude <= 23.5 ~ "Tropical",
                            latitude > 23.5 ~ "Temperate")) %>% 
  group_by(year, region) %>% 
  #calculate mean SST by year in tropical region
  summarize(mean_sst = mean(mean_summer_sst_degC, na.rm = TRUE))

ggplot(mean_sst, aes(x = year, y = mean_sst, group = region)) +
  geom_point(aes(shape = region, colour = region), size = 5) + 
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 3) + 
  facet_wrap(~region, scales = "free_y") + 
  geom_text(data = subset(mean_sst, region == "Tropical"), 
            aes(x = 1990, y = max(mean_sst) + 0.25, 
                label = "y = 0.013803x + 0.276293, R² = 0.644, p < 0.05"), 
            color = "black", size = 6, inherit.aes = FALSE) +
  geom_text(data = subset(mean_sst, region == "Temperate"), 
            aes(x = 1990, y = max(mean_sst) + 0.25, 
                label = "y = 0.020160x - 23.054953, R² = 0.7326, p < 0.05"), 
            color = "black", size = 6, inherit.aes = FALSE) +
  xlab("Year") +
  ylab("Mean SST (°C)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 30), 
    axis.text = element_text(size = 25), 
    axis.title = element_text(size = 30),      
    legend.position = "none") +
  guides(size = "none", shape = guide_legend(override.aes = list(size = 5))) +
  labs(colour = "Region", shape = "Region") #+ ggsave("figures/mean_sst_overtime.png", width = 20, height = 10, dpi = 300)

# HYPOTHESIS ONE - done not using ----
## total abundance change over time by region - done not using 
{
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
      axis.text = element_text(size = 10), 
      axis.title = element_text(size = 30),
      legend.position = "none") #+ ggsave("figures/TotalAbundance.png", width = 25, height = 15, dpi = 300)
}
## NEXT INTREPERT THIS AND DO WITH MEAN-SST TO COMAPRE TOTAL ABUNDANCE / SPECIES RICHNESS TREND ----
## compare species total abundance to temp_min/temp_max
# Fit a linear model for temp_min
lm_min <- lm(total_abundance ~ temp_min + region, data = df1)
plot(lm_min) 

# Fit a linear model for temp_max
lm_max <- lm(total_abundance ~ temp_max + region, data = df1)
summary(lm_max)

lm_combined <- lm(total_abundance ~ temp_min + temp_max + region, data = df1)
summary(lm_combined)

cor_temp_min <- cor(df1$total_abundance, df1$temp_min, use = "complete.obs")
cor_temp_max <- cor(df1$total_abundance, df1$temp_max, use = "complete.obs")

cat("Correlation with temp_min:", cor_temp_min, "\n")
cat("Correlation with temp_max:", cor_temp_max, "\n")

lmer_model <- lmer(total_abundance ~ temp_min + temp_max + (1 | region), data = df1)
summary(lmer_model)

# make a plot that shows the make up of the data and the species we have in it ------
# Species Composition Across Regions
#use new df with phlym and stiff %>%
  group_by(region, species) %>%
  summarise(species_count = n()) %>%
  ggplot(aes(x = region, y = species_count, fill = species)) + 
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Region", 
       y = "Species Count", 
       fill = "Species") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Heatmap of Species Presence Across Regions and Years %>%
  group_by(year, region, species) %>%
  summarise(species_count = n()) %>%
  spread(key = species, value = species_count, fill = 0) %>%
  ggplot(aes(x = year, y = region)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_viridis_c() +
  labs(x = "Year", y = "Region", fill = "Species Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#relative rank abundance plot (Whittaker plot) how evenly or unevenly species are distributed in dataset %>%
    group_by(species) %>%
    summarise(abundance = sum(species_richness, na.rm = TRUE)) %>%
    arrange(desc(abundance)) %>%
    ggplot(aes(x = reorder(species, -abundance), y = abundance)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(x = "Species", 
         y = "Abundance", 
         title = "Species Rank Abundance Plot") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

# beta diversity; COMPARE TO TEMP ----
{   
beta_diversity <- df1 %>%
  group_by(region, year) %>%
  summarize(bc_dist_mean = mean(as.vector(vegdist(as.matrix(total_abundance), method = "bray"))),
            .groups = "drop") %>%
  drop_na(bc_dist_mean)

beta_diversity %>%
 filter(year >= 1970 & year <= 2010) %>% 
  ggplot(aes(x = factor(year), y = bc_dist_mean, color = region, group = region)) +
  geom_line(linewidth = 6) +
  geom_smooth(method = "lm", se = TRUE)+
  facet_wrap(~region, scales = "free") +
  labs(x = "Year", y = "Bray-Curtis Distance") +
  scale_color_brewer(palette = "Set2") +
  geom_text(data = subset(beta_diversity, region == "Temperate"), 
            aes(x = 25, y = 0.69, label = "y = 1.2967x - 0.0003, R² = 0.0023, p > 0.05"), 
            color = "black", size = 12, inherit.aes = FALSE) +
  geom_text(data = subset(beta_diversity, region == "Tropical"), 
            aes(x = 5, y = 0.69, label = "y = 4.9915x - 0.0021, R² = 0.0368, p > 0.05"), 
            color = "black", size = 12, inherit.aes = FALSE) +
  theme_minimal()+
  theme_minimal(base_size = 14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 30), 
        axis.text = element_text(size = 17), 
        axis.title = element_text(size = 30),      
        legend.position = "none") + ggsave("figures/BetaDiversity.png", width = 30, height = 15, dpi = 300)

# Run the linear model and extract p-values and coefficients for each region
trop <- beta_diversity %>% 
  filter(region == "Tropical")
temp <- beta_diversity %>% 
  filter(region == "Temperate")

lm_trop <- lm(bc_dist_mean ~ year, trop)
lm_temp <- lm(bc_dist_mean ~ year, temp)
summary(lm_temp)
trop_sum <- tidy(lm_trop)
temp_sum <- tidy(lm_temp)
} # done 
#Compare beta diversity to thermal tolerance of different species
# Assuming 'beta_diversity' has species and community data, 
beta_diversity2 <- df1 %>%
  group_by(region, year, genus_species) %>%
  summarize(bc_dist_mean = mean(as.vector(vegdist(as.matrix(total_abundance), method = "bray"))),
            .groups = "drop") %>%
  drop_na(bc_dist_mean)

# and 'thermal_tolerance' contains 'species' and 'CTmax' columns
thermal_tolerance <- df1 %>%
  select(c(7,10:11, 14))

# Merge the datasets by species - redo with species richness
merged_data <- left_join(beta_diversity2, thermal_tolerance, by = "genus_species") %>% drop_na()

#relationship between Beta Diversity and Thermal Tolerance (CTmax) - not working 
ggplot(merged_data, aes(x = temp_max, y = bc_dist_mean)) +
  geom_point(aes(color = region), size = 3, alpha = 0.7) +  # Scatter plot of CTmax vs. beta diversity
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Linear regression line
  labs(x = "Critical Thermal Maximum (CTmax)", y = "Bray-Curtis Distance (Beta Diversity)",
       title = "Comparison of Beta Diversity and Thermal Tolerance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_data <- merged_data %>%
  filter(year >= 1960 & year <= 2010) %>%  # Optional: filter for relevant years
  group_by(region, genus_species, year) %>% 
  summarise(
    temp_min = mean(temp_min, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE),
    mean_sst = mean(mean_sst, na.rm = TRUE),
    bc_dist_mean = mean(bc_dist_mean, na.rm = TRUE)) %>% 
  ungroup()

# Plot
ggplot(plot_data, aes(x = genus_species)) +
  geom_segment(aes(xend = genus_species, y = temp_min, yend = temp_max),
               color = "skyblue", linewidth = 1.5) +
  geom_point(aes(y = mean_sst), color = "orange", size = 3) +
  # geom_point(aes(y = bc_dist_mean), color = "red", size = 2, alpha = 0.7) +
  facet_wrap(~region, scales = "free_x") +  # Facet by region
  labs(
    x = "Species",
    y = "Temperature (°C) / Bray-Curtis Distance",
    title = "Thermal Breadth, Mean SST, and Bray-Curtis Distance by Species",
    caption = "Blue bars: Thermal breadth (Temp Min to Max)\nOrange points: Mean SST\nRed points: Bray-Curtis Distance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12)
  )

{
# 3. Statistical analysis
# Compute correlation between beta diversity and thermal tolerance
cor_test <- cor.test(merged_data$bc_dist_mean, merged_data$temp_max, method = "pearson")

# Output the correlation result
cat("Correlation between Beta Diversity and Thermal Tolerance:", cor_test$estimate, "\n")
cat("p-value:", cor_test$p.value, "\n")

# Optional: Regression analysis
lm_model <- lm(bc_dist_mean ~ temp_max, data = merged_data)
summary(lm_model)  # Get the summary of the regression model
}

# Calculate Shannon diversity ; fix X -AXIS DISPLAY DIFFERENTLY ? COMPARE TO TEMP ----
#for each group (species richness over time) 
# Assuming you group data by region and year for diversity calculation
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

df1_shannon <- df1 %>%
  group_by(year, region) %>%
  summarise(shannon_div = -sum((species_richness / sum(species_richness)) * log(species_richness / sum(species_richness)), na.rm = TRUE))

df1_shannon %>%
  ggplot(aes(x = year, y = shannon_div, color = region)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~region, scales = "free") + 
  labs(x = "Year", 
       y = "Shannon Diversity Index",
       color = "Region") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 15))
