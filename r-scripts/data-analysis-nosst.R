## 05-data-analysis-nosst
# Nov 6. 2024
# IN, 

library(dplyr)
library(vegan)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(readr)
library(stringr)

df <- read_csv("data-processed/full_harmonized_btgt.csv")
df1 <- read_csv("data-processed/BioTime summer sst without NAs.csv")
df2 <- read_csv("data-processed/BioTime_processed.csv")

## use stuarts code to measure beta diversity - changes in Beta diversity compared to SST
# comapre species richness/abundnace to year/ sst / compare between diff families 
# glm to  understand relationship between species abundance and year 

# Calculate species richness per year or SST level
#round sst mean and create groups to look at all temps
#CREATE TEMPERATURE RANGE COLUMN with even sized groups to facet wrap my plots by
range(df1$mean_summer_sst_degC) # 9.48 to 29.31

# Define non-overlapping temperature breakpoints
temp_breaks <- c(9.40, 15.00, 21.00, 27.00, 29.31)  # Explicit range endpoints
temp_labels <- c("Low (9.40-15.00)", "Medium-Low (15.01-21.00)", 
                 "Medium-High (21.01-27.00)", "High (27.01-29.31)")

# Add temperature range column with explicit labels for non-overlapping ranges
df_richness <- df1 %>%
  mutate(
    mean_summer_sst_degC = round(mean_summer_sst_degC, digits = 2),
    temp_range = case_when(
      mean_summer_sst_degC >= 9.40 & mean_summer_sst_degC <= 15.00 ~ "Low (9.40-15.00)",
      mean_summer_sst_degC > 15.00 & mean_summer_sst_degC <= 21.00 ~ "Medium-Low (15.01-21.00)",
      mean_summer_sst_degC > 21.00 & mean_summer_sst_degC <= 27.00 ~ "Medium-High (21.01-27.00)",
      mean_summer_sst_degC > 27.00 & mean_summer_sst_degC <= 29.31 ~ "High (27.01-29.31)")) %>%
  group_by(year, mean_summer_sst_degC, temp_range) %>%
  summarize(species_richness = n_distinct(species),
    total_abundance = sum(abundance),
    .groups = "drop")

# Plot species richness trends over time by temperature range
ggplot(df_richness, aes(x = year, y = species_richness, color = temp_range)) +
  geom_jitter(alpha = 0.3) +
  facet_wrap(~temp_range) +  # Facet by temperature range
  labs(
    title = "Species Richness Over Time by SST Level",
    x = "Year",
    y = "Species Richness",
    color = "Temperature Range C") +
  theme_minimal()

## now fix it so it keeps all the columns df1 and the information made from df_richness to make a new df  
df_combined <- df1 %>%
  mutate(mean_summer_sst_degC = round(mean_summer_sst_degC, digits = 2)) %>%
  left_join(df_richness, by = c("year", "mean_summer_sst_degC"))

# trying some random stuff -----
ggplot(df_combined, aes(x = year, y = species_richness, color = temp_range)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1.2) +  # Aggregate by mean richness
  labs(
    title = "Species Richness Over Time by Temperature Range",
    x = "Year",
    y = "Mean Species Richness",
    color = "Temperature Range"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(df_combined, aes(x = year, y = log(total_abundance), fill = temp_range)) +
  geom_area(stat = "summary", fun = sum, alpha = 0.6) +  # Aggregate by sum abundance
  labs(
    title = "Total Abundance Over Time by Temperature Range",
    x = "Year",
    y = "Total Abundance",
    fill = "Temperature Range"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(df_combined, aes(x = temp_range, y = log(abundance), fill = temp_range)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Abundance Distribution by Temperature Range",
    x = "Temperature Range",
    y = "Abundance"
  ) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_combined, aes(x = year, y = species_richness, fill = region)) +
  geom_col(position = "dodge", stat = "summary", fun = mean) +
  labs(
    title = "Species Richness by Year and Region",
    x = "Year",
    y = "Mean Species Richness",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

df_combined %>%
  group_by(region, year) %>%
  summarize(total_abundance = sum(total_abundance), .groups = "drop") %>%
  ggplot(aes(x = year, y = log(total_abundance), fill = region)) +
  geom_area(alpha = 0.6) +
  labs(
    title = "Total Abundance by Region Over Time",
    x = "Year",
    y = "Total Abundance",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#STATISTICAL ANALYSIS ----
# linear model species richness change over time
lm_richness <- lm(species_richness ~ year, data = df_combined)
summary(lm_richness)

# relationship between abundance and multiple predictors - region (tropical?) statistically significant
glm_abundance <- glm(abundance ~ year + mean_summer_sst_degC + region, 
                     data = df_combined, 
                     family = gaussian(link = "log"))  # Assuming abundance is count data
summary(glm_abundance)



#post meeting graphs - nov18

species_richness <- full2 %>%
  group_by(region, year) %>%
  summarize(species_richness = n_distinct(species))

# shannon diversity stuff 
library(vegan)

species_matrix <- full2 %>%
  pivot_wider(names_from = species, values_from = abundance, values_fill = 0) %>%
  select(-site_id, -year)

# Shannon diversity index
shannon_index <- diversity(as.matrix(species_matrix), index = "shannon")

# Add to data
combined_data$shannon_index <- shannon_index


# Fit a model to test species richness trends in tropical regions
# linear mixed-effects model
library(lme4)

unique(full2$region)

# no tropical data - cannot predict how ctmax/min affects it but can separately
# tropical_data <- full2 %>% filter(region == "Tropical")

temperate_data <- full2 %>% filter(region == "Temperate") %>%
  group_by(year, mean_sst) %>%
  summarize(species_richness = n_distinct(species),
            total_abundance = sum(abundance),
            .groups = "drop")

richness_model_temperate <- lmer(species_richness ~ year * mean_sst + (1 | latitude),
  data = temperate_data)

summary(richness_model_temperate)

