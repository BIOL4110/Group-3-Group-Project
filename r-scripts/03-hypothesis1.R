#03-hypothesis1.R
# Packages 
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
library(viridis)

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
  theme_bw()+
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
  select(c(7,10:11, 14:17))

# Merge the datasets by species - redo with species richness
merged_data <- left_join(beta_diversity2, thermal_tolerance, by = c("genus_species")) %>% drop_na()

#relationship between species richness and Thermal Tolerance (CTmax) - not working 
ggplot(merged_data, aes(x = mean_sst, y = species_richness, group = temp_max)) +
  geom_boxplot()+
  facet_wrap(~region, scales = "free")+
  labs(x = "Critical Thermal Maximum (CTmax)", y = "species richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#geom_point(aes(color = region), size = 3, alpha = 0.7) +  # Scatter plot of CTmax vs. beta diversity
#geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Linear regression line

plot_data <- merged_data %>%
  filter(year >= 1960 & year <= 2010) %>%  # Optional: filter for relevant years
  group_by(region, genus_species, year) %>% 
  summarise(
    temp_min = mean(temp_min, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE),
    mean_sst = mean(mean_sst, na.rm = TRUE),
    bc_dist_mean = mean(bc_dist_mean, na.rm = TRUE)) %>% 
  ungroup()

 ## remove data if 
ggplot(plot_data, aes(x = genus_species)) +
  geom_segment(aes(xend = genus_species, y = temp_min, yend = temp_max),
               color = "skyblue", linewidth = 1.5) +
  geom_point(aes(y = mean_sst), color = "orange", size = 3) +
  #geom_point(aes(y = species_richness), color = "red", size = 2, alpha = 0.7) +
  facet_wrap(~region, scales = "free_x") +  # Facet by region
  labs(x = "Species",
    y = "Temperature (°C) / Bray-Curtis Distance",
    title = "Thermal Breadth, Mean SST, and Bray-Curtis Distance by Species",
    caption = "Blue bars: Thermal breadth (Temp Min to Max)\nOrange points: Mean SST\nRed points: Bray-Curtis Distance") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12))

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

# Calculate Shannon diversity ; match to species ----
{
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
        legend.position = "none") 

df1_shannon <- df1 %>%
  group_by(year, region) %>%
  summarise(shannon_div = -sum((species_richness / sum(species_richness)) * log(species_richness / sum(species_richness)), na.rm = TRUE))

library(gridExtra)
library(RColorBrewer)
library(grid)

tropical_plot <- df1_shannon %>%
  filter(year <= 2010, region == "Tropical", shannon_div >= 1) %>%
  ggplot(aes(x = year, y = shannon_div, color = region)) + 
  geom_smooth(method = "lm", se = TRUE, linewidth = 4) +
  geom_point(size = 4, alpha = 0.7) +
  ggtitle("Tropical") +
  geom_text(data = subset(df1_shannon), 
            aes(x = 2006, y = max(shannon_div) + 0.25, 
                label = "y = 0.1484x + -292.1347, R² = 0.543, p < 0.05"), 
            color = "black", size = 12, inherit.aes = FALSE) +
  scale_color_manual(values = "#FC8D62") + 
  scale_x_continuous(breaks = seq(min(df1_shannon$year), max(df1_shannon$year), by = 1)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(hjust = 1),
    axis.text = element_text(size = 15),
    axis.title = element_blank(), # Remove axis titles
    plot.title = element_text(size = 20, hjust = 0.5), # Center the title
    legend.position = "none")

temperate_plot <- df1_shannon %>%
  filter(year <= 2010, region == "Temperate", shannon_div >= 1) %>%
  ggplot(aes(x = year, y = shannon_div, color = region)) + 
  geom_smooth(method = "lm", se = TRUE, linewidth = 4) +
  geom_point(size = 4, alpha = 0.7) +
  ggtitle("Temperate") +
  geom_text(data = subset(df1_shannon), 
            aes(x = 1985, y = max(shannon_div) + 0.50, 
                label = "y = 0.0655x + -125.1170, R² = 0.394, p < 0.05"), 
            color = "black", size = 12, inherit.aes = FALSE) +
  scale_color_manual(values = "#66C2A5") + 
  scale_x_continuous(breaks = seq(min(df1_shannon$year), max(df1_shannon$year), by = 5)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(hjust = 1),
    axis.text = element_text(size = 15),
    axis.title = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.5), 
    legend.position = "none")

plot(temperate_plot)
# Create a shared axis label as a grob
x_axis_label <- textGrob("Year", gp = gpar(fontsize = 25))
y_axis_label <- textGrob("Shannon Diversity Index", rot = 90, gp = gpar(fontsize = 25))

# Arrange the plots with the shared labels
combined <- grid.arrange(arrangeGrob(grobs = list(temperate_plot, tropical_plot),
    layout_matrix = rbind(c(1, 2)), 
    widths = c(1, 1)), 
  left = y_axis_label, 
  bottom = x_axis_label) 

ggsave(filename = "figures/shannon_diversity2.png", plot = combined, width = 30, height = 15, dpi = 300)
} # done

# macth df1shnnon to species 
merged2 <- left_join(df1_shannon, thermal_tolerance, by = "year")
plot_data <- merged2 %>%
  filter(year >= 1960 & year <= 2010) %>%  # Optional: filter for relevant years
  group_by(region, year) %>% 
  summarise(
    temp_min = mean(temp_min, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE),
    mean_sst = mean(mean_sst, na.rm = TRUE)) %>% 
  ungroup()

# Plot
ggplot(plot_data, aes(x = genus_species)) +
  geom_segment(aes(xend = genus_species, y = temp_min, yend = temp_max),
               color = "skyblue", linewidth = 1.5) +
  geom_point(aes(y = mean_sst), color = "orange", size = 3) +
  #geom_point(aes(y = species_richness), color = "red", size = 2, alpha = 0.7) +
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
# species richness by sst- finish LMs ----
trop <- merged_data %>%
  filter(region == "Tropical") %>%
  group_by(year, mean_sst) %>%
  summarise(mean_species_richness = mean(species_richness, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

lm_model <- lm(mean_species_richness ~ mean_sst, data = trop)
summary(lm_model)
p_value <- summary(lm_model)$coefficients[2, 4]  # p-value for mean_sst
p_value_display <- ifelse(p_value < 0.05, "< 0.05", paste("= ", round(p_value, 4)))

# Create the plot with p-value in the caption
ggplot(trop, aes(x = mean_sst, y = mean_species_richness, colour = mean_species_richness)) + 
  geom_jitter(alpha = 0.7, size = 4) + 
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", linewidth = 2) + 
  facet_wrap(~year) +
  scale_color_viridis(option = "D") +
  labs(x = "Mean SST°C", 
    y = "Average Species Richness", 
    title = "Tropical",
    caption = paste0(
      "SST % Change: ", round(change_data$percentage_change_sst, 2), "%\n",
      "Species Richness % Change: ", round(change_data$percentage_change_richness, 2), "%\n",
      "p-value = ", p_value_display)) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1, size = 15),
    text = element_text(size = 18),
    axis.title = element_text(size = 25),
    plot.title = element_text(hjust = 0.5, size = 30),
    plot.caption = element_text(hjust = 1, vjust = 15, size = 20),
    legend.position = "none") #+ ggsave("figures/tropical_sst_speciesrich.png", width = 20, height = 10, dpi = 300)

#clculate change of avg species richness and mean sst from first to last year and how to plot that
change_data <- trop %>%
  summarise(
    first_year_sst = mean(mean_sst[year == min(year)], na.rm = TRUE),
    last_year_sst = mean(mean_sst[year == max(year)], na.rm = TRUE),
    first_year_richness = mean(mean_species_richness[year == min(year)], na.rm = TRUE),
    last_year_richness = mean(mean_species_richness[year == max(year)], na.rm = TRUE)) %>%
  mutate(change_sst = last_year_sst - first_year_sst,
    change_richness = last_year_richness - first_year_richness,
    percentage_change_sst = round((change_sst / first_year_sst) * 100, 2),
    percentage_change_richness = round((change_richness / first_year_richness) * 100, 2))
## change in sst -0.5275974 # 1.8% decrease
# change richness 7.37382 63% increase

temp_data <- merged_data %>%
  filter(region == "Temperate") %>%
  group_by(year, mean_sst) %>%
  summarise(mean_species_richness = mean(species_richness, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

# Filter for SST 0-20°C
temp_data_0_20 <- temp_data %>%
  filter(mean_sst >= 0 & mean_sst <= 20) %>%
  rename(sst20 = mean_sst, msr20 = mean_species_richness)

# Filter for SST 25-30°C
temp_data_25_30 <- temp_data %>%
  filter(mean_sst >= 25 & mean_sst <= 30) %>%
  rename(sst25 = mean_sst, msr25 = mean_species_richness)

# Combine the two datasets
combined_temp_data <- temp_data_0_20 %>%
  full_join(temp_data_25_30, by = "year")

all_long <- combined_temp_data %>%
  pivot_longer(cols = c(sst20, msr20, sst25, msr25), 
               names_to = c(".value", "range"), 
               names_pattern = "(sst|msr)(\\d+)") %>%
  mutate(range = case_when(range == "20" ~ "10-20°C",range == "25" ~ "25-30°C"))

# Calculate the first and last years
first_year <- min(all_long$year)
last_year <- max(all_long$year)

# Filter for every 5 years, including the first and last year
temperate_20 <- all_long %>%
  filter(range == "10-20°C") %>%
  filter(year %% 5 == 0 | year == first_year | year == last_year)

# Plot with facets for the filtered years
temperate_20 %>%
  drop_na(sst) %>%
  ggplot(aes(x = sst, y = msr, colour = msr)) + 
  geom_jitter(alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  scale_color_viridis(option = "D") +
  facet_wrap(~year, scales = "free_x") +  # Facet wrap for selected years
  labs(x = "Mean SST°C", 
       y = "Average Species Richness", 
       title = "Temperate (10-20°C)",
       caption = paste0(
         "SST % Change: ", round(change$percentage_change_sst, 2), "%\n",
         "Species Richness % Change: ", round(change$percentage_change_richness, 2), "%\n",
         "p-value = <0.05")) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1, size = 18),
    text = element_text(size = 18),
    axis.title = element_text(size = 25),
    plot.title = element_text(hjust = 0.5, size = 30),
    plot.caption = element_text(hjust = 1, vjust = 15, size = 20),
    legend.position = "none") #+ ggsave("figures/temperate10_20c.png", width = 20, height = 10, dpi = 300)

change <- temp_data_0_20 %>%
  summarise(
    first_year_sst = mean(sst20[year == min(year)], na.rm = TRUE),
    last_year_sst = mean(sst20[year == max(year)], na.rm = TRUE),
    first_year_richness = mean(msr20[year == min(year)], na.rm = TRUE),
    last_year_richness = mean(msr20[year == max(year)], na.rm = TRUE)) %>%
  mutate(change_sst = last_year_sst - first_year_sst,
         change_richness = last_year_richness - first_year_richness,
         percentage_change_sst = round((change_sst / first_year_sst) * 100, 2),
         percentage_change_richness = round((change_richness / first_year_richness) * 100, 2))

## change in sst  1.51% increase
# change richness -0.14% decreaae # bc of data ditribution 

## 25 - 30 C

# instead of filtering by how many years to show, i want to only show the plots with more than 10 observations
temperate_25 <- all_long %>%
  filter(range == "25-30°C")%>%
  filter(year %% 3 == 0 | year == first_year | year == last_year) 

temperate_25 %>%
  drop_na(sst) %>%
  ggplot(aes(x = sst, y = msr, colour = msr)) + 
  geom_jitter(alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  scale_color_viridis(option = "D") +
  facet_wrap(~year, scales = "free") +  # Facet wrap for selected years
  labs(x = "Mean SST°C", 
       y = "Average Species Richness", 
       title = "Temperate (25-30°C)",
       caption = paste0(
         "SST % Change: ", round(change25$percentage_change_sst, 2), "%\n",
         "Species Richness % Change: ", round(change25$percentage_change_richness, 2), "%\n",
         "p-value = <0.05")) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1, size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 30),
        plot.caption = element_text(hjust = 1, vjust = 10, size = 20),
        legend.position = "none") #+ ggsave("figures/temperate25_30c.png", width = 20, height = 10, dpi = 300)

change25 <- temp_data_25_30 %>%
  summarise(
    first_year_sst = mean(sst25[year == min(year)], na.rm = TRUE),
    last_year_sst = mean(sst25[year == max(year)], na.rm = TRUE),
    first_year_richness = mean(msr25[year == min(year)], na.rm = TRUE),
    last_year_richness = mean(msr25[year == max(year)], na.rm = TRUE)) %>%
  mutate(change_sst = last_year_sst - first_year_sst,
         change_richness = last_year_richness - first_year_richness,
         percentage_change_sst = round((change_sst / first_year_sst) * 100, 2),
         percentage_change_richness = round((change_richness / first_year_richness) * 100, 2))

lm_model25 <- lm(msr25 ~ sst25, data = temp_data_25_30)
summary(lm_model)

# logged abundance code- eh not really working ---- 

# Step 1: Calculate Topt as the midpoint of Tmin and Tmax (or customize as needed)
df2 <- df1 %>%
  mutate(Topt = (temp_min + temp_max) / 2)

# Step 2: Log-transform abundance
df2 <- df2 %>%
  mutate(log_abundance = log(abundance + 1))  # Avoid log(0)

# Step 3: Fit a TPC curve (e.g., Gaussian or quadratic fit)
tpc_fits <- df2 %>%
  group_by(region, genus_species) %>%
  filter(n_distinct(mean_sst) > 2) %>%  # Ensure at least 3 unique points for a quadratic fit
  summarise(fit = list(lm(log_abundance ~ poly(mean_sst, 2))), .groups = "drop")

# Step 2: Extract the fitted TPC curves
tpc_predictions <- tpc_fits %>%
  mutate(predictions = map(fit, ~ {
    data.frame(mean_sst = seq(min(df2$mean_sst, na.rm = TRUE), 
                              max(df2$mean_sst, na.rm = TRUE), length.out = 100),
               fitted_values = predict(.x, 
                                       newdata = data.frame(mean_sst = seq(min(df2$mean_sst, na.rm = TRUE), 
                                                                           max(df2$mean_sst, na.rm = TRUE), length.out = 100))))
  })) %>%
  unnest(predictions)

df2_topt <- df2 %>%
  group_by(region, genus_species) %>%
  summarise(
    t_opt = mean(mean_sst[log_abundance == max(log_abundance, na.rm = TRUE)]),
    .groups = "drop"
  )

# Step 2: Merge Topt with the original dataframe
df2 <- df2 %>%
  left_join(df2_topt, by = c("region", "genus_species"))

# Step 3: Create the plot
df2 %>%
  ggplot(aes(x = mean_sst, y = log_abundance, color = genus_species)) +
  geom_point(alpha = 0.6, size = 3) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linewidth = 1) +  # Quadratic fit
  geom_vline(aes(xintercept = t_opt), linetype = "dashed", color = "orange", linewidth = 1) +  # Topt line
  facet_wrap(~region, scales = "free", ncol = 1) +  # Facet by region and species
  labs(x = "Temperature (°C)", 
       y = "Log Abundance", 
       title = "Abundance vs. Temperature with Topt") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(strip.text = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none")




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