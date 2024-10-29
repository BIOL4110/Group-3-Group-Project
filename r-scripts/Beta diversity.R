library(dplyr)
library(vegan)
library(tidyverse)
library(ggplot2)
library(broom)


bio_time <- read.csv("BioTime_processed.csv")

# Filter data for the two latitude ranges
df_trop <- bio_time %>% 
  subset(!is.na(species)) %>%
  filter(latitude >= 0 & latitude <= 23.5) %>% 
  group_by(genus_species,year) %>% 
  summarise(total_abundance = sum(abundance, na.rm = FALSE)) %>% 
  select(genus_species,year)
 


df_temp <- bio_time %>%
  subset(!is.na(species)) %>% 
  filter(latitude > 23.5 & latitude <= 66.5 & year > 2000 & year < 2011) %>% 
  group_by(genus_species,year) %>% 
  summarise(total_abundance = sum(abundance, na.rm = FALSE)) %>% 
  select(genus_species,year)


# Calculate Jaccard similarity index for each year
jaccard_similarity <- function(year) {
  species_trop <- df_trop %>% filter(year == !!year) %>% pull(genus_species) %>% unique()
  species_temp <- df_temp %>% filter(year == !!year) %>% pull(genus_species) %>% unique()
  
  intersection <- length(intersect(species_trop, species_temp))
  union <- length(union(species_trop, species_temp))
  
  if (union == 0) return(NA) else return(intersection / union)
}

years <- intersect(df_trop$year, df_temp$year)

jaccard_indices <- map_dbl(years, jaccard_similarity)

jaccard_results <- data.frame(year = years, jaccard_index = jaccard_indices)
jaccard_results


#plot the results of jacaard
jaccard_results %>% 
  ggplot(aes(year,jaccard_index)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylab("Jaccard index") + 
  xlab("Year") +
  theme_classic()


jaccard_model <- lm(jaccard_index ~ year, data = jaccard_results)

jaccard_model$coefficients
summary(jaccard_model)$adj.r.squared
summary(jaccard_model)
