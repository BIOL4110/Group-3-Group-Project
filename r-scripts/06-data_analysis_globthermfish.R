##06 - data-analysis of Globtherm data - Celeste and Mya

## Load packages

library(taxadb) # Creates a local database of many commonly used taxonomic authorities and provides functions that can quickly query this data
library(dplyr)
library(tidyverse)
library(parallel) # Support for parallel computation, including by forking and random-number generation.
library(furrr) # combine purrr's family of mapping functions with future's parallel processing capabilities.
library(taxize) # allows users to search over many taxonomic data sources for species names.
library(stringr)

library(janitor)
library(readr)
library(stringr)
library(traitdata)
library(ggplot2)

#Load databases
globtherm_analysis <- read_csv("data-processed/harmonized_globtherm2.csv")
colnames(globtherm_analysis)
head(globtherm_analysis)

globtherm_analysis %>% select(class) %>% print(n=50)
unique_class <- globtherm_analysis %>% distinct(class)
print(unique_class)

unique_order <- globtherm_analysis %>% distinct(order)
print(unique_order) %>% print(n=45)

filtered_globtherm <- globtherm_analysis %>% filter(class == "Actinopterygii")
print(filtered_globtherm) %>% print(n=39) %>% View

globtherm_fish <- filtered_globtherm
View(globtherm_fish)

### Write csv for new globtherm df with only class: Actinopterygii
write_csv(globtherm_fish, "data-processed/globtherm_fish.csv")

## Plot the data!!!

globtherm_fish %>% ggplot(aes(tmax, th_genus_species, color = region)) + geom_jitter(aes()) +
  scale_color_manual(values = c("Temperate" = "blue", "Tropical" = "green"))
