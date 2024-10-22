#THIS IS NOT WORKING YET

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(sf)
library(dplyr)

# Load the countries data
countries <- ne_countries(scale = "medium", returnclass = "sf")

bio_time <- read.csv("data-processed/BioTime_processed.csv")

observation_coordinates <- st_as_sf(bio_time,coords=c("longitude","latitude"),crs=4326)

ggplot()+
  geom_sf(data=countries)+
  geom_sf(data=observation_coordinates)+
  theme(legend.position="none",
        panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank())
