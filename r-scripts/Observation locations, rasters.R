library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(tidyr)
library(gstat)
library(scales)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(terra)

# Load the data
countries <- ne_countries(scale = "medium", returnclass = "sf")

locations <- read.csv("data-processed/btfbsst_without_NA.csv")

observation_coordinates <- st_as_sf(locations,coords=c("longitude2","latitude2"),crs=4326)

#plot locations on a blank background
ggplot()+
  geom_sf(data=countries)+
  geom_sf(data=observation_coordinates)+
  coord_sf(ylim = c(-0.5,90),
           expand = FALSE) +
  theme(legend.position="none",
        panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size = 8),
        axis.ticks=element_line(colour = "black"),
        panel.background=element_blank())




#RASTER FOR MIDSUMMER (JULY) 1963

#load in data
raster1 <- raster("data-raw/sst.mon.mean.nc_subset (Jul. 1963).nc")

print(raster1)
rotated_raster1 <- rotate(raster1)

#convert to data frame to allow it to be plotted in ggplot
df_sst_rast1 <- as.data.frame(rotated_raster1, xy = TRUE)
head(df_sst_rast1)

ggplot()+
  geom_raster(data = df_sst_rast1, 
              aes(x = x, y = y,
                  fill = Monthly.Means.of.Global.Sea.Surface.Temperature)) +
  coord_sf(crs = crs(rotated_raster1)) +
  geom_sf(data=countries)+
  geom_sf(data=observation_coordinates, colour = "red", size = 0.5)+
  coord_sf(ylim = c(-0.5,90),
           expand = FALSE) +
  labs(fill = "Mean SST (°C) July 1963") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_blank())




#RASTER FOR MIDSUMMER (JULY) 2014

#load in data
raster2 <- raster("data-raw/sst.mon.mean.nc_subset (Jul. 2014).nc")

print(raster2)
rotated_raster2 <- rotate(raster2)

#convert to data frame to allow it to be plotted in ggplot
df_sst_rast2 <- as.data.frame(rotated_raster2, xy = TRUE)
head(df_sst_rast2)

ggplot()+
  geom_raster(data = df_sst_rast2, 
              aes(x = x, y = y,
                  fill = Monthly.Means.of.Global.Sea.Surface.Temperature)) +
  coord_sf(crs = crs(rotated_raster2)) +
  geom_sf(data=countries)+
  geom_sf(data=observation_coordinates, colour = "red", size = 0.5)+
  coord_sf(ylim = c(-0.5,90),
           expand = FALSE) +
  labs(fill = "Mean SST (°C) July 2014") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_blank())




#RASTER FOR MIDSUMMER (JULY) 2024

#load in data
raster3 <- raster("data-raw/sst.mon.mean.nc_subset (Jul. 2024).nc")

print(raster3)
rotated_raster3 <- rotate(raster3)

#convert to data frame to allow it to be plotted in ggplot
df_sst_rast3 <- as.data.frame(rotated_raster3, xy = TRUE)
head(df_sst_rast3)

ggplot()+
  geom_raster(data = df_sst_rast3, 
              aes(x = x, y = y,
                  fill = Monthly.Means.of.Global.Sea.Surface.Temperature)) +
  coord_sf(crs = crs(rotated_raster3)) +
  geom_sf(data=countries)+
  geom_sf(data=observation_coordinates, colour = "red", size = 0.5)+
  coord_sf(ylim = c(-0.5,90),
           expand = FALSE) +
  labs(fill = "Mean SST (°C) July 2024") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_blank())
