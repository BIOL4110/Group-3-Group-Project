## i am now going to attempt spatial data

#packages
library(dplyr)
library(tidyr)
library(gstat)
library(rgeos)
library(scales)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
library(ggplot2)
options(stringsAsFactors = FALSE)

sst_raster <- raster("data-raw/oisst-data.nc")

print(sst_raster)

plot(sst_raster, main = "Map of Global Sea Surface Temperatures from NOAA", 
     xlab = "longitude?",
     ylab = "latitude?")

# Filter for tropical latitudes (23.5°N to 23.5°S)
tropical_extent <- extent(-180, 180, -23.5, 23.5)
sst_tropical <- crop(sst_raster, tropical_extent)
library(viridis)
library(maps)

# Plot for tropical latitudes - Most recent 2024/10
plot(sst_tropical, main = "Map of SSTs - Tropical Latitudes",
     xlab = "Longitude", ylab = "Latitude",
     col = viridis(100))

map("world", add = TRUE, fill = TRUE, col = "black")

# Filter for temperate latitudes in the Northern Hemisphere (23.5°N to 66.5°N)
temperate_extent_north <- extent(-180, 180, 23.5, 66.5)

# Crop the SST raster to the temperate extent
sst_temperate_north <- crop(sst_raster, temperate_extent_north)

# Rotate the cropped raster to shift longitudes from -180,180 to 0,360
sst_temperate_north_rotated <- rotate(sst_temperate_north)

# Plot the rotated SST data with viridis colors
plot(sst_temperate_north_rotated, main = "Map of SSTs - Temperate Latitudes",
     xlab = "Longitude", ylab = "Latitude", col = viridis(100))


## TURNING SST DATAFRAME RECTANGULAR - how do i get the data for all the years
library(lattice)
library(RColorBrewer)

raster2 <- raster("data-raw/sst.nc")

print(raster2)

plot(raster2, main = "Map of Global Sea Surface Temperatures from NOAA", 
     xlab = "longitude?",
     ylab = "latitude?")
