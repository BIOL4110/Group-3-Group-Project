## i am now going to attempt spatial data
# oct - 08 - 2024

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

#load in data
raster2 <- raster("data-raw/sst.nc")

print(raster2)
rotated_raster <- rotate(raster2)

plot(rotated_raster, main = "Map of Global Sea Surface Temperatures from NOAA", 
     xlab = "Longitude", ylab = "Latitude",
     col = viridis(100))

map("world", add = TRUE, fill = TRUE, col = "black")


# Filter for tropical latitudes (23.5°N to 23.5°S)
library(viridis)
library(maps)
#rotated to change from 0-360 to -180-180


grey_raster <- rotated_raster
grey_raster[] <- NA

tropical_extent <- extent(-180, 180, -23.5, 23.5)
mask <- rotated_raster >= -Inf & rotated_raster <= Inf

sst_tropical <- mask(rotated_raster, tropical_extent)

plot(grey_raster, col = "grey", 
     main = "Map of SSTs - Tropical Latitudes", 
     xlab = "Longitude", ylab = "Latitude", 
     xlim = c(-180, 180), 
     ylim = c(-50, 50))


## properly crop the latitude!
plot(sst_tropical2, main = "Map of SSTs - Tropical Latitudes",
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

