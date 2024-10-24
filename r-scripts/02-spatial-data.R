## i am now going to attempt spatial data
# oct - 08 - 2024

#packages
library(dplyr)
library(tidyr)
library(gstat)
#library(rgeos)
library(scales)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
library(ggplot2)
options(stringsAsFactors = FALSE)

library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(sf)

#load in data
raster2 <- raster("data-raw/sst.nc")

print(raster2)
rotated_raster <- rotate(raster2)

#convert to data frame to allow it to be plotted in ggplot
df_sst_rast <- as.data.frame(rotated_raster, xy = TRUE)

# Load the countries data
countries <- ne_countries(scale = "medium", returnclass = "sf")

# Load the observation data
bio_time <- read.csv("data-processed/BioTime_processed.csv")

# Set the location data to plot on ggplot as spatial data
observation_coordinates <- st_as_sf(bio_time,coords=c("longitude","latitude"),crs=4326)

# plot in ggplot

ggplot() +
  geom_raster(data = df_sst_rast , aes(x = x, y = y, fill = Weekly.Mean.of.Sea.Surface.Temperature)) + 
  coord_sf(crs = crs(rotated_raster)) +
  geom_sf(data = countries) +
  geom_sf(data = observation_coordinates, colour = "red", size = 0.5)










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

#help from chatgpt making the Map
library(raster)
library(viridis)
library(maps)
library(ggplot2)
library(gridExtra)
library(sp)

# Load the raster data
raster2 <- raster("data-raw/sst.nc")

# Rotate the raster
rotated_raster <- rotate(raster2)

# Define the polygons for tropical and temperate regions
tropical_poly <- Polygon(cbind(c(-180, -180, 180, 180), c(-23.5, 23.5, 23.5, -23.5)))
tropical_polys <- Polygons(list(tropical_poly), ID = "tropical")
tropical_sp <- SpatialPolygons(list(tropical_polys))

temperate_poly <- Polygon(cbind(c(-180, -180, 180, 180), c(23.5, 66.5, 66.5, 23.5)))
temperate_polys <- Polygons(list(temperate_poly), ID = "temperate")
temperate_sp <- SpatialPolygons(list(temperate_polys))

# Convert the SpatialPolygons to SpatialPolygonsDataFrame
tropical_spdf <- SpatialPolygonsDataFrame(tropical_sp, data.frame(id = "Tropical", row.names = "tropical"))
temperate_spdf <- SpatialPolygonsDataFrame(temperate_sp, data.frame(id = "Temperate", row.names = "temperate"))

# Mask the raster with the polygons
tropical_masked <- mask(rotated_raster, tropical_spdf)
temperate_masked <- mask(rotated_raster, temperate_spdf)

palette <- viridis(100)

x_limits <- c(-180, 180)

plot(rotated_raster, main = "Map of Global Sea Surface Temperatures from NOAA", 
     xlab = "Longitude", ylab = "Latitude", col = viridis(100), axes = TRUE, 
     xlim = x_limits)

raster <- as.data.frame(rotated_raster, xy = T, na.rm = T)

# Add the world map
map("world", add = TRUE, fill = TRUE, col = "white")

# Draw red border around the tropical region ## switch .5 of t
rect(-180, 0, 180, 22, border = "red", lwd = 4)

# Draw blue border around the temperate region
rect(-180, 23.5, 180, 66.5, border = "blue", lwd = 4)

raster3 <- raster("data-raw/sst.ltm.1991-2020.nc")
sst3 <- as.data.frame(raster3, xy = T, na.rm = T) ## how to add in years

## added the years but is the data actually correct?
years <- rep(1991:2020, each = 12)

sst3$Year <- years[1:nrow(sst3)]

sst3$Month <- rep(1:12, times = length(1991:2020))

##need to add a legend OUTSIDE the plot - not working
 ## use link when it time
# https://tmieno2.github.io/R-as-GIS-for-Economists/geom-raster.html
# where data is downloaded from 
# https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.html sst.wakeman.1990-present
