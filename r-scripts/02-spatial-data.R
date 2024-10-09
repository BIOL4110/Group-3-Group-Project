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

plot(sst_raster)
