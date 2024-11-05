library(terra)
library(sf)
library(tidyverse)
library(lubridate)
library(anytime)


#load weekely mean temperature raster data 
temp  <- terra::rast("data-raw/sst.nc")
class(temp)

#load the sf object with the points  
pts <- read_sf("data-processed/BioTime_processed.csv")
pts <- pts %>% mutate(latitude = as.numeric(latitude),
                      longitude = as.numeric(longitude))

# Add a new column to the point dataset to add the mean temperature 
pts <- pts %>% mutate("annual_mean_sst_anomaly" = NA)

# This loop will go through each row in the point data and calculates mean temperature 
for (i in seq(1: nrow(pts))){
  
  # select weekly temperatures that fall within the year of the row
  y_temp_rasters <- temp[[year(time(temp)) == pts$year[i]]]
  
  #convert the locs to a spatraster object
  locs <- vect(pts[i,], geom = c("latitude", "longitude"))
  
  # extract the temperature from all weekly rasters at the location that corresponds to the row  
  annual_temp_value <- terra::extract(y_temp_rasters,locs)
  
  # add the mean temp to the dataframe
  pts$annual_mean_sst_anomaly[i] <- mean(as.numeric(annual_temp_value[1,2:length(annual_temp_value)]))
}

#writing .csv data file
pts %>% 
  write.csv("data-processed/BioTime_with_sst.csv")


#file source: https://psl.noaa.gov/mddb2/makePlot.html?variableID=156646&fileID=1463649