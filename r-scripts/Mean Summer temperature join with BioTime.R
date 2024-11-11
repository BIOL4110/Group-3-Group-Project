#link to sst data download https://psl.noaa.gov/repository/entry/show?entryid=f45cf25c-bde2-44bd-bf3d-c943d92c0dd8


library(tidyverse)
library(dplyr)
library(lubridate)
library(ncdf4)

BioTime <- read_csv("data-processed/BioTime_processed.csv")


#following this article to see what I can achieve 
#https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd

temp_nc <- nc_open("data-raw/sst.mon.mean.nc")

#exploring some aspects of the .nc file
print(temp_nc)
attributes(temp_nc$var)
attributes(temp_nc$dim)


#extracting variables from the .nc file
lat <- ncvar_get(temp_nc, "lat")

lon <- ncvar_get(temp_nc, "lon")

time <- ncvar_get(temp_nc, "time")
tunits <- ncatt_get(temp_nc, "time", "units")
#We use this one because our units are in "Days since ..."
time_obs <- as.Date(time, origin = "1891-01-01", tz = "GMT") 
#This code would be for units in "Seconds since ..."
#time_obs <- as.POSIXct(time, origin = "1891-01-01", tz = "GMT")
range(time_obs)

sst_array <- ncvar_get(temp_nc, "sst")
fillvalue <- ncatt_get(temp_nc,"sst", "parent_stat")

sst_array[sst_array == fillvalue$value] <- NA
sst_array


lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))
head(lonlattime)

sst_vec_long <- as.vector(sst_array)

#create data.frame
sst_obs <- data.frame(cbind(lonlattime, sst_vec_long))
head(sst_obs)
#rename columns
colnames(sst_obs) <- c("longitude", "latitude", "date","monthly_sst_deg_C")
head(sst_obs)


#filter the data set down to the years we are working with
sst_obs <- sst_obs %>% 
  filter(year(date) > 1962 & year(date) < 2015)

#reclass columns from "character" class
sst_obs <- sst_obs %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         monthly_sst_deg_C = as.numeric(monthly_sst_deg_C))

print(sapply(sst_obs,class))

#filter to the area and season
sst_obs2 <- sst_obs %>% 
  #this line filters to latitudes that we are concerned about
  filter(latitude >= 0 & latitude <= 66.5) %>% 
  #this line filters to the summer months (June - August inclusive)
  filter(month(date) > 05 & month(date) < 09)


#calculate a mean summer sst for each long/lat in a given year
sst_obs3 <- sst_obs2 %>% 
  group_by(latitude, longitude, year(date)) %>% 
  summarize(mean_summer_sst_degC = mean(monthly_sst_deg_C, na.rm = TRUE)) %>% 
  ungroup() %>% 
  na.omit()

#rename columns
colnames(sst_obs3) <- c("latitude", "longitude", "year","mean_summer_sst_degC")
head(sst_obs3)         




#OK now i need to work with the BioTime data to prepare for a join
BioTime %>% 
  mutate(as_date(year)) %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

#Here we need to round to the nearest 0.5 in latitude and longitude due to the resolution of the termperature dataset
BioTime_rounded <- BioTime %>% 
  mutate(latitude = round(latitude*2)/2) %>% 
  mutate(longitude = round(longitude*2)/2)

print(sapply(BioTime_rounded,class))
print(sapply(sst_obs3,class))

#We need to convert longitude from a -180 to 180 system into a 0 - 360 system so that it matches the temperature data
BioTime_rounded$longitude <- ifelse(BioTime_rounded$longitude < 0, BioTime_rounded$longitude + 360, BioTime_rounded$longitude)

#join mean summer ssts from cleaned sst data set that match BioTime based on latitude, longitude, and year
BioTime_sst <- left_join(BioTime_rounded, sst_obs3, by = c("latitude", "longitude", "year"))


#See dataset without NA values
BioTime_sst_clean <- BioTime_sst %>% 
  na.omit()


write_csv(BioTime_sst,"data-processed/BioTime summer sst with NAs.csv")
write_csv(BioTime_sst_clean, "data-processed/BioTime summer sst without NAs.csv")
