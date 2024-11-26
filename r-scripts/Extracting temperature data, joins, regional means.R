#link to sst data download https://psl.noaa.gov/repository/entry/show?entryid=f45cf25c-bde2-44bd-bf3d-c943d92c0dd8


library(tidyverse)
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)
library(ggpmisc)




#DO NOT RUN FROM HERE!!!
#the following takes a while to run so I have written a .csv file of the finished product
#please scroll down to work with figures




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
  filter(year(date) > 1962 & year(date) < 2025)

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

write_csv(sst_obs3,"data-processed/extracted_sst.csv")



#BEGIN HERE TO WORK WITH FIGURES AND JOINS!!!



sst_obs3 <- read_csv("data-processed/extracted_sst.csv")

#Find average summer SSTs in the tropical and temperate regions.
#Looking for trends between 1963 - 2024

mean_sst <- sst_obs3 %>% 
  #creat column identifying regions by latitude
  mutate(region = case_when(latitude >= 0 & latitude <= 23.5 ~ "Tropical",
                            latitude > 23.5 ~ "Temperate")) %>% 
  group_by(year, region) %>% 
  #calculate mean SST by year in tropical region
  summarize(mean_sst = mean(mean_summer_sst_degC, na.rm = TRUE))

trop <- mean_sst %>% 
  filter(region == "Tropical")
temp <- mean_sst %>% 
  filter(region == "Temperate")

lm_trop <- lm(mean_sst ~ year, trop)
summary(lm_trop)

lm_temp <- lm(mean_sst ~ year, temp)
summary(lm_temp)

#Plot yearly mean summer SST
sum_sst <- ggplot(mean_sst, aes(x = year,
                     y = mean_sst,
                     group = region)) +
  geom_point(aes(shape = region, colour = region, size = 5)) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = "lm") +
  geom_text(aes(x = 1990, y = 26, 
                label = "y = 0.013803x + 0.276293, R² = 0.644, p < 0.05"), 
            color = "black", size = 8) +
  geom_text(aes(x = 1990, y = 19, 
                label = "y = 0.020160x - 23.054953, R² = 0.7326, p < 0.05"), 
            color = "black", size = 8) +
  xlab("Year") +
  ylab("Mean SST (°C)") +
  theme_minimal(base_size = 14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 30), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 30),      
        legend.position = "right") +
  guides(size = "none",
         shape = guide_legend(override.aes = list(size = 5))) +
  labs(colour = "Region", shape = "Region")

ggsave("Figures/Mean summer SST.png", 
       plot = sum_sst, bg = "transparent",
       width = 20, height = 10, dpi = 300)



#Join with harmonized btfb set (BioTime - Fishbase)
harm_btfb <- read_csv("data-processed/btfb_without_NA.csv")

harm_btfb %>% 
  mutate(as_date(year)) %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

#Duplicate latitude and longitude to retain original values after rounding
harm_btfb <- harm_btfb %>% 
  mutate(latitude2 = latitude,
         longitude2 = longitude)

#Here we need to round to the nearest 0.5 in latitude and longitude due to the resolution of the termperature dataset
full_harm_rounded <- harm_btfb %>% 
  mutate(latitude = round(latitude*2)/2) %>% 
  mutate(longitude = round(longitude*2)/2)

#We need to convert longitude from a -180 to 180 system into a 0 - 360 system so that it matches the temperature data
full_harm_rounded$longitude <- ifelse(full_harm_rounded$longitude < 0, 
                                      full_harm_rounded$longitude + 360,
                                      full_harm_rounded$longitude)

#join mean summer ssts data set that match btgt based on latitude, longitude, and year
full_harm_btfbsst <- left_join(full_harm_rounded, sst_obs3, 
                               by = c("latitude", "longitude", "year"))

full_harm_btfbsst_clean <- full_harm_btfbsst %>% 
  na.omit()

write_csv(full_harm_btfbsst_clean, "data-processed/btfbsst_without_NA.csv")








#The remaining code is unused for our project.
#We chose to work with the BioTime and Fishbase data.



#Join with harmonized btgt set (BioTime - GlobTherm)
harm_btgt <- read_csv("data-processed/full_harmonized_btgt.csv")

harm_btgt %>% 
  mutate(as_date(year)) %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

#Duplicate latitude and longitude to retain original values after rounding
harm_btgt <- harm_btgt %>% 
  mutate(latitude2 = latitude,
         longitude2 = longitude)

#Here we need to round to the nearest 0.5 in latitude and longitude due to the resolution of the termperature dataset
full_harm_btgt_rounded <- harm_btgt %>% 
  mutate(latitude = round(latitude*2)/2) %>% 
  mutate(longitude = round(longitude*2)/2)

#We need to convert longitude from a -180 to 180 system into a 0 - 360 system so that it matches the temperature data
full_harm_btgt_rounded$longitude <- ifelse(full_harm_btgt_rounded$longitude < 0, 
                                           full_harm_btgt_rounded$longitude + 360,
                                           full_harm_btgt_rounded$longitude)

#join mean summer ssts data set that match btgt based on latitude, longitude, and year
full_harm_btgtsst <- left_join(full_harm_btgt_rounded, sst_obs3, 
                               by = c("latitude", "longitude", "year"))

full_harm_btgtsst_clean <- full_harm_btgtsst %>% 
  na.omit()

write_csv(full_harm_all_data, "data-processed/full_harm_all_data.csv")




#Join with BioTime set
BioTime %>% 
  mutate(as_date(year)) %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

#Duplicate latitude and longitude to retain original values after rounding
#Will be needed later for full harmonization
BioTime <- BioTime %>% 
  mutate(latitude2 = latitude,
         longitude2 = longitude)

#Here we need to round to the nearest 0.5 in latitude and longitude due to the resolution of the termperature dataset
BioTime_rounded <- BioTime %>% 
  mutate(latitude = round(latitude*2)/2) %>% 
  mutate(longitude = round(longitude*2)/2)

print(sapply(BioTime_rounded,class))
print(sapply(sst_obs3,class))

#We need to convert longitude from a -180 to 180 system into a 0 - 360 system so that it matches the temperature data
BioTime_rounded$longitude <- ifelse(BioTime_rounded$longitude < 0, BioTime_rounded$longitude + 360, BioTime_rounded$longitude)

#join mean summer ssts from cleaned sst data set that match BioTime based on latitude, longitude, and year
BioTime_sst <- left_join(BioTime_rounded, sst_obs3, 
                         by = c("latitude", "longitude", "year"))

#retain original latitude and longitude values only and relocate to the previous position
BioTime_sst <- BioTime_sst %>% 
  select(-latitude,-longitude, -genus_species) %>% 
  rename(latitude = latitude2,
         longitude = longitude2) %>% 
  select(abundance, year, latitude, longitude, genus, species, region, mean_summer_sst_degC)


#See dataset without NA values
BioTime_sst_clean <- BioTime_sst %>% 
  na.omit()


write_csv(BioTime_sst,"data-processed/BioTime summer sst with NAs.csv")
write_csv(BioTime_sst_clean, "data-processed/BioTime summer sst without NAs.csv")



