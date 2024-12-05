#02-sst.r
# IN / SB

library(tidyverse)
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)
library(ggpmisc)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(patchwork)
library(hexbin)
library(stringr)

#following this article to see what I can achieve in temperature extraction
#https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd

#this file can be found in our OSF project
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

#No longer need to retain rounded location data. Clear and keep original location data associated with BioTime
full_harm_btfbsst_clean <- full_harm_btfbsst_clean %>% 
  select(-c(longitude,latitude)) %>% 
  rename(latitude = latitude2,
         longitude = longitude2)

#write_csv(full_harm_btfbsst_clean, "data-processed/btfbsst_without_NA.csv")

# PLOTTING SEA SURFACE TEMPERATURES RISING OVER THE YEARS  ----
## only summer months
sst_trend_model <- lm(mean_sst ~ year, data = mean_sst)
summary(sst_trend_model)

# Extract the slope (coefficient of year)
slope <- coef(sst_trend_model)["year"]
cat("The slope of the trend line is:", slope, "°C per year\n")
#positive slope of 0.01C per year - temp increasing that much every year 

sst_obs3 <- read_csv("data-processed/extracted_sst.csv")

mean_sst <- sst_obs3 %>% 
  #creat column identifying regions by latitude
  mutate(region = case_when(latitude >= 0 & latitude <= 23.5 ~ "Tropical",
                            latitude > 23.5 ~ "Temperate")) %>% 
  group_by(year, region) %>% 
  #calculate mean SST by year in tropical region
  summarize(mean_sst = mean(mean_summer_sst_degC, na.rm = TRUE))

# redo linear analysis 

ggplot(mean_sst, aes(x = year, y = mean_sst, group = region)) +
  geom_point(aes(shape = region, colour = region), size = 5) + 
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 3) + 
  facet_wrap(~factor(region, levels = c("Tropical", "Temperate")), scales = "free_y") +  geom_text(data = subset(mean_sst, region == "Tropical"), 
            aes(x = 1990, y = max(mean_sst) + 0.25, 
                label = "y = 0.013803x + 0.276293, R² = 0.644, p < 0.05"), 
            color = "black", size = 6, inherit.aes = FALSE) +
  geom_text(data = subset(mean_sst, region == "Temperate"), 
            aes(x = 1990, y = max(mean_sst) + 0.25, 
                label = "y = 0.020160x - 23.054953, R² = 0.7326, p < 0.05"), 
            color = "black", size = 6, inherit.aes = FALSE) +
  annotate("text", x = 1985, y = max(mean_sst) + 1, label = "a", 
           size = 10, fontface = "bold") +
  xlab("Year") +
  ylab("Mean SST (°C)") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 30), 
        axis.text = element_text(size = 25), 
        axis.title = element_text(size = 30),      
        legend.position = "none") +
  guides(size = "none", shape = guide_legend(override.aes = list(size = 5))) +
  labs(colour = "Region", shape = "Region") #+ ggsave("figures/mean_sst_overtime.png", width = 20, height = 10, dpi = 300)

# Panel (a): Map with Hexagonal Binning ----
# Load the world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Hexagonal binning map
map_plot <- ggplot() +
  geom_sf(data = world, fill = "lightgrey", colour = "white") +
  geom_hex(data = full_harm_btfbsst_clean, aes(x = longitude, y = latitude), bins = 50) +
  coord_sf(ylim = c(-0.5,80)) +
  scale_fill_viridis(option = "plasma", name = "n-surveys") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank()) +
  labs(title = "(a) Survey Locations", x = "Longitude", y = "Latitude")

# Panel (b): Violin plot

filtered_data <- full_harm_btfbsst_clean %>%
  mutate(genus_species = str_replace_all(genus_species, "_", " "))

# Create the violin plot

violin_plot <- ggplot(filtered_data, aes(x = reorder(genus_species, mean_summer_sst_degC, FUN = median),
                                         y = mean_summer_sst_degC)) +
  geom_violin(fill = "lightgrey", colour = "black", width = 0.8) +  # Adjust width for compactness
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Rotate labels vertically
    panel.grid.major.x = element_blank(),  # Remove vertical gridlines
    panel.grid.minor = element_blank(),    # Remove minor gridlines
    panel.grid.major.y = element_line(colour = "grey90"),  # Keep horizontal gridlines
    panel.background = element_blank(),
    plot.background = element_blank())+
  labs(title = "(b) Temperature Distribution by Species",
       x = NULL,  # Remove x-axis label
       y = "Mean Summer SST (°C)") +
  scale_y_continuous(limits = c(10, 30), breaks = seq(10, 30, by = 5))

# Combine the plots
final_plot <- map_plot / violin_plot +
  plot_layout(heights = c(2,1))

# Display the final plot
print(final_plot)

ggsave("Figures/Plot.species.png", 
       plot = final_plot, 
       width = 12, 
       height = 8, 
       bg = "transparent", 
       dpi = 300)

