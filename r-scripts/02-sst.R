#02-sst.r

# ADD SEA SURFACE TEMEPRATURES - 

#stuart addd code here  ----

# SEA SURFACE TEMPERATURES RISING OVER THE YEARS - done ----
## only summer months
sst_trend_model <- lm(mean_sst ~ year, data = mean_sst)
summary(sst_trend_model)

# Extract the slope (coefficient of year)
slope <- coef(sst_trend_model)["year"]
cat("The slope of the trend line is:", slope, "°C per year\n")
#positive slope of 0.01C per year - temp increasing that much every year 

#stuarts data
sst_obs3 <- read_csv("data-processed/extracted_sst.csv")

mean_sst <- sst_obs3 %>% 
  #creat column identifying regions by latitude
  mutate(region = case_when(latitude >= 0 & latitude <= 23.5 ~ "Tropical",
                            latitude > 23.5 ~ "Temperate")) %>% 
  group_by(year, region) %>% 
  #calculate mean SST by year in tropical region
  summarize(mean_sst = mean(mean_summer_sst_degC, na.rm = TRUE))

ggplot(mean_sst, aes(x = year, y = mean_sst, group = region)) +
  geom_point(aes(shape = region, colour = region), size = 5) + 
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 3) + 
  facet_wrap(~region, scales = "free_y") + 
  geom_text(data = subset(mean_sst, region == "Tropical"), 
            aes(x = 1990, y = max(mean_sst) + 0.25, 
                label = "y = 0.013803x + 0.276293, R² = 0.644, p < 0.05"), 
            color = "black", size = 6, inherit.aes = FALSE) +
  geom_text(data = subset(mean_sst, region == "Temperate"), 
            aes(x = 1990, y = max(mean_sst) + 0.25, 
                label = "y = 0.020160x - 23.054953, R² = 0.7326, p < 0.05"), 
            color = "black", size = 6, inherit.aes = FALSE) +
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
  labs(colour = "Region", shape = "Region") + ggsave("figures/mean_sst_overtime.png", width = 20, height = 10, dpi = 300)



