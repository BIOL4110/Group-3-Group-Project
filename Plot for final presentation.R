# Required Libraries
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(patchwork)
library(hexbin)
library(stringr)

# Load your data (update the path as necessary)
data <- read.csv("data-processed/btfbsst_without_NA.csv")

# Panel (a): Map with Hexagonal Binning
# Load the world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Hexagonal binning map
map_plot <- ggplot() +
  geom_sf(data = world, fill = "lightgrey", colour = "white") +
  geom_hex(data = data, aes(x = longitude2, y = latitude2), bins = 50) +
  coord_sf(ylim = c(-0.5,80)) +
  scale_fill_viridis(option = "plasma", name = "n-surveys") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank()) +
  labs(title = "(a) Survey Locations", x = "Longitude", y = "Latitude")

# Panel (b): Boxplot with Scatter
# Filter to top 30 species for clarity
top_species <- data %>%
  count(genus_species, sort = TRUE) %>%
  slice_max(n, n = 30) %>%
  pull(genus_species)

filtered_data <- data %>%
  filter(genus_species %in% top_species)

filtered_data <- filtered_data %>%
  mutate(genus_species = str_replace_all(genus_species, "_", " "))

# Create the violin with scatter
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

ggsave("Figures/Plot-limited.species.png", 
       plot = final_plot, 
       width = 12, 
       height = 8, 
       bg = "transparent", 
       dpi = 300)
