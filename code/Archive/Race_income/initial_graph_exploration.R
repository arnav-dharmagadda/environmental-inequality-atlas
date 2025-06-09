###################################################################
# Purpose: initial visualizations of race-income gridded eif data
# Author: Josie Fischman
# Date: 6/3/2025
# Inputs:
# Outputs: 
###################################################################
library(ggplot2)

library(sf)
#change into spatial data
merged_sf <- st_as_sf(
  merged,
  coords = c("pm25_grid_x", "pm25_grid_y"),
  crs = 4326  # WGS 84 (standard GPS lat/lon)
)

#chat gpt map
ggplot(merged_sf) +
  geom_sf(aes(fill = n_noise_postprocessed_Black)) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  labs(title = "Black Residents in Charlottesville",
       fill = "Noise Count") +
  theme_minimal()


#convert county filtered into a shape file
county_sf <- st_as_sf(
  county_filtered,
  coords = c("pm25_grid_x", "pm25_grid_y"),
  crs = 4326  # WGS 84 (standard GPS lat/lon)
)

#convert perc black into SF
merged <- merged %>%
  mutate(
    total_postprocessed = rowSums(select(., starts_with("n_noise_postprocessed_")), na.rm = TRUE),
    perc_black = 100 * n_noise_postprocessed_Black / total_postprocessed
  )
merged_sf <- st_as_sf(merged, coords = c("pm25_grid_x", "pm25_grid_y"), crs = 4326)

ggplot() +
  # Fill grid points by % Black
  geom_sf(data = merged_sf, aes(fill = perc_black), color = NA) +
  
  # Outline for Charlottesville and Albemarle
  geom_sf(data = county_sf, fill = NA, color = "black", size = 1) +
  
  scale_fill_viridis_c(name = "% Black", option = "magma", direction = -1, na.value = "transparent") +
  
  labs(
    title = "Percentage of Black Population in Charlottesville/Albemarle",
    fill = "% Black"
  ) +
  theme_minimal()


