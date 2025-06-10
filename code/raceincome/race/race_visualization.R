###################################################################
# Purpose: initial visualizations of race-income gridded eif data
# Author: Josie Fischman
# Date created: 6/3/2025
####################################################################
# Inputs:raceincome_wide
# Outputs: Black Residents in Charlottesville.png, Percentage of Black Population in Charlottesville.png
###################################################################

#change into spatial data
raceincome_sf <- st_as_sf(
  raceincome_wide,
  coords = c("grid_lon", "grid_lat"),
  crs = 4326  # WGS 84 (standard GPS lat/lon)
)
#filter year 2023 
raceincome_2023 <- raceincome_sf %>%
  select(geometry, matches("2023$"))
#filter for "black
raceincome_2023 <- raceincome_2023 %>%
  mutate(
    total_black = rowSums(across(contains("black"), ~as.numeric(.x)), na.rm = TRUE)
  )

#Count of Black Residents in Charlottesville
v <- ggplot(raceincome_2023) +
  geom_sf(aes(color = total_black)) +
  scale_color_viridis_c(option = "plasma", na.value = "transparent") +
  labs(title = "Black Residents in Charlottesville",
       fill = "Noise Count") +
  theme_minimal()

ggsave("output/raceincome/race/Black Residents in Charlottesville.png", plot = v, width = 6, height = 4)



#convert perc black into SF
raceincome_2023 <- raceincome_2023 %>%
  mutate(
    total_pop = rowSums(across(ends_with("2023"), as.numeric), na.rm = TRUE),
    perc_black = 100 * total_black / total_pop
  )
#Graph of Percentage of Black Population
w <- ggplot(raceincome_2023) +
  # Fill grid points by % Black
  geom_sf(aes(color = perc_black)) +
  
  scale_color_viridis_c(name = "% Black", option = "magma", direction = -1, na.value = "transparent") +
  
  labs(
    title = "Percentage of Black Population in Charlottesville/Albemarle",
    fill = "% Black"
  ) +
  theme_minimal()

ggsave("output/raceincome/race/Percentage of Black Population in Charlottesville.png", plot = w, width = 6, height = 4)



