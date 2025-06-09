###################################################################
# Purpose: initial visualizations of race-income gridded eif data
# Author: Josie Fischman
# Date: 6/3/2025
# Inputs:
# Outputs: 
###################################################################
library(ggplot2)
library(RColorBrewer)

library(sf)
#change into spatial data
merged2_sf <- st_as_sf(
  merged2,
  coords = c("pm25_grid_x", "pm25_grid_y"),
  crs = 4326  # WGS 84 (standard GPS lat/lon)
)
#convert county filtered into a shape file
county_sf <- st_as_sf(
  county_filtered,
  coords = c("pm25_grid_x", "pm25_grid_y"),
  crs = 4326  # WGS 84 (standard GPS lat/lon)
)

#map of people in 10th decile
merged2_sf$decile10_bin <- cut(
  merged2_sf$n_noise_postprocessed_10,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),  # Covering all possible values
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)


ggplot(merged2_sf) +
  geom_sf(aes(color = decile10_bin), size = 2) +
  scale_color_brewer(palette = "YlOrRd", na.value = "grey90") +
  labs(
    title = "Binned decile 10 Resident Counts",
    fill = "Count Bin"
  ) +
  theme_minimal()

#1st decile map

merged2_sf$decile1_bin <- cut(
  merged2_sf$n_noise_postprocessed_1,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),  # Covering all possible values
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)


ggplot(merged2_sf) +
  geom_sf(aes(color = decile1_bin), size = 2) +
  scale_color_brewer(palette = "YlOrRd", na.value = "grey90") +
  labs(
    title = "Binned decile 1 Resident Counts",
    fill = "Count Bin"
  ) +
  theme_minimal()
# decile 1-3
merged2_sf$decile_0_3_total <- 
  coalesce(merged2_sf$n_noise_postprocessed_0, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_1, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_2, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_3, 0)

# Step 2: Bin the summed values
merged2_sf$decile_0_3_bin <- cut(
  merged2_sf$decile_0_3_total,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)

# Step 3: Plot the binned totals
ggplot(merged2_sf) +
  geom_sf(aes(color = decile_0_3_bin), size = 2) +
  scale_color_brewer(palette = "YlOrRd", na.value = "grey90") +
  labs(
    title = "Binned Resident Counts in Deciles 0–3",
    color = "Count Bin"
  ) +
  theme_minimal()

#bin 8-10
merged2_sf$decile_8_10_total <- 
  coalesce(merged2_sf$n_noise_postprocessed_8, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_9, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_10, 0) 

# Step 2: Bin the summed values
merged2_sf$decile_8_10_bin <- cut(
  merged2_sf$decile_8_10_total,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)

# Step 3: Plot the binned totals
ggplot(merged2_sf) +
  geom_sf(aes(color = decile_8_10_bin), size = 2) +
  scale_color_brewer(palette = "YlOrRd", na.value = "grey90") +
  labs(
    title = "Binned Resident Counts in Deciles 8–10",
    color = "Count Bin"
  ) +
  theme_minimal()

# Find places with the highest percentages of low income
#calculate total
merged2_sf$decile_total <- 
  coalesce(merged2_sf$n_noise_postprocessed_0, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_1, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_2, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_3, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_4, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_5, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_6, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_7, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_8, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_9, 0) +
  coalesce(merged2_sf$n_noise_postprocessed_10, 0) 

#calculate percentage
merged2_sf$percent_0_3 <- with(
  merged2_sf,
  100 * decile_0_3_total / decile_total
)

#graph percentage
ggplot(merged2_sf) +
  geom_sf(aes(color = percent_0_3), size = 2.5) +
  scale_color_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "grey90",
    name = "% in 0–3 Decile"
  ) +
  labs(title = "Percent of Residents in 0–3 Deciles") +
  theme_minimal()



