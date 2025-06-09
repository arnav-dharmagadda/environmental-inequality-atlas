###################################################################
# Purpose: initial visualizations of race-income gridded eif data
# Author: Josie Fischman
# Date: 6/3/2025
# Inputs:
# Outputs: 
###################################################################
install.packages("ggspatial")
install.packages("rosm")   # for map tiles

library(ggplot2)
library(sf)
library(ggspatial)
library(rosm)
library(ggplot2)

library(RColorBrewer)
library(tmap)
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
# Step 1: Convert to sf POINT object
merged2_sf <- st_as_sf(merged2_sf, coords = c("grid_lon", "grid_lat"), crs = 4326)
# Step 2: Get expanded bounding box to fully contain all grid cells
bbox <- st_bbox(merged2_sf)
bbox_expanded <- bbox
bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - 0.005
bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + 0.005
# Step 3: Create grid polygons at 0.01° resolution covering the area
grid_polygons <- st_make_grid(
  cellsize = c(0.01, 0.01),
  offset = c(bbox_expanded["xmin"], bbox_expanded["ymin"]),
  n = c(ceiling((bbox_expanded["xmax"] - bbox_expanded["xmin"]) / 0.01),
        ceiling((bbox_expanded["ymax"] - bbox_expanded["ymin"]) / 0.01)),
  crs = 4326,
  what = "polygons"
)
# Step 4: Convert grid to sf and join with your original data (spatial match)
grid_sf <- st_sf(geometry = grid_polygons)
# Step 5: Spatial join: match points to polygons (assumes 1 point per cell)
merged2_sf <- st_join(grid_sf, merged2_sf, join = st_contains)
#map of people in 10th decile
merged2_sf$decile10_bin <- cut(
  merged2_sf$n_noise_postprocessed_10,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),  # Covering all possible values
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)


ggplot(merged2_sf) +
  geom_sf(aes(fill = decile10_bin), color="white", size = 2) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey90") +
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
  geom_sf(aes(fill = decile1_bin), color="white", size = 2) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey90") +
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

#share of people in bottom three deciles
merged2_sf <- merged2_sf %>%
  mutate(
    across(starts_with("n_noise_"), as.numeric),  # Convert to numeric
    total_low = rowSums(across(c(n_noise_0, n_noise_1, n_noise_2, n_noise_3)), na.rm = TRUE),
    total_all = rowSums(across(matches("^n_noise_\\d$|^n_noise_10$")), na.rm = TRUE),
    share_low = ifelse(total_all > 0, total_low / total_all, NA)
  )
merged2_sf <- st_transform(merged2_sf, 4326)

ggplot(merged2_sf) +
  geom_sf(aes(fill = share_low), color = "white", size = 2) +
  scale_fill_viridis_c(
    name = "Share in Bottom 3 Deciles",
    limits = c(0, 1),
    na.value = "white"
  ) +
  labs(
    title = "Share of Residents in Bottom 3 Income Deciles",
    fill = "Proportion"
  ) +
  theme_minimal()

