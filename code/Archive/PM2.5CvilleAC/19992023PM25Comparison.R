# Load required libraries
library(arrow)
library(dplyr)
library(sf)
library(ggplot2)
#install.packages("patchwork")
library(patchwork)

# Set working directory
setwd("/Users/elizabethshiker/Dropbox/environmental-inequality-atlas/")

# Load CBSA gridpoints
load("data/gridpoints_with_county_2020.rda")
county <- df

# Filter for Charlottesville and Albemarle County
county_filtered <- county %>%
  filter(STATEFP == "51", COUNTYFP %in% c("540", "003"))

# Load PM2.5 data
pollution_2023 <- read_parquet("data/eif_pollutants/gridded_eif_pollutants_2023.parquet")
pollution_1999 <- read_parquet("data/eif_pollutants/gridded_eif_pollutants_1999.parquet")

# Merge with county-filtered grid
merged_2023 <- county_filtered %>%
  inner_join(pollution_2023, by = c("pm25_grid_x" = "grid_lon", "pm25_grid_y" = "grid_lat"))

merged_1999 <- county_filtered %>%
  inner_join(pollution_1999, by = c("pm25_grid_x" = "grid_lon", "pm25_grid_y" = "grid_lat"))

# Compute actual min/max from both years in these counties
actual_range <- range(
  c(merged_2023$pm25_ACAG_gwr, merged_1999$pm25_ACAG_gwr),
  na.rm = TRUE
)
range_combined <- c(floor(actual_range[1]) - 1, ceiling(actual_range[2]) + 1)

# Convert to spatial format
merged_2023_sf <- st_as_sf(merged_2023, coords = c("pm25_grid_x", "pm25_grid_y"), crs = 4326)
merged_1999_sf <- st_as_sf(merged_1999, coords = c("pm25_grid_x", "pm25_grid_y"), crs = 4326)

# Plot: 1999 (keep y-axis, remove legend)
map_1999 <- ggplot() +
  geom_sf(data = merged_1999_sf, aes(color = pm25_ACAG_gwr), size = 1.5) +
  scale_color_viridis_c(
    option = "plasma", direction = -1,
    limits = range_combined, name = "PM2.5 (µg/m³)"
  ) +
  theme_minimal() +
  labs(title = "1999") +
  theme(
    axis.title.y = element_text(),   # keep y axis title
    axis.text.y = element_text(),    # keep y axis labels
    axis.ticks.y = element_line(),   # keep y axis ticks
    legend.position = "none"         # remove legend
  )

# Plot: 2023 (remove y-axis, keep legend)
map_2023 <- ggplot() +
  geom_sf(data = merged_2023_sf, aes(color = pm25_ACAG_gwr), size = 1.5) +
  scale_color_viridis_c(
    option = "plasma", direction = -1,
    limits = range_combined, name = "PM2.5 (µg/m³)"
  ) +
  theme_minimal() +
  labs(title = "2023") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Combine plots with overall title
(map_1999 | map_2023) +
  plot_annotation(
    title = "PM2.5 in Charlottesville and Albemarle County",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  )