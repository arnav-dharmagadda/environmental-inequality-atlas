###################################################################
# Purpose: initial visualizations of race-income gridded eif data
# Author: Josie Fischman
# Date: 6/3/2025
# Inputs: raceincome_wide
# Outputs: Binned Decile 10 Resident Counts.png, Binned Decile 1 Resident Counts.png, Binned Resident Counts in Deciles 0-3.png, Binned Resident Counts in Deciles 8-10.png, Percent of Residents in 0-3 Deciles.png, Percent of Residents in 0-3 Deciles2.png  
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
# Convert wide raceincome data to sf (point layer)
raceincome_sf <- st_as_sf(
  raceincome_wide,
  coords = c("grid_lon", "grid_lat"),
  crs = 4326  # WGS 84 (standard GPS lat/lon)
)

# Convert to long format, extract components, and filter to 2023
raceincome_2023 <- raceincome_sf %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "variable",
    values_to = "count"
  ) %>%
  filter(!is.na(count)) %>%
  mutate(
    race = gsub("_.*", "", variable),
    decile = as.integer(str_extract(variable, "(?<=_inc_)\\d+")),
    year = as.integer(str_extract(variable, "\\d{4}$"))
  ) %>%
  filter(year == 2023, race != "other", !is.na(decile))

# Create bounding box and grid
bbox <- st_bbox(raceincome_2023)
bbox_expanded <- bbox
bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - 0.005
bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + 0.005

grid_polygons <- st_make_grid(
  cellsize = c(0.01, 0.01),
  offset = c(bbox_expanded["xmin"], bbox_expanded["ymin"]),
  n = c(
    ceiling((bbox_expanded["xmax"] - bbox_expanded["xmin"]) / 0.01),
    ceiling((bbox_expanded["ymax"] - bbox_expanded["ymin"]) / 0.01)
  ),
  crs = 4326,
  what = "polygons"
)

grid_sf <- st_sf(geometry = grid_polygons)

# Spatial join: match points to grid cells
raceincome_2023 <- st_join(grid_sf, raceincome_2023, join = st_contains)

# Sum up income decile values for each decile
raceincome_2023 <- raceincome_2023 %>%
  group_by(geometry, race) %>%
  mutate(
    decile_0_3_total = sum(if_else(decile %in% 0:3, count, 0, missing = 0)),
    decile_8_10_total = sum(if_else(decile %in% 8:10, count, 0, missing = 0)),
    decile_total = sum(count, na.rm = TRUE),
    percent_0_3 = if_else(decile_total > 0, 100 * decile_0_3_total / decile_total, NA_real_)
  ) %>%
  ungroup()

# Bin and plot decile 10 only
decile10_data <- raceincome_2023 %>% filter(decile == 10)
decile10_data$decile10_bin <- cut(
  decile10_data$count,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)

x <- ggplot(decile10_data) +
  geom_sf(aes(fill = decile10_bin), color="white", size = 2) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey90") +
  labs(
    title = "Binned decile 10 Resident Counts",
    fill = "Count Bin"
  ) +
  theme_minimal()

ggsave("output/raceincome/income/Binned Decile 10 Resident Counts.png", plot = x, width = 6, height = 4)

# Decile 1 only
decile1_data <- raceincome_2023 %>% filter(decile == 1)
decile1_data$decile1_bin <- cut(
  decile1_data$count,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)

y <- ggplot(decile1_data) +
  geom_sf(aes(fill = decile1_bin), color="white", size = 2) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey90") +
  labs(
    title = "Binned decile 1 Resident Counts",
    fill = "Count Bin"
  ) +
  theme_minimal()
ggsave("output/raceincome/income/Binned Decile 1 Resident Counts.png", plot = y, width = 6, height = 4)


# Bin the summed values in decile 0–3
raceincome_2023$decile_0_3_bin <- cut(
  raceincome_2023$decile_0_3_total,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)

z<- ggplot(raceincome_2023) +
  geom_sf(aes(fill = decile_0_3_bin), color="white", size = 2) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey90") +
  labs(
    title = "Binned Resident Counts in Deciles 0–3",
    color = "Count Bin"
  ) +
  theme_minimal()

ggsave("output/raceincome/income/Binned Resident Counts in Deciles 0-3.png", plot = z, width = 6, height = 4)


# Bin the summed values in decile 8–10
raceincome_2023$decile_8_10_bin <- cut(
  raceincome_2023$decile_8_10_total,
  breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),
  labels = c("0", "1–5", "6–10", "11–20", "21–50", "50+"),
  right = TRUE
)

a <- ggplot(raceincome_2023) +
  geom_sf(aes(fill = decile_8_10_bin), color="white", size = 2) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey90") +
  labs(
    title = "Binned Resident Counts in Deciles 8–10",
    color = "Count Bin"
  ) +
  theme_minimal()
ggsave("output/raceincome/income/Binned Resident Counts in Deciles 8-10.png", plot = a, width = 6, height = 4)


# Plot percent of residents in 0–3 deciles
b <- ggplot(raceincome_2023) +
  geom_sf(aes(fill = percent_0_3), color="white", size = 2.5) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "grey90",
    name = "% in 0–3 Decile"
  ) +
  labs(title = "Percent of Residents in 0–3 Deciles") +
  theme_minimal()
ggsave("output/raceincome/income/Percent of Residents in 0-3 Deciles.png", plot = b, width = 6, height = 4)


# Share of people in bottom 3 deciles by group
grouped_low_share <- raceincome_2023 %>%
  group_by(geometry, race) %>%
  summarise(
    total_low = sum(if_else(decile %in% 0:3, count, 0, missing = 0)),
    total_all = sum(count, na.rm = TRUE),
    share_low = if_else(total_all > 0, total_low / total_all, NA_real_)
  ) %>%
  ungroup()

c <- ggplot(grouped_low_share) +
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
ggsave("output/raceincome/income/Percent of Residents in 0-3 Deciles2.png", plot = c, width = 6, height = 4)

