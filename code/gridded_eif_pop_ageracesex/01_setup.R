################################################################################
# FILE: 01_setup.R
# PURPOSE: Set up the environment for cleaning and analyzing the gridded EIF
# age, race, and sex files.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: gridpoints_with_county_2020.rda, gridded_eif_pop_ageracesex files
# (1999-2023)
# OUTPUTS: None.
################################################################################

#### Clear Environment ####

rm(list = ls())

#### Setting working directory and file paths ####

setwd("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/")

data_path <- "data/"

ageracesex_paths <- setNames(
  paste0(data_path, "gridded_eif_pop_ageracesex/gridded_eif_pop_ageracesex_", 1999:2023, ".parquet"),
  paste0("ageracesex_", 1999:2023, "_path")
)
list2env(as.list(ageracesex_paths), envir = .GlobalEnv)

gridpoints_path <- paste0(data_path, "gridpoints_with_county_2020.rda")

#### Load Libraries ####

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(sf, terra, dplyr, ggplot2, tmap, arrow, dplyr, tidyr)

#### Load Data ####

load(gridpoints_path)
gridpoints <- df
rm(df)

for (year in 2020:2023) {
  path_var <- paste0("ageracesex_", year, "_path")
  path <- get(path_var)
  assign(paste0("ageracesex_", year), read_parquet(path))
}

#### Filter and Clean Data ####

gridpoints <- gridpoints %>%
  filter(STATEFP == "51", COUNTYFP %in% c("540", "003")) %>%
  rename(
    grid_lon = pm25_grid_x,
    grid_lat = pm25_grid_y
  )

for (year in 2020:2023) {
  ageracesex_name <- paste0("ageracesex_", year)
  assign(
    ageracesex_name,
    left_join(gridpoints, get(ageracesex_name), by = c("grid_lon", "grid_lat"))
  )
}

for (year in 2020:2023) {
  ageracesex_name <- paste0("ageracesex_", year)
  df <- get(ageracesex_name)
  df_wide <- df %>%
    pivot_wider(
      names_from = c(race_ethnicity),
      values_from = n_noise_postprocessed,
      names_sep = "_"
    )
  assign(ageracesex_name, df_wide)
}

# Collapse based on race variables

ageracesex_2020 <- ageracesex_2020 %>%
  group_by(grid_lon, grid_lat) %>%
  summarize(
    Asian = sum(Asian, na.rm = TRUE),
    White = sum(White, na.rm = TRUE),
    `Other/Unknown` = sum(`Other/Unknown`, na.rm = TRUE),
    Black = sum(Black, na.rm = TRUE),
    Hispanic = sum(Hispanic, na.rm = TRUE),
    AIAN = sum(AIAN, na.rm = TRUE),
    STATEFP = first(STATEFP),
    COUNTYFP = first(COUNTYFP),
    COUNTYNS = first(COUNTYNS),
    AFFGEOID = first(AFFGEOID),
    GEOID = first(GEOID),
    NAME = first(NAME),
    LSAD = first (LSAD),
    ALAND = first(ALAND),
    AWATER = first(AWATER),
    .groups = "drop"
  )

# Step 1: Convert to sf POINT object
ageracesex_2020 <- st_as_sf(ageracesex_2020, coords = c("grid_lon", "grid_lat"), crs = 4326)

# Step 2: Get expanded bounding box to fully contain all grid cells
bbox <- st_bbox(ageracesex_2020)
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
ageracesex_2020 <- st_join(grid_sf, ageracesex_2020, join = st_contains)

