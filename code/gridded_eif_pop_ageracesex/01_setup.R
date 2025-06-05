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

ageracesex_2023_path <- paste0(data_path, "gridded_eif_pop_ageracesex/gridded_eif_pop_ageracesex_2023.parquet")

gridpoints_path <- paste0(data_path, "gridpoints_with_county_2020.rda")

output_path <- paste0(data_path, "processed/gridded_eif_pop_ageracesex/")

#### Load Libraries ####

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(sf, terra, dplyr, ggplot2, tmap, arrow, dplyr, tidyr, scales, haven)

#### Load Data ####

load(gridpoints_path)
gridpoints <- df
rm(df)

ageracesex_2023 <- read_parquet(ageracesex_2023_path)

#### Filter and Clean Data ####

gridpoints <- gridpoints %>%
  filter(STATEFP == "51", COUNTYFP %in% c("540", "003")) %>%
  rename(
    grid_lon = pm25_grid_x,
    grid_lat = pm25_grid_y
  )

# Loop 1999 through 2023

for (year in 1999:2023) {
  # Construct path to parquet file
  parquet_path <- paste0(data_path, "gridded_eif_pop_ageracesex/gridded_eif_pop_ageracesex_", year, ".parquet")
  
  # Read the parquet file
  ageracesex_year <- read_parquet(parquet_path)
  
  # Merge with filtered gridpoints
  merged_year <- left_join(gridpoints, ageracesex_year, by = c("grid_lon", "grid_lat"))
  
  # Write to Stata .dta
  output_file <- paste0(output_path, "ageracesex_", year, ".dta")
  write_dta(merged_year, output_file)
}

ageracesex_2023 <- left_join(gridpoints, ageracesex_2023, by = c("grid_lon", "grid_lat"))

# Reshape and collapse the data into separate race, age, and sex files

race_2023 <- ageracesex_2023 %>%
    pivot_wider(
      names_from = c(race_ethnicity),
      values_from = n_noise_postprocessed,
      names_sep = "_"
    ) %>%
    group_by(grid_lon, grid_lat) %>%
    summarize(
      asian = sum(Asian, na.rm = TRUE),
      white = sum(White, na.rm = TRUE),
      `other` = sum(`Other/Unknown`, na.rm = TRUE),
      black = sum(Black, na.rm = TRUE),
      hispanic = sum(Hispanic, na.rm = TRUE),
      aian = sum(AIAN, na.rm = TRUE),
      total = sum(c(Asian, White, Black, Hispanic, AIAN, `Other/Unknown`), na.rm = TRUE),
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
    ) %>%
    mutate(
      black_share = black / total,
      white_share = white / total,
      hispanic_share = hispanic / total,
      asian_share = asian / total
    )

sex_2023 <- ageracesex_2023 %>%
  pivot_wider(
    names_from = c(sex),
    values_from = n_noise_postprocessed,
    names_sep = "_"
  ) %>%
  group_by(grid_lon, grid_lat) %>%
  summarize(
    male = sum(M, na.rm = TRUE),
    female = sum(F, na.rm = TRUE),
    total = sum(c(M, F), na.rm = TRUE),
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
  ) %>%
  mutate(
    male_share = male / total,
    female_share = female / total
  )

age_2023 <- ageracesex_2023 %>%
  pivot_wider(
    names_from = c(age_group),
    values_from = n_noise_postprocessed,
    names_sep = "_"
  ) %>%
  group_by(grid_lon, grid_lat) %>%
  summarize(
    under_18 = sum(`Under 18`, na.rm = TRUE),
    over_65 = sum(`Over 65`, na.rm = TRUE),
    bet_19_65 = sum(`19-65`, na.rm = TRUE),
    total = sum(c(`Under 18`, `Over 65`, `19-65`), na.rm = TRUE),
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
  ) %>%
  mutate(
    under_18_share = under_18 / total,
    bet_19_65_share = bet_19_65 / total,
    over_65_share = over_65 / total
  )

agerace_2023_long <- ageracesex_2023 %>%
  group_by(grid_lon, grid_lat, race_ethnicity, age_group) %>%
  summarize(
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
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

# Adapt to appropriate polygon sf objects

## Race

### Step 1: Convert to sf POINT object
race_2023 <- st_as_sf(race_2023, coords = c("grid_lon", "grid_lat"), crs = 4326)

### Step 2: Get expanded bounding box to fully contain all grid cells
bbox <- st_bbox(race_2023)
bbox_expanded <- bbox
bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - 0.005
bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + 0.005

### Step 3: Create grid polygons at 0.01° resolution covering the area
grid_polygons <- st_make_grid(
  cellsize = c(0.01, 0.01),
  offset = c(bbox_expanded["xmin"], bbox_expanded["ymin"]),
  n = c(ceiling((bbox_expanded["xmax"] - bbox_expanded["xmin"]) / 0.01),
        ceiling((bbox_expanded["ymax"] - bbox_expanded["ymin"]) / 0.01)),
  crs = 4326,
  what = "polygons"
)

### Step 4: Convert grid to sf and join with your original data (spatial match)
grid_sf <- st_sf(geometry = grid_polygons)

### Step 5: Spatial join: match points to polygons (assumes 1 point per cell)
race_2023 <- st_join(grid_sf, race_2023, join = st_contains)

## Sex

### Step 1: Convert to sf POINT object
sex_2023 <- st_as_sf(sex_2023, coords = c("grid_lon", "grid_lat"), crs = 4326)

### Step 2: Get expanded bounding box to fully contain all grid cells
bbox <- st_bbox(sex_2023)
bbox_expanded <- bbox
bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - 0.005
bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + 0.005

### Step 3: Create grid polygons at 0.01° resolution covering the area
grid_polygons <- st_make_grid(
  cellsize = c(0.01, 0.01),
  offset = c(bbox_expanded["xmin"], bbox_expanded["ymin"]),
  n = c(ceiling((bbox_expanded["xmax"] - bbox_expanded["xmin"]) / 0.01),
        ceiling((bbox_expanded["ymax"] - bbox_expanded["ymin"]) / 0.01)),
  crs = 4326,
  what = "polygons"
)

### Step 4: Convert grid to sf and join with your original data (spatial match)
grid_sf <- st_sf(geometry = grid_polygons)

### Step 5: Spatial join: match points to polygons (assumes 1 point per cell)
sex_2023 <- st_join(grid_sf, sex_2023, join = st_contains)

## Age Group

### Step 1: Convert to sf POINT object
age_2023 <- st_as_sf(age_2023, coords = c("grid_lon", "grid_lat"), crs = 4326)

### Step 2: Get expanded bounding box to fully contain all grid cells
bbox <- st_bbox(age_2023)
bbox_expanded <- bbox
bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - 0.005
bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + 0.005

### Step 3: Create grid polygons at 0.01° resolution covering the area
grid_polygons <- st_make_grid(
  cellsize = c(0.01, 0.01),
  offset = c(bbox_expanded["xmin"], bbox_expanded["ymin"]),
  n = c(ceiling((bbox_expanded["xmax"] - bbox_expanded["xmin"]) / 0.01),
        ceiling((bbox_expanded["ymax"] - bbox_expanded["ymin"]) / 0.01)),
  crs = 4326,
  what = "polygons"
)

### Step 4: Convert grid to sf and join with your original data (spatial match)
grid_sf <- st_sf(geometry = grid_polygons)

### Step 5: Spatial join: match points to polygons (assumes 1 point per cell)
age_2023 <- st_join(grid_sf, age_2023, join = st_contains)
