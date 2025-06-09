###################################################################
# Purpose: combining race and income--cleaning and organizing
# Author: Josie Fischman
# Date: 6/4/2025
# Inputs:
# Outputs: 
###################################################################
# Load libraries
library(arrow)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)

# Set working directory
setwd("/Users/jfischman/Library/CloudStorage/OneDrive-BowdoinCollege/Documents/GitHub/environmental-inequality-atlas/data")

# Load gridpoints and population data
load("gridpoints_with_county_2020 (1).rda")
pop <- read_parquet("race_income/gridded_eif_pop_raceincome_2023.parquet")
county <- df

# Rename coordinates to match
pop <- pop %>%
  rename(pm25_grid_x = grid_lon, pm25_grid_y = grid_lat)

# Filter for Charlottesville and Albemarle
county_filtered <- county %>%
  filter(
    (STATEFP == "51" & COUNTYFP == "540")  |
      (STATEFP == "51" & COUNTYFP == "003")
  )

# STEP 1: Summarize by grid, race, and income decile
pop_summary <- pop %>%
  group_by(pm25_grid_x, pm25_grid_y, income_decile, race_ethnicity) %>%
  summarise(
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

# STEP 2: Create a single column name like n_noise_Black_0
pop_summary <- pop_summary %>%
  mutate(colname = paste0("n_noise_", race_ethnicity, "_", income_decile))

# STEP 3: Pivot wider to make each colname a new column
pop_wide <- pop_summary %>%
  select(pm25_grid_x, pm25_grid_y, colname, n_noise_postprocessed) %>%
  pivot_wider(
    names_from = colname,
    values_from = n_noise_postprocessed
  )

# STEP 4: Merge with the county shapefile
merged3 <- county_filtered %>%
  inner_join(pop_wide, by = c("pm25_grid_x", "pm25_grid_y"))

race_long <- merged3 %>%
  pivot_longer(
    cols = starts_with("n_noise_"),
    names_to = "race_decile",
    values_to = "count"
  ) %>%
  filter(!is.na(count)) %>%
  mutate(
    # Extract race and decile from column name
    race = sub("n_noise_(.*?)_\\d+$", "\\1", race_decile),
    decile = as.integer(sub(".*_(\\d+)$", "\\1", race_decile))
  )

race_summary <- race_long %>%
  group_by(race, decile) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")

