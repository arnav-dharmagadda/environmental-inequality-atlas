###################################################################
# Purpose: cleaning and organizing income gridded eif data
# Author: Josie Fischman
# Date: 6/3/2025
# Inputs:
# Outputs: 
###################################################################
# Load libraries
library(arrow)     # for reading parquet
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)

setwd("/Users/jfischman/Library/CloudStorage/OneDrive-BowdoinCollege/Documents/GitHub/environmental-inequality-atlas/data")
# Load CBSA gridpoints

load("gridpoints_with_county_2020 (1).rda")

# Load population data 

pop <- read_parquet("race_income/gridded_eif_pop_raceincome_2023.parquet")
county <- df

names(pop)
names(county)

pop_ <- pop %>%
  rename(pm25_grid_x = grid_lon, pm25_grid_y = grid_lat)

pop_income <- pop_ %>%
  group_by(pm25_grid_x, pm25_grid_y, income_decile) %>%
  summarise(
    n_noise = sum(n_noise, na.rm = TRUE),
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

pop_income_wide <- pop_income %>%
  pivot_wider(
    names_from = income_decile,
    values_from = c(n_noise, n_noise_postprocessed),
    names_sep = "_"
  )


# Filter the county data for Charlottesville and Albemarle County
# Charlottesville: STATEFP = "51", COUNTYFP = "540"
# Albemarle: STATEFP = "51", COUNTYFP = "003"

county_filtered <- county %>%
  filter(
    (STATEFP == "51" & COUNTYFP == "540")  |
      (STATEFP == "51" & COUNTYFP == "003")
  )

# Merge the dataframes

merged2 <- county_filtered %>%
  inner_join(pop_income_wide, by = c("pm25_grid_x", "pm25_grid_y"))

names(merged2)





