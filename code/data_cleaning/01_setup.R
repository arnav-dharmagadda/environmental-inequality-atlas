################################################################################
# FILE: 01_setup.R
# PURPOSE: Set up the environment for cleaning and analyzing the gridded EIF
# files.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: gridpoints_with_county_2020.rda
# OUTPUTS: None.
################################################################################

#### Clear Environment ####

#rm(list = ls())

#### Setting working directory and file paths ####


setwd("/Users/jfischman/Library/CloudStorage/OneDrive-BowdoinCollege/Documents/GitHub/environmental-inequality-atlas/")

data_path <- "/Users/jfischman/Library/CloudStorage/OneDrive-BowdoinCollege/Documents/General Data/EIF atlas/"

gridpoints_path <- paste0(data_path, "/race_income/gridpoints_with_county_2020 (1).rda")

processed_path <- "data/processed/"

dta_path_ars <- "data/processed/ageracesex_dta/"

rda_path_ars <- "data/processed/ageracesex_rda/"

dta_path_ri <- "data/processed/raceincome_dta/"

rda_path_ri <- "data/processed/raceincome_rda/"

map_path <- "output/maps/"

#### Load Libraries ####

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(sf, terra, dplyr, ggplot2, tmap, arrow, dplyr, tidyr, scales, haven, stringr, purrr)

#### Load Data ####

load(gridpoints_path)
gridpoints <- df
rm(df)

#### Filter Gridpoints to Focus Area ####

gridpoints <- gridpoints %>%
  filter(STATEFP == "51" & COUNTYFP == "003" | COUNTYFP == "540") %>%
  rename(
    grid_lon = pm25_grid_x,
    grid_lat = pm25_grid_y
  )
