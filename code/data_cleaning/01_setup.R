################################################################################
# FILE: 01_setup.R
# PURPOSE: Set up the environment for cleaning and analyzing the gridded EIF
# files.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: gridpoints_with_county_2024.rda
# OUTPUTS: None.
################################################################################

#### Clear Environment ####

#rm(list = ls())

#### Setting working directory and file paths ####

setwd("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/")

data_path <- "gridded_eif_data/"

git_path <- "GitHub/environmental-inequality-atlas/"

gridpoints_path <- paste0(data_path, "gridpoints_with_cd_2024.rda")

processed_path <- paste0(git_path, "data/processed/")

dta_path_ars <- paste0(git_path, "data/processed/ageracesex_dta/")

rda_path_ars <- paste0(git_path, "data/processed/ageracesex_rda/")

dta_path_ri <- paste0(git_path, "data/processed/raceincome_dta/")

rda_path_ri <- paste0(git_path, "data/processed/raceincome_rda/")

map_path <- paste0(git_path, "output/maps/")

#### Load Libraries ####

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(ncdf4, sf, terra, dplyr, ggplot2, tmap, arrow, dplyr, tidyr, scales, haven, stringr, purrr)

#### Load Data ####

load(gridpoints_path)
gridpoints <- df
gridpoints_nat <- gridpoints
rm(df)

#### Filter Gridpoints to Focus Area ####

gridpoints <- gridpoints %>%
  filter(GEOID == "5105") %>%
  mutate(
    #grid_lon = as.character(grid_lon),
    #grid_lat = as.character(grid_lat)
  ) %>%
  rename(
    grid_lon = pm25_grid_x,
    grid_lat = pm25_grid_y
  )

gridpoints_nat <- gridpoints_nat %>%
  mutate(
    #grid_lon = as.character(grid_lon),
    #grid_lat = as.character(grid_lat)
  ) %>%
  rename(
    grid_lon = pm25_grid_x,
    grid_lat = pm25_grid_y
  )

