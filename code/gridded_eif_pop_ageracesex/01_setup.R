################################################################################
# FILE: 01_setup.R
# PURPOSE: Set up the environment for cleaning and analyzing the gridded EIF
# age, race, and sex files.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: 
# OUTPUTS:
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

library(arrow)

for (year in 1999:2023) {
  path_var <- paste0("ageracesex_", year, "_path")
  path <- get(path_var)
  assign(paste0("ageracesex_", year), read_parquet(path))
}


