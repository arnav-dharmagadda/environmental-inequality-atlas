################################################################################
# FILE: 01_setup.R
# PURPOSE: Set up the environment for cleaning and analyzing the gridded EIF
# pollution files by demographics.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: pollutants_long.rda, ageracesex_long.rda, raceincome_long.rda
# OUTPUTS: None.
################################################################################

#### Setting working directory and file paths ####

setwd("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/")

# File Paths

data_path <- "gridded_eif_data/"
processed <- "GitHub/environmental-inequality-atlas/data/processed/"
output <- "GitHub/environmental-inequality-atlas/output/pollutants/"

#### Load Libraries ####

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(sf, terra, dplyr, ggplot2, tmap, arrow, dplyr, tidyr, scales, haven, reactable, sparkline, htmltools, purrr, webshot2, ncdf4, stars)

#### Load Data ####

file_path <- paste0(processed, "pollutants_rda/nat_pollutants_2023.rda")
load(file_path)

file_path <- paste0(processed, "raceincome_rda/nat_raceincome_2024.rda")
load(file_path)

file_path <- paste0(processed, "pollutants_rda/pollutants_long.rda")
load(file_path) 

file_path <- paste0(processed, "ageracesex_rda/ageracesex_long.rda")
load(file_path)

file_path <- paste0(processed, "raceincome_rda/raceincome_long.rda")
load(file_path)

#### MERGE ####

raceincome_pollute <- pollutants_combined %>%
  left_join(raceincome_combined, by = c("grid_lon", "grid_lat", "year", "STATEFP", "COUNTYFP", "COUNTYNS", "GEOID", "GEOIDFQ", "NAME", "NAMELSAD", "STUSPS", "STATE_NAME", "LSAD", "ALAND", "AWATER"))

merged_year <- merged_year %>% 
  mutate(year = 2023)

national <- merged_year_nat %>%
  mutate(year = 2023) %>%
  left_join(merged_year, by = c("grid_lon", "grid_lat", "year", "STATEFP", "COUNTYFP", "COUNTYNS", "GEOID", "GEOIDFQ", "NAME", "NAMELSAD", "STUSPS", "STATE_NAME", "LSAD", "ALAND", "AWATER"))
