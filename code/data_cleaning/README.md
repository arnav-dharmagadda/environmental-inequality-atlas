# Basic Information

* Name: data_cleaning/
* Author: Arnav Dharmagadda
* Created: 6/3/2025
* Time to Run: ~25 minutes

# Purpose

This module contains seven individual R scripts that are used to process raw parquet files downloaded from the Census Gridded Environmental Impacts Frame (EIF). The files processed are from 1999 through 2023. Each script fulfills a different purpose:

* 01_setup.R: Sets up the R environment.
* 02_clean_ageracesex: Processes the age-race-sex files, saving in wide and long formats.
* 03_clean_ageracesex: Processes the age-race-sex files, saving in wide and long formats.
* 04_merge_ageracesex_raceincome: Merges grid-year and grid observation files across race-income and age-race-sex files.
* 05_point_adjustment: Turns centroids into points, randomly distributed.
* 06_hexagon_adjustment: Turns centroids/points into hexagonal geometries for mapping.
* 07_grid_adjustment: Turns centroids into grid geometry for mapping.

The 00_master_script.R file provides a convenient way to run all five files. Set the working diretory, git_path, and data_path in the 01_setup.R file before running the master script.

Census raw data can be downloaded here: https://www2.census.gov/ces/gridded_eif/

# Instructions

1. Run
