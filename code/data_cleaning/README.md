# Basic Information

Name: data_cleaning/
Author: Arnav Dharmagadda
Created: 6/3/2025
Time to Run: ~5 minutes

# Purpose

This module contains five individual R scripts that are used to process raw parquet files downloaded from the Census Gridded Environmental Impacts Frame (EIF). The files processed are from 1999 through 2023. Each script fulfills a different purpose:

* 01_setup.R: Sets up the R environment.
* 02_clean_ageracesex: Processes the age-race-sex files, saving in wide and long formats.
* 03_clean_ageracesex: Processes the age-race-sex files, saving in wide and long formats.
* 04_merge_ageracesex_raceincome: Merges grid-year and grid observation files across race-income and age-race-sex files.
* 05_polygonal_adjustment: Turns points into grid geometry for mapping.

The 00_master_script.R file provides a convenient way to run all five files. When running 01_setup.R individually, you may need to uncomment certain file paths at the beginning since these are currently adjusted to work on the master script. 

Census raw data can be downloaded here: https://www2.census.gov/ces/gridded_eif/

# Instructions

1. Run