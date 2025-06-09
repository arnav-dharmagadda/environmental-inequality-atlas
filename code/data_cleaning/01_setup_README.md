# Basic Information

* Name: 01_setup.R
* Author: Arnav Dharmagadda
* Created: 6/3/2025

# Purpose

This script sets up the R environment for cleaning the Gridded EIF age-race-sex and race-income files from 1999 through 2023.

It will clear the R environment, set file paths for use throughout the module, and it will load the necessary libraries.

It will load the gridpoints shapefile and allow a user to filter to the oberservations pertinent to analysis. When created, this file isolate observations to Albemarle and Charlottesville in Virginia. This can be edited to include the entire nation when needed.

# Instructions

1. Change the working directory in the setwd() command to reflect the appropriate working directory for the environmental-inequality-atlas repository
2. TEMPORARY: While data is not stored in the GitHub when working, you will need to set the data_path to be the location on your computer where you have stored the shared Dropbox data folder. 
3. If necessary, change the filters under "Filter Gridpoints to Focus Area" to reflect your needs.
