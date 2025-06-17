# Basic Information

* Name: 05_point_adjustment.R
* Author: Arnav Dharmagadda
* Created: 6/17/2025

# Purpose

This script generates spatial point data from gridded population data stored in .rda files. For each input file, it loads the dataset, checks for the presence of a specified population column (defaulting to n_noise_postprocessed), and then simulates one point per person based on the (possibly fractional) population count in each grid cell. Points are uniformly distributed within the bounds of the original grid cell, maintaining the spatial resolution defined by cell_size. The function also carries forward all relevant metadata from each row and adds one-hot encoded indicator variables for categorical attributes such as race, age group, sex, and income decile.

The final output is a spatial (sf) object where each point represents one person, complete with geographic coordinates and associated demographic indicators. These outputs are saved with a _point.rda suffix in the same directory as the input files. The script automatically applies this transformation across multiple directories, skipping any files that have already been processed or do not meet naming conventions. This approach is especially useful for creating detailed person-level synthetic populations for spatial analysis and visualization.

# Instructions

1. Run
