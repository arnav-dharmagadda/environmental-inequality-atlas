# Basic Information

* Name: 07_grid_adjustment.R
* Author: Arnav Dharmagadda
* Created: 6/9/2025

# Purpose

This script will turn centroids in the previously generated dataframe .rda files into grid polygons for use in mapping. 

The defined function creates a new environment and loads the .rda files for analysis. It converts each observation to an sf point object. It expands the bounding box such that it goes 0.05 degrees to the left/right and up/down of the point. It builds a grid that covers the entire bounding box. It determines where each point is and it maps that to a grid cell. It overwrites the .rda file with the polygon data and saves the file.

The code will identify all .rda files in the processed folder and apply the function to that. It ignores the previously generated hexagonal files or the exploded point files.

# Instructions

1. Run
