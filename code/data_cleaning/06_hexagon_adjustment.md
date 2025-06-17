# Basic Information

* Name: 06_hexagon_adjustment.R
* Author: Arnav Dharmagadda
* Created: 6/9/2025

# Purpose

This script will turn points and centroids in the previously generated dataframe .rda files into hexagonal polygons for use in mapping. 

The defined function creates a new environment and loads the .rda files for analysis. It converts each observation to an sf point object. It expands the buffer such that it has 6 sides and has a 0.005 degree radius. It determines where each point is and it maps that to a grid cell. It create a new _hex.rda file with the polygon data and saves the file.

The code will identify all .rda files in the processed folder and apply the function to that. It also outputs a version of the original centroid files with HEX IDs attached, labeled as _with_hex_id.

# Instructions

1. Run
