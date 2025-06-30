# Load required libraries
library(sf)
library(dplyr)

setwd("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/gridded_eif_data/")

# Load the shapefile

shapefile <- st_read("cb_2024_us_cd119_500k/cb_2024_us_cd119_500k.shp")
# Load the gridpoints

load("all_gridpoints.rda")

gridpoints_sf <- st_as_sf(all_gridpoints, coords = c("pm25_grid_x", "pm25_grid_y"), crs = 4326)

# Make sure CRS matches
gridpoints_sf <- st_transform(gridpoints_sf, st_crs(shapefile))

# Spatial join
gridpoints_with_sf <- st_join(gridpoints_sf, shapefile, join = st_within)

df <- as.data.frame(gridpoints_with_sf)
df <- df[, !grepl("geometry", names(df))]

#save(df, file = "Data/Gridded_EIF/gridpoints_with_cbsa.rda")
save(df, file = "gridpoints_with_cd_2024.rda")
