################################################################################
# FILE: 05_polygonal_adjustment.R
# PURPOSE: Make grid polygons instead of points
# AUTHOR: Arnav Dharmagadda
# CREATED: June 9th, 2025
################################################################################
# INPUTS: Processed .rda files in data/processed/ageracesex_rda/,
# data/processed/raceincome_rda/, and data/processed/
# OUTPUTS: Overwritten .rda files with grid polygons
################################################################################

#### DEFINE A FUNCTION ####

# Function to process and overwrite an .rda file
process_rda_file <- function(file_path, lon_col = "grid_lon", lat_col = "grid_lat", cellsize = 0.01) {
  # Step 0: Load the .rda file (assumes it contains ONE object, e.g., "race_2023")
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]  # Get the variable name inside the .rda
  df <- env[[df_name]]
  
  # Step 1: Convert to sf POINT object
  df_sf <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)
  
  # Step 2: Expand bounding box
  bbox <- st_bbox(df_sf)
  bbox_expanded <- bbox
  bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - cellsize / 2
  bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + cellsize / 2
  
  # Step 3: Create grid
  grid_polygons <- st_make_grid(
    cellsize = c(cellsize, cellsize),
    offset = c(bbox_expanded["xmin"], bbox_expanded["ymin"]),
    n = c(
      ceiling((bbox_expanded["xmax"] - bbox_expanded["xmin"]) / cellsize),
      ceiling((bbox_expanded["ymax"] - bbox_expanded["ymin"]) / cellsize)
    ),
    crs = 4326,
    what = "polygons"
  )
  
  # Step 4: Convert grid to sf
  grid_sf <- st_sf(geometry = grid_polygons)
  
  # Step 5: Spatial join
  df_gridded <- st_join(grid_sf, df_sf, join = st_contains)
  
  # Overwrite the object in the environment
  assign(df_name, df_gridded, envir = env)
  
  # Step 6: Save back to the same .rda file
  save(list = df_name, file = file_path, envir = env)
}

# Run the function on all .rda files in the specified directory

file_paths <- list.files("data/processed/", pattern = "\\.rda$", full.names = TRUE, recursive = TRUE)
file_paths <- "data/processed/raceincome_rda/raceincome_2023.rda"
walk(file_paths, process_rda_file)

