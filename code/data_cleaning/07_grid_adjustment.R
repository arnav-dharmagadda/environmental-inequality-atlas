################################################################################
# FILE: 07_grid_adjustment.R
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
process_rda_file <- function(file_path, lon_col = "grid_lon", lat_col = "grid_lat", cellsize = 0.01, filter_to_counties = TRUE) {
  # Step 0: Load the .rda file (assumes it contains ONE object, e.g., "race_2023")
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]  # Get the variable name inside the .rda
  df <- env[[df_name]]
  
  # Step 1: Convert to sf POINT object
  df_sf <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  
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
  
  # Optional: Filter grid to county area using existing county data (convex hull)
  if (filter_to_counties && "COUNTYFP" %in% colnames(df) && "STATEFP" %in% colnames(df)) {
    # Create a boundary from the county points (convex hull)
    county_points <- df %>%
      filter(STATEFP == "51" & (COUNTYFP == "003" | COUNTYFP == "540")) %>%
      dplyr::select(all_of(c(lon_col, lat_col))) %>%
      distinct()
    
    if (nrow(county_points) > 0) {
      # Convert county points to sf and create convex hull
      county_points_sf <- st_as_sf(county_points, coords = c(lon_col, lat_col), crs = 4326)
      county_boundary <- st_convex_hull(st_union(county_points_sf))
      
      # Keep only grid cells that intersect with the county boundary
      grid_sf <- st_filter(grid_sf, county_boundary, .predicate = st_intersects)
      
      message("Filtered grid to ", nrow(grid_sf), " cells within Charlottesville/Albemarle area")
    }
  }
  
  # Step 5: Spatial join
  df_gridded <- st_join(grid_sf, df_sf, join = st_contains)
  
  # Step 5.1: Optional - Remove grid cells with no data (skip for year_long files)
  file_basename <- basename(file_path)
  if (!grepl("raceincome_year_long|ageracesex_year_long", file_basename)) {
    df_gridded <- df_gridded[!is.na(df_gridded$n_noise_postprocessed), ]
  }
  
  # Step 5.2: Replace NAs with 0s for numeric columns (except grid coordinates)
  for (col_name in names(df_gridded)) {
    if (is.numeric(df_gridded[[col_name]]) && !col_name %in% c("grid_lon", "grid_lat")) {
      df_gridded[[col_name]] <- ifelse(is.na(df_gridded[[col_name]]), 0, df_gridded[[col_name]])
    }
  }
  
  # Overwrite the object in the environment
  assign(df_name, df_gridded, envir = env)
  
  # Step 6: Save back to the same .rda file
  save(list = df_name, file = file_path, envir = env)
}

# Run the function on all .rda files in the specified directory

all_rda_files <- list.files(processed_path, pattern = "\\.rda$", full.names = TRUE, recursive = TRUE)

# Filter to exclude files ending in "_hex.rda"
file_paths <- all_rda_files[!grepl("_hex\\.rda$", all_rda_files) & !grepl("^nat_", basename(all_rda_files)) & !grepl("_point\\.rda$", all_rda_files) & !grepl("_point_with_hex_id\\.rda$", all_rda_files)]

#file_paths <- '/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/raceincome_rda/raceincome_2024.rda'
#file_paths <- '/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/ageracesex_rda/ageracesex_2024.rda'


walk(file_paths, process_rda_file)

