################################################################################
# FILE: 05_hexagon_adjustment.R
# PURPOSE: Make hexagonal polygons instead of square polygons.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 10th, 2025
################################################################################
# INPUTS: Processed .rda files in data/processed/ageracesex_rda/,
# data/processed/raceincome_rda/, and data/processed/
# OUTPUTS: Versions of those files with _hex suffixes
################################################################################

#### Define Functions ####

# Function to create a hexagon centered at a point
create_hexagon <- function(point, radius = 0.005) {
  st_buffer(point, dist = radius, nQuadSegs = 6)
}

# Function to process a single .rda file into hexagon polygons
process_rda_to_hex <- function(file_path, lon_col = "grid_lon", lat_col = "grid_lat", radius = 0.005) {
  # Step 1: Load the original .rda file into a temporary environment
  env <- new.env()
  load(file_path, envir = env)
  
  # Step 2: Extract the data frame inside
  df_name <- ls(env)[1]
  df <- env[[df_name]]
  
  # Step 3: Convert to sf POINT object
  df_sf <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)
  
  # Step 4: Create hexagons centered on each point
  hex_geom <- st_geometry(df_sf) %>%
    lapply(create_hexagon, radius = radius) %>%
    st_sfc(crs = 4326)
  
  # Step 5: Create a new sf object with hexagonal geometries
  df_hex <- st_sf(df, geometry = hex_geom)
  
  # Step 6: Save to new .rda file with _hex suffix
  hex_file_path <- sub("\\.rda$", "_hex.rda", file_path)
  assign(df_name, df_hex, envir = env)
  save(list = df_name, file = hex_file_path, envir = env)
  
  message("Saved hex version to: ", hex_file_path)
}

#### Apply Functions to Data Files ####

# === Load and process all .rda files from multiple folders === #

# List of folders to search for input .rda files
folders <- c(processed_path, rda_path_ars, rda_path_ri)

# Get all .rda file paths from those folders
file_paths <- unlist(lapply(folders, function(dir) {
  list.files(dir, pattern = "\\.rda$", full.names = TRUE)
}))

# Run the processing function on each file
walk(file_paths, process_rda_to_hex)
