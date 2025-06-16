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

process_rda_to_hex_grid <- function(file_path, lon_col = "grid_lon", lat_col = "grid_lat", hex_cellsize = 0.02) {
  
  # Step 1: Load the .rda file
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]
  df <- env[[df_name]]
  
  # Step 2: Convert the original data frame to an sf POINT object
  points_sf <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  
  # Step 3: Create a single, seamless hexagonal grid that covers all points.
  hex_grid <- st_make_grid(points_sf, cellsize = hex_cellsize, square = FALSE)
  
  # Convert the grid to a full sf object so it can hold data
  hex_grid_sf <- st_sf(geometry = hex_grid) %>%
    mutate(hex_id = row_number()) # Add a unique ID to each hexagon
  
  # Step 4: Spatially join your original points to the new hexagonal grid.
  points_in_hex <- st_join(points_sf, hex_grid_sf, join = st_intersects)
  
  df_with_hex_id <- st_drop_geometry(points_in_hex)  # remove sf geometry
  assign(df_name, df_with_hex_id, envir = env)
  save(list = df_name, file = file_path, envir = env)
  message("Saved updated point data (with hex_id) to original file: ", file_path)
  
  grouping_vars <- c("hex_id")
  
  if ("year" %in% colnames(points_in_hex)) {
    grouping_vars <- c(grouping_vars, "year")
    message("Found 'year' column. Grouping by year.")
  } else {
    message("No 'year' column found. Proceeding without grouping by year.")
  }
  
  # Step 5: Aggregate the data for each hexagon by summing the population.
  hex_summary <- points_in_hex %>%
    st_drop_geometry() %>% # Drop geometry for faster, non-spatial grouping
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      across(
        where(is.numeric) & !any_of(c("grid_lon", "grid_lat", "COUNTYFP", "STATEFP", "NAME", "GEOID")), 
        ~if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE),
        .names = "{.col}" # This keeps the original column names
      ),
      grid_lon = first(grid_lon),
      grid_lat = first(grid_lat),
      COUNTYFP = first(COUNTYFP),
      STATEFP = first(STATEFP),
      name = first(NAME),
      GEOID = first(GEOID),
      n_points = n() 
    )
  
  # Step 6: Join the summarized data back to the hexagonal grid object.
  df_hex_grid <- hex_grid_sf %>%
    right_join(hex_summary, by = "hex_id") %>%
    filter(!is.na(hex_id)) # Keep only hexagons with population > 0
  
  # Step 7: Save to new .rda file with "_hex.rda" suffix
  hex_file_path <- sub("(_hex)?\\.rda$", "_hex.rda", file_path)
  
  assign(df_name, df_hex_grid, envir = env)
  save(list = df_name, file = hex_file_path, envir = env)
  
  message("Saved hex grid version to: ", hex_file_path)
}

#### Apply Functions to Data Files ####

# List of folders to search for input .rda files
folders <- c(processed_path, rda_path_ars, rda_path_ri)

# Get all .rda file paths from those folders
all_rda_files <- unlist(lapply(folders, function(dir) {
  list.files(dir, pattern = "\\.rda$", full.names = TRUE)
}))

file_paths <- all_rda_files[!grepl("_hex\\.rda$", all_rda_files) & !grepl("^nat_", basename(all_rda_files))]

# Run the processing function on each file
walk(file_paths, process_rda_to_hex_grid)
