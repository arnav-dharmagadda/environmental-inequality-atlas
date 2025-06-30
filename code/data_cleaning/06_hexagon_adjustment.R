################################################################################
# FILE: 06_hexagon_adjustment.R
# PURPOSE: Make hexagonal polygons instead of square polygons, with optional
# filtering to specific geographic areas (counties).
# AUTHOR: Arnav Dharmagadda
# CREATED: June 10th, 2025
################################################################################
# INPUTS: Processed .rda files in data/processed/ageracesex_rda/,
# data/processed/raceincome_rda/, and data/processed/
# OUTPUTS: Versions of those files with _hex suffixes, versions of those files 
# with hex IDs attached (_with_hex_id)
################################################################################

#### Define Functions ####

process_rda_to_hex_grid <- function(file_path, lon_col = "grid_lon", lat_col = "grid_lat", hex_cellsize = 0.05, filter_to_counties = TRUE) {
  
  # Step 1: Load the .rda file
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]
  df <- env[[df_name]]

  # Step 2: Convert the original data frame to an sf POINT object
  if (!inherits(df, "sf")) {
    points_sf <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  } else {
    points_sf <- df
  }
  
  # Step 3: Create a single, seamless hexagonal grid that covers all points.
  hex_grid <- st_make_grid(points_sf, cellsize = hex_cellsize, square = FALSE)
  
  # Convert the grid to a full sf object so it can hold data
  hex_grid_sf <- st_sf(geometry = hex_grid) %>%
    mutate(hex_id = row_number()) # Add a unique ID to each hexagon
  
  # Optional: Filter hexagons to county area using existing county data
  if (filter_to_counties && "GEOID" %in% colnames(df) && "STATEFP" %in% colnames(df)) {
    # Create a boundary from the county points (convex hull)
    county_points <- df %>%
      filter(STATEFP == "51" & (GEOID == "5105")) %>%
      dplyr::select(all_of(c(lon_col, lat_col))) %>%
      distinct()
    
    if (nrow(county_points) > 0) {
      # Convert county points to sf and create convex hull
      county_points_sf <- st_as_sf(county_points, coords = c(lon_col, lat_col), crs = 4326)
      county_boundary <- st_convex_hull(st_union(county_points_sf))
      
      # Transform to match hex grid CRS
      county_boundary <- st_transform(county_boundary, st_crs(hex_grid_sf))
      
      # Keep only hexagons that intersect with the county boundary
      hex_grid_sf <- st_filter(hex_grid_sf, county_boundary, .predicate = st_intersects)
      
      message("Filtered hexagonal grid to ", nrow(hex_grid_sf), " hexagons within Charlottesville/Albemarle area")
    }
  }
  
  # Step 4: Spatially join your original points to the new hexagonal grid.
  points_in_hex <- st_join(points_sf, hex_grid_sf, join = st_intersects)
  
  intermediate_file_path <- sub("\\.rda$", "_with_hex_id.rda", file_path)
  
  # Save the point data with hex_id to this new file
  df_with_hex_id <- st_drop_geometry(points_in_hex)
  assign(df_name, df_with_hex_id, envir = env)
  save(list = df_name, file = intermediate_file_path, envir = env)
  message("Saved updated point data (with hex_id) to new intermediate file: ", intermediate_file_path)
  
  grouping_vars <- c("hex_id")
  
  if ("year" %in% colnames(points_in_hex)) {
    grouping_vars <- c(grouping_vars, "year")
    message("Found 'year' column. Grouping by year.")
  } else {
    message("No 'year' column found. Proceeding without grouping by year.")
  }
  
  # Step 5: Aggregate the data for each hexagon by summing the population.
  hex_summary <- points_in_hex %>%
    st_drop_geometry() %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      # any_of() will not throw an error if a column is not found.
      # This correctly handles `hex_id` being a grouping variable.
      across(
        c(where(is.numeric), -any_of(c(lon_col, lat_col, "hex_id", "STATEFP", "GEOID", "name"))),
        ~if(all(is.na(.))) NA_real_ else sum(., na.rm = TRUE),
        .names = "{.col}"
      ),
      # The rest of the summary remains the same
      grid_lon = first(grid_lon),
      grid_lat = first(grid_lat),
      STATEFP = first(STATEFP),
      name = first(NAMELSAD),
      GEOID = first(GEOID),
      n_points = n(),
      .groups = "drop" # Use .groups = "drop" to automatically ungroup
    )
  
  # Step 6: Join the summarized data back to the hexagonal grid object.
  # Use left_join to preserve ALL hexagons in the filtered area, including empty ones
  df_hex_grid <- hex_grid_sf %>%
    left_join(hex_summary, by = "hex_id") %>%
    # Replace NA values with 0 for numeric columns (empty hexagons)
    mutate(
      across(where(is.numeric), ~tidyr::replace_na(.x, 0)),
      # For character columns that should have values, use appropriate defaults
      grid_lon = ifelse(is.na(grid_lon), st_coordinates(st_centroid(geometry))[,1], grid_lon),
      grid_lat = ifelse(is.na(grid_lat), st_coordinates(st_centroid(geometry))[,2], grid_lat),
      STATEFP = ifelse(is.na(STATEFP), "unknown", STATEFP),
      name = ifelse(is.na(name), "unknown", name),
      GEOID = ifelse(is.na(GEOID), "unknown", GEOID),
      n_points = ifelse(is.na(n_points), 0, n_points)
    )
  
  empty_hexagons <- sum(df_hex_grid$n_points == 0)
  total_hexagons <- nrow(df_hex_grid)
  message("Created ", total_hexagons, " total hexagons (", empty_hexagons, " empty, ", 
          total_hexagons - empty_hexagons, " with data)")
  
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

file_paths <- all_rda_files[!grepl("_hex\\.rda$", all_rda_files) & !grepl("^nat_", basename(all_rda_files)) & !grepl("_with_hex_id\\.rda$", basename(all_rda_files))]

# Run the processing function on each file with county filtering enabled
walk(file_paths, ~ process_rda_to_hex_grid(.x, filter_to_counties = TRUE))

