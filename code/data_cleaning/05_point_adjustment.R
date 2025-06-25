################################################################################
# FILE: 05_point_adjustment.R
# PURPOSE: Explode grids into points.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 16th, 2025
################################################################################
# INPUTS: Processed .rda files in data/processed/ageracesex_rda/,
# data/processed/raceincome_rda/, and data/processed/
# OUTPUTS: Versions of those files with _point suffixes
################################################################################

#### DEFINE FUNCTION ####

generate_people_points_from_file <- function(file_path,
                                             pop_col = "n_noise_postprocessed",
                                             cell_size = 0.01,
                                             seed = 2005) {
  set.seed(seed)
  
  # Load the .rda file into a temporary environment
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]
  df <- env[[df_name]]
  
  # Skip if population column is missing
  if (!(pop_col %in% colnames(df))) {
    message("Skipping: ", file_path, " (column '", pop_col, "' not found)")
    return(invisible(NULL))
  }
  
  # Inner function: generate person-points for one row
  generate_people_points <- function(row) {
    lon_center <- as.numeric(row$grid_lon)
    lat_center <- as.numeric(row$grid_lat)
    pop <- suppressWarnings(as.numeric(row[[pop_col]]))
    
    if (is.na(pop) || pop <= 0) return(NULL)
    
    base_n <- floor(pop)
    remainder <- pop - base_n
    n_people <- base_n + rbinom(1, 1, prob = remainder)
    if (n_people == 0) return(NULL)
    
    half <- cell_size / 2
    points <- tibble(
      grid_lon = runif(n_people, lon_center - half, lon_center + half),
      grid_lat = runif(n_people, lat_center - half, lat_center + half)
    )
    
    meta <- row %>%
      select(-grid_lon, -grid_lat, -all_of(pop_col)) %>%
      slice(rep(1, n_people))
    
    bind_cols(points, meta)
  }
  
  # Generate person-points from all rows
  people_points_df <- map_dfr(1:nrow(df), function(i) generate_people_points(df[i, ]))
  
  # Add indicator variables for relevant categorical variables
  relevant_vars <- c("race_ethnicity", "age_group", "sex", "income_decile")
  
  for (var in relevant_vars) {
    if (var %in% names(people_points_df)) {
      # Convert to character if needed (especially for numeric vars like income_decile)
      people_points_df[[var]] <- as.character(people_points_df[[var]])
      
      # Dynamically build formula and run model.matrix
      formula <- as.formula(paste0("~", var, " - 1"))
      dummies <- model.matrix(formula, data = people_points_df) %>%
        as_tibble() %>%
        rename_with(~ gsub(paste0("^", var), paste0(var, "_"), .x))  # e.g., race_ethnicityAsian → race_ethnicity_Asian
      
      people_points_df <- bind_cols(people_points_df, dummies)
    } else {
      message("Skipping indicator creation: column '", var, "' not found.")
    }
  }
  
  # Convert to sf
  people_points_sf <- st_as_sf(people_points_df, coords = c("grid_lon", "grid_lat"), crs = 4326, remove = FALSE)
  
  # Save as _point.rda
  output_path <- sub("\\.rda$", "_point.rda", file_path)
  save(people_points_sf, file = output_path)
  message("Saved point-level sf object to: ", output_path)
}

#### Apply Functions to Data Files ####

# List of folders to search for input .rda files
folders <- c(processed_path, rda_path_ars, rda_path_ri)

# Get all .rda file paths from those folders
all_rda_files <- unlist(lapply(folders, function(dir) {
  list.files(dir, pattern = "\\.rda$", full.names = TRUE)
}))

file_paths <- all_rda_files[!grepl("_hex\\.rda$", all_rda_files) & !grepl("^nat_", basename(all_rda_files)) & !grepl("_point\\.rda$", all_rda_files) & !grepl("_with_hex_id\\.rda$", all_rda_files)]

# Run the processing function on each file
walk(file_paths, generate_people_points_from_file)

