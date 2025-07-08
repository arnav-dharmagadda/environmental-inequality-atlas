################################################################################
# FILE: 04_map.R
# PURPOSE: Create hexagon map of PM2.5 exposure.
# AUTHOR: Arnav Dharmagadda
# CREATED: July 8th, 2025
################################################################################
# INPUTS: 
# OUTPUTS: None.
################################################################################

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------------------------

#' Create rectangular grid from point data
#' 
#' @param file_path Path to .rda file containing point data
#' @param lon_col Name of longitude column
#' @param lat_col Name of latitude column
#' @param cellsize Grid cell size in degrees
#' @param filter_to_counties Whether to filter to Charlottesville/Albemarle
#' @return Saves gridded data back to the same .rda file
process_rda_to_grid <- function(file_path, lon_col = "lon", lat_col = "lat", 
                                cellsize = 0.01, filter_to_counties = TRUE) {
  
  # Load data
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]
  df <- env[[df_name]]
  
  # Validate input
  if (nrow(df) == 0) {
    stop("Input data frame is empty: ", file_path)
  }
  
  # Convert to spatial points
  df_sf <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  if (nrow(df_sf) == 0) {
    stop("No valid points after spatial conversion. Check coordinate columns.")
  }
  
  # Create grid
  bbox <- st_bbox(df_sf)
  bbox_expanded <- bbox
  bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - cellsize / 2
  bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + cellsize / 2
  
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
  
  if (length(grid_polygons) == 0) {
    stop("Grid creation failed. Check bounding box and cell size.")
  }
  
  grid_sf <- st_sf(geometry = grid_polygons)
  
  # Filter to study area if requested
  if (filter_to_counties && all(c("COUNTYFP", "STATEFP") %in% colnames(df))) {
    county_points <- df %>%
      filter(STATEFP == "51" & COUNTYFP %in% c("003", "540")) %>%
      select(all_of(c(lon_col, lat_col))) %>%
      distinct()
    
    if (nrow(county_points) > 0) {
      county_points_sf <- st_as_sf(county_points, coords = c(lon_col, lat_col), crs = 4326)
      county_boundary <- st_convex_hull(st_union(county_points_sf))
      grid_sf <- st_filter(grid_sf, county_boundary, .predicate = st_intersects)
      
      if (nrow(grid_sf) == 0) {
        stop("All grid cells filtered out by county boundary.")
      }
      message("Filtered grid to ", nrow(grid_sf), " cells within study area")
    }
  }
  
  # Spatial join and clean data
  df_gridded <- st_join(grid_sf, df_sf, join = st_contains)
  
  # Remove empty cells for PM2.5 data
  if ("PM25" %in% names(df_gridded)) {
    df_gridded <- df_gridded[!is.na(df_gridded$PM25), ]
  }
  
  # Clean numeric columns
  numeric_cols <- names(df_gridded)[sapply(df_gridded, is.numeric)]
  coord_cols <- c("grid_lon", "grid_lat", lon_col, lat_col)
  clean_cols <- setdiff(numeric_cols, coord_cols)
  
  for (col in clean_cols) {
    df_gridded[[col]] <- ifelse(is.na(df_gridded[[col]]), 0, df_gridded[[col]])
  }
  
  # Save results
  assign(df_name, df_gridded, envir = env)
  save(list = df_name, file = file_path, envir = env)
  
  message("Grid processing complete: ", nrow(df_gridded), " cells with data")
}

# ------------------------------------------------------------------------------
# DATA LOADING AND PREPROCESSING
# ------------------------------------------------------------------------------

# File paths
nc_file_path <- paste0(data_path, "V6GL02.03.CNNPM25.NA.202301-202312.nc")
shapefile_cd_path <- paste0(data_path, "cb_2024_us_county_500k/cb_2024_us_county_500k.shp")

# Extract PM2.5 data from NetCDF
message("Loading PM2.5 data from NetCDF...")
nc <- nc_open(nc_file_path)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
pm25 <- ncvar_get(nc, "PM25")
nc_close(nc)

# Subset to study area (Charlottesville/Albemarle bounding box)
lat_idx <- which(lat >= 37.6 & lat <= 38.6)
lon_idx <- which(lon >= -78.9 & lon <= -78.0)

# Handle 3D data (take first time slice if needed)
if (length(dim(pm25)) == 3) {
  pm25 <- pm25[lon_idx, lat_idx, 1]
} else {
  pm25 <- pm25[lon_idx, lat_idx]
}

# Create point grid
grid <- expand.grid(lon = lon[lon_idx], lat = lat[lat_idx])
grid$PM25 <- as.vector(pm25)
grid <- grid[!is.na(grid$PM25), ]  # Remove missing values

# Convert to spatial points
pm25_points <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Load county boundaries
message("Loading county boundaries...")
counties <- st_read(shapefile_cd_path, quiet = TRUE)
if (st_crs(counties) != st_crs(pm25_points)) {
  counties <- st_transform(counties, st_crs(pm25_points))
}

# Filter to study counties
pm25_with_county <- st_join(pm25_points, counties, join = st_within) %>%
  filter(STATEFP == "51" & COUNTYFP %in% c("003", "540"))

message("Loaded ", nrow(pm25_with_county), " PM2.5 observations in study area")

# ------------------------------------------------------------------------------
# GRID PROCESSING
# ------------------------------------------------------------------------------

# Save point data for grid processing
message("Creating rectangular grid...")
pm25_df <- st_drop_geometry(pm25_with_county)
tmp_pm25_rda <- tempfile(fileext = ".rda")
pm25_with_county_df <- pm25_df
save(pm25_with_county_df, file = tmp_pm25_rda)

# Apply grid processing
process_rda_to_grid(tmp_pm25_rda, lon_col = "lon", lat_col = "lat", 
                   cellsize = 0.01, filter_to_counties = TRUE)

# Load gridded results
load(tmp_pm25_rda)
pm25_grid <- pm25_with_county_df

# ------------------------------------------------------------------------------
# HEXAGONAL OVERLAY CREATION
# ------------------------------------------------------------------------------

message("Creating hexagonal overlay...")

# Convert rectangular grid to raster
pm25_raster <- st_rasterize(pm25_grid["PM25"], dx = 0.01, dy = 0.01)

# Create hexagonal grid overlay
hex_grid <- st_make_grid(pm25_grid, cellsize = 0.015, square = FALSE)
hex_grid_sf <- st_sf(geometry = hex_grid) %>%
  mutate(hex_id = row_number())

# Sample raster values at hexagon centroids
hex_centroids <- st_centroid(hex_grid_sf)
hex_values <- st_extract(pm25_raster, hex_centroids)
hex_grid_sf$PM25 <- hex_values$PM25

# Remove hexagons with no data
hex_grid_sf <- hex_grid_sf[!is.na(hex_grid_sf$PM25), ]

message("Created ", nrow(hex_grid_sf), " hexagons with PM2.5 data")

# Save PM2.5 point data with county info for reproducibility
file_path <- paste0(map_data, "pm25_with_county.rda")
save(pm25_with_county, hex_grid_sf, file = file_path)

# ------------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------------

# Set up publication-quality map theme
tmap_mode("plot")

# Create point map for comparison
point_map <- tm_shape(pm25_with_county) +
  tm_dots(
    col = "PM25", 
    palette = "inferno", 
    style = "cont", 
    size = 0.05, 
    title = "PM2.5 (μg/m³)"
  ) +
  tm_shape(counties) +
  tm_borders(lwd = 1, col = "black") +
  tm_layout(
    legend.outside = TRUE,
    fontfamily = "Lato"
  )

# Create hexagonal map with publication styling
hex_map <- tm_basemap("CartoDB.Positron", alpha = 1) +
  tm_shape(hex_grid_sf) +
  tm_fill(
    col = "PM25",
    title = "PM2.5 (μg/m³)",
    style = "cont",
    palette = "inferno",
    fill_alpha = 0.9,
    colorNA = "transparent",
    textNA = "",
    lwd = 0
  ) +
  tm_borders(
    col = "white",
    lwd = 0.3
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.3
  ) +
  tm_layout(
    main.title.size = 0.8,
    main.title.fontfamily = "Lato",
    legend.outside = TRUE,
    legend.text.size = 0.6,
    legend.title.size = 0.7,
    legend.text.fontfamily = "Lato",
    legend.title.fontfamily = "Lato",
    fontfamily = "Lato",
    bg.color = "transparent",
    outer.bg.color = "transparent"
  )

# Display the hexagonal map
print(hex_map)
