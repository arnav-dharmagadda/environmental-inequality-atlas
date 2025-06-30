################################################################################
# FILE: 00_all_gridpoints.R
# PURPOSE: Import NetCDF file and extract latitude and longitude gridpoints
# AUTHOR: Arnav Dharmagadda
# CREATED: June 24, 2025
################################################################################
# INPUTS: NetCDF file with grid data, shapefile for US counties
# OUTPUTS: RDA file with gridpoints and county information
################################################################################

#### Set File Paths ####

# NetCDF file path
nc_file_path <- paste0(data_path, "V6GL02.03.CNNPM25.NA.202301-202312.nc")

# Shapefile path
shapefile_cd_path <- paste0(data_path, "cb_2024_us_cd119_500k/cb_2024_us_cd119_500k.shp")

# Output path for gridpoints
gridpoints_output_path <- paste0(data_path, "gridpoints_with_county_2024.rda")

#### Import NetCDF File and Extract Gridpoints ####

# Open the NetCDF file
nc_data <- nc_open(nc_file_path)

# Print information about the NetCDF file structure
print(nc_data)

# Get the variable names
var_names <- names(nc_data$var)
dim_names <- names(nc_data$dim)

print("Available variables:")
print(var_names)
print("Available dimensions:")
print(dim_names)

# Extract latitude and longitude - try different approaches
# First, try to get them as variables
latitude <- NULL
longitude <- NULL

# Try common variable names
lat_var_names <- c("lat", "latitude", "LAT", "XLAT", "y")
lon_var_names <- c("lon", "longitude", "LON", "XLONG", "x")

for (var_name in lat_var_names) {
  if (var_name %in% var_names) {
    latitude <- ncvar_get(nc_data, var_name)
    cat("Found latitude variable:", var_name, "\n")
    break
  }
}

for (var_name in lon_var_names) {
  if (var_name %in% var_names) {
    longitude <- ncvar_get(nc_data, var_name)
    cat("Found longitude variable:", var_name, "\n")
    break
  }
}

# If not found as variables, try to get them from dimensions
if (is.null(latitude)) {
  for (dim_name in lat_var_names) {
    if (dim_name %in% dim_names) {
      latitude <- nc_data$dim[[dim_name]]$vals
      cat("Found latitude dimension:", dim_name, "\n")
      break
    }
  }
}

if (is.null(longitude)) {
  for (dim_name in lon_var_names) {
    if (dim_name %in% dim_names) {
      longitude <- nc_data$dim[[dim_name]]$vals
      cat("Found longitude dimension:", dim_name, "\n")
      break
    }
  }
}

# Check what we found
cat("Latitude structure:\n")
cat("  Class:", class(latitude), "\n")
cat("  Dimensions:", dim(latitude), "\n")
cat("  Length:", length(latitude), "\n")
cat("  Range:", range(latitude, na.rm = TRUE), "\n")

cat("Longitude structure:\n")
cat("  Class:", class(longitude), "\n")
cat("  Dimensions:", dim(longitude), "\n")
cat("  Length:", length(longitude), "\n")
cat("  Range:", range(longitude, na.rm = TRUE), "\n")

# Close the NetCDF file
nc_close(nc_data)

#### Create Gridpoints DataFrame ####

if (is.null(latitude) || is.null(longitude)) {
  stop("Could not find latitude or longitude variables/dimensions in the NetCDF file")
}

# Handle different data structures
if (is.vector(latitude) && is.vector(longitude)) {
  # Check if they have the same length (already paired coordinates)
  if (length(latitude) == length(longitude)) {
    cat("Creating grid from paired lat/lon vectors of equal length\n")
    gridpoints <- data.frame(
      grid_lat = latitude,
      grid_lon = longitude
    )
  } else {
    # Different lengths - create all combinations (Cartesian product)
    cat("Creating grid from 1D lat/lon vectors (Cartesian product)\n")
    cat("Latitude points:", length(latitude), "Longitude points:", length(longitude), "\n")
    cat("This will create", length(latitude) * length(longitude), "gridpoints\n")
    gridpoints <- expand.grid(
      grid_lat = latitude,
      grid_lon = longitude
    )
  }
} else if (is.matrix(latitude) && is.matrix(longitude)) {
  # 2D arrays - should have same dimensions
  if (!identical(dim(latitude), dim(longitude))) {
    stop("Latitude and longitude matrices have different dimensions")
  }
  cat("Creating grid from 2D lat/lon matrices\n")
  gridpoints <- data.frame(
    grid_lat = as.vector(latitude),
    grid_lon = as.vector(longitude)
  )
} else if (is.array(latitude) && is.array(longitude)) {
  # Multi-dimensional arrays - check if same dimensions
  if (!identical(dim(latitude), dim(longitude))) {
    # Different dimensions - treat as separate coordinate arrays
    cat("Multi-dimensional arrays with different dimensions - creating Cartesian product\n")
    gridpoints <- expand.grid(
      grid_lat = as.vector(latitude),
      grid_lon = as.vector(longitude)
    )
  } else {
    # Same dimensions - flatten them
    cat("Creating grid from multi-dimensional lat/lon arrays\n")
    gridpoints <- data.frame(
      grid_lat = as.vector(latitude),
      grid_lon = as.vector(longitude)
    )
  }
} else {
  # Handle mixed types or other cases
  cat("Converting to vectors and creating grid\n")
  lat_vec <- as.vector(latitude)
  lon_vec <- as.vector(longitude)
  
  if (length(lat_vec) == length(lon_vec)) {
    gridpoints <- data.frame(
      grid_lat = lat_vec,
      grid_lon = lon_vec
    )
  } else {
    gridpoints <- expand.grid(
      grid_lat = lat_vec,
      grid_lon = lon_vec
    )
  }
}

#### Display Results ####

# Force conversion to numeric, handling any character data
gridpoints$grid_lat <- as.numeric(gridpoints$grid_lat)
gridpoints$grid_lon <- as.numeric(gridpoints$grid_lon)

# Remove any rows with missing coordinates
gridpoints <- gridpoints[complete.cases(gridpoints), ]

# Clean up the data.frame - remove any dimensional attributes and ensure it's a simple data.frame
cat("Before cleanup - attributes:", names(attributes(gridpoints)), "\n")
gridpoints <- data.frame(
  grid_lat = as.numeric(gridpoints$grid_lat),
  grid_lon = as.numeric(gridpoints$grid_lon),
  stringsAsFactors = FALSE
)
cat("After cleanup - attributes:", names(attributes(gridpoints)), "\n")

# Show the structure of the resulting data
str(gridpoints)
head(gridpoints)

# Summary statistics
cat("\nLatitude range:", range(gridpoints$grid_lat), "\n")
cat("Longitude range:", range(gridpoints$grid_lon), "\n")
cat("Total gridpoints:", nrow(gridpoints), "\n")

# Performance warning and option to subset for testing
if (nrow(gridpoints) > 1000000) {
  cat("\nWARNING: Large dataset detected (", nrow(gridpoints), "points)\n")
  cat("Consider subsetting for initial testing. Uncomment the line below for a sample:\n")
  cat("# gridpoints <- gridpoints[sample(nrow(gridpoints), 100000), ]  # Sample 100k points\n")
  
  # Uncomment this line to work with a smaller sample for testing:
  # gridpoints <- gridpoints[sample(nrow(gridpoints), 100000), ]
  
  # Or use geographic filter for study area (Charlottesville/Albemarle region):
  gridpoints <- gridpoints %>%
    filter(
      #grid_lat >= 37.6 & grid_lat <= 38.6,
      #grid_lon >= -78.9 & grid_lon <= -78.0
    )
  cat("After geographic filtering:", nrow(gridpoints), "points remaining\n")
}

# Read both shapefiles
shapefile <- st_read(shapefile_cd_path)

# Performance optimization: Check if CRS transformation is actually needed
cat("Shapefile CRS:", st_crs(shapefile)$input, "\n")

# Create sf object with minimal overhead
cat("Creating sf object for", nrow(gridpoints), "gridpoints...\n")
gridpoints_sf <- st_as_sf(gridpoints, coords = c("grid_lon", "grid_lat"), crs = 4326, remove = FALSE)

# Only transform if necessary (most shapefiles are already in WGS84/4326)
if (st_crs(shapefile) != st_crs(4326)) {
  cat("CRS transformation needed - this may take a while for large datasets...\n")
  cat("Consider subsetting your data first if this takes too long.\n")
  gridpoints_sf <- st_transform(gridpoints_sf, st_crs(shapefile))
} else {
  cat("No CRS transformation needed - shapefile is already in WGS84\n")
}

# Spatial join
gridpoints_with_sf <- st_join(gridpoints_sf, shapefile, join = st_within)

df <- as.data.frame(gridpoints_with_sf)
df <- df[, !grepl("geometry", names(df))]

df$grid_lat <- round(df$grid_lat, 3)
df$grid_lon <- round(df$grid_lon, 3)

save(df, file = gridpoints_output_path)

