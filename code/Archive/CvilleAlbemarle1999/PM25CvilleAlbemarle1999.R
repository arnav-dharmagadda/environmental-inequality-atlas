# Load required libraries
library(arrow)     # for reading parquet
library(dplyr)     # for data manipulation
library(sf)        # for spatial data
library(ggplot2)   # for plotting

# Set working directory
setwd("/Users/elizabethshiker/Dropbox/environmental-inequality-atlas/")

# Load CBSA gridpoints data
load("data/gridpoints_with_county_2020.rda")  # loads `df` into environment
county <- df  # rename for clarity

#IMPORTANT to note that using 2020 county boundaries with 1999 pollution data

# Load PM2.5 pollution data
pollution <- read_parquet("data/eif_pollutants/gridded_eif_pollutants_1999.parquet")

# Filter for Charlottesville (FIPS: 51540) and Albemarle County (FIPS: 51003)
county_filtered <- county %>%
  filter((STATEFP == "51" & COUNTYFP %in% c("540", "003")))

# Join county and pollution data using grid coordinates
# Note: pm25_grid_x and pm25_grid_y match grid_lon and grid_lat
merged <- county_filtered %>%
  inner_join(pollution, by = c("pm25_grid_x" = "grid_lon", "pm25_grid_y" = "grid_lat"))

# Convert to spatial object for mapping
merged_sf <- st_as_sf(merged, coords = c("pm25_grid_x", "pm25_grid_y"), crs = 4326)


# Plot PM2.5 data using ggplot2
ggplot() +
  geom_sf(data = merged_sf, aes(color = pm25_ACAG_gwr), size = 1.5) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "PM2.5 (µg/m³)") +
  theme_minimal() +
  labs(
    title = "PM2.5 Concentrations in Charlottesville and Albemarle County (1999)",
    subtitle = "Data from EIF Gridded Pollutants and CBSA Gridpoints",
    caption = "Source: ACAG GWR model"
  )