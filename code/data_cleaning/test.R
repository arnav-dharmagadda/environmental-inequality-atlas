library(dplyr)
library(tidyr)
library(purrr)

load("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/ageracesex_rda/ageracesex_2023.rda")
base_2023 <- merged_year

# Your example input (confirmed correct)
base_2023 <- tibble(
  grid_lon = c(-78.5, -78.49),
  grid_lat = c(38.03, 38.04),
  population = c(10, 20),
  category = c("A", "B")
)

# Function that explodes a single row into 100 regular subpoints
explode_grid_cell <- function(row) {
  # Extract lon/lat
  lon_center <- as.numeric(row$grid_lon)
  lat_center <- as.numeric(row$grid_lat)
  
  # Define grid cell bounds
  cell_size <- 0.01
  half <- cell_size / 2
  xmin <- lon_center - half
  ymin <- lat_center - half
  
  # Generate 10x10 regular grid offsets within the cell
  n_side <- 10
  step <- cell_size / n_side
  offsets <- seq(step / 2, cell_size - step / 2, by = step)
  
  # Build 100 new coordinates
  grid_points <- expand.grid(x = offsets, y = offsets) %>%
    mutate(
      grid_lon = xmin + x,
      grid_lat = ymin + y
    ) %>%
    select(grid_lon, grid_lat)
  
  # Repeat the other attributes 100 times
  other_data <- row %>%
    select(-grid_lon, -grid_lat) %>%
    slice(rep(1, nrow(grid_points)))
  
  # Combine coordinates + attributes
  bind_cols(grid_points, other_data)
}

# Apply the function row-by-row (safe)
exploded_df <- bind_rows(lapply(1:nrow(base_2023), function(i) {
  explode_grid_cell(base_2023[i, ])
}))

ggplot(exploded_df, aes(x = grid_lon, y = grid_lat)) +
  geom_point(size = 0.00000003, alpha = 0.6) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Exploded Grid Points")
