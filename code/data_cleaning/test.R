library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)



# Your example input (confirmed correct)
base_2023 <- tibble(
  grid_lon = c(-78.5, -78.49),
  grid_lat = c(38.03, 38.04),
  population = c(10, 20),
  category = c("A", "B")
)

load("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/ageracesex_rda/ageracesex_2023_point.rda")
base_2023 <- people_points_sf


ggplot(base_2023, aes(x = grid_lon, y = grid_lat, color = race_ethnicity)) +
  geom_point(size = .002, alpha = 0.6) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Exploded Grid Points", color = "Race/Ethnicity") +
  scale_color_manual(
    values = c(
      "White" = "#1f77b4",         # blue
      "Black" = "#ff7f0e",         # orange
      "Hispanic" = "#2ca02c",      # green
      "Asian" = "#d62728",         # red
      "AIAN" = "#9467bd",          # purple
      "Other/Unknown" = "#7f7f7f"  # gray
    )
  )

tm_shape(base_2023) +
  tm_dots(
    col = "race_ethnicity",
    size = 0.001,           # tmap uses relative sizes
    alpha = 0.6,
    palette = c(
      "White" = "#1f77b4",         
      "Black" = "#ff7f0e",         
      "Hispanic" = "#2ca02c",      
      "Asian" = "#d62728",         
      "AIAN" = "#9467bd",          
      "Other/Unknown" = "#7f7f7f"  
    ),
    title = "Race/Ethnicity"
  ) +
  tm_layout(
    title = "Exploded Grid Points",
    legend.outside = TRUE,
    frame = FALSE
  )

load("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/raceincome_rda/raceincome_2023_point.rda")
points <- people_points_sf

# Convert income_decile to numeric
points <- points %>%
  mutate(income_decile = as.numeric(income_decile))

# Plot with a gradient color scale
tm_basemap("CartoDB.Positron") +
  tm_shape(points) +
  tm_dots(
    col = "income_decile",
    palette = "viridis",
    title = "Average Income Decile",
    style = "cont",
    legend.hist = FALSE,
    colorNA = "transparent",
    alpha = 0.6,
    size = 0.01  # adjust for visibility
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )

load("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/raceincome_rda/raceincome_2023_point_hex.rda")
hex <- people_points_sf

# Convert income_decile to numeric
hex <- hex %>%
  mutate(
    total_income_count = rowSums(across(starts_with("income_decile_")), na.rm = TRUE),
    avg_income_decile = rowSums(
      across(starts_with("income_decile_"), ~ . * as.numeric(gsub("income_decile_", "", cur_column()))),
      na.rm = TRUE
    ) / total_income_count
  )

# Plot with a gradient color scale
tm_basemap("CartoDB.Positron") +
  tm_shape(hex) +
  tm_polygons(col = "avg_income_decile", palette = "viridis",
              title = "Average Income Decile",
              style = "cont",
              legend.hist = FALSE,
              colorNA = "transparent",
              alpha = 0.6,
              border.col = "white",
              lwd = 1) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )

load("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/raceincome_rda/raceincome_long_year.rda")
fishnet <- raceincome_year_long

fishnet <- fishnet %>%
  filter(year == 2023) %>%
  mutate(
    total_income_count = rowSums(across(starts_with("inc_")), na.rm = TRUE),
    avg_income_decile = rowSums(
      across(starts_with("inc_"), ~ . * as.numeric(gsub("inc_", "", cur_column()))),
      na.rm = TRUE
    ) / total_income_count
  )

# Plot with a gradient color scale
tm_basemap("CartoDB.Positron") +
  tm_shape(fishnet) +
  tm_polygons(col = "avg_income_decile", palette = "viridis",
              title = "Average Income Decile",
              style = "cont",
              legend.hist = FALSE,
              colorNA = "transparent",
              alpha = 0.6,
              border.col = "white",
              lwd = 0.5) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )

#### LAYERED MAPS ####

tm_basemap("CartoDB.Positron") +
  tm_shape(fishnet) +
  tm_polygons(col = "avg_income_decile", palette = "viridis",
              title = "Average Income Decile",
              style = "cont",
              legend.hist = FALSE,
              colorNA = "transparent",
              alpha = 0.7,
              border.col = "white",
              lwd = 0.5) +
  tm_shape(points) +
  tm_dots(
    alpha = 0.1,
    size = 0.01  # adjust for visibility
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )

tm_basemap("CartoDB.Positron") +
  tm_shape(hex) +
  tm_polygons(col = "avg_income_decile", palette = "viridis",
              title = "Average Income Decile",
              style = "cont",
              legend.hist = FALSE,
              colorNA = "transparent",
              alpha = 0.7,
              border.col = "white",
              lwd = 0.5) +
  tm_shape(points) +
  tm_dots(
    alpha = 0.1,
    size = 0.01  # adjust for visibility
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )


