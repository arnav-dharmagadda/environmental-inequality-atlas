################################################################################
# FILE: 02_maps.R
# PURPOSE: This script generates maps of gridded EIF population data.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: 
# OUTPUTS: 
################################################################################

#### TOTAL POPULATION DISTRIBUTION ####

tmap_mode("plot")

total_pop_map <- 
  tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "total", palette = "-viridis",
              title = "Total Population",
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

total_pop_map

tmap_save(total_pop_map,
          filename = paste0(maps_output, "total_population_distribution_map.jpeg"))

#### CHARLOTTESVILLE INSET ####