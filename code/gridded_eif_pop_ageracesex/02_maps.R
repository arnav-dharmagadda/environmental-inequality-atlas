################################################################################
# FILE: 02_maps.R
# PURPOSE: This script generates maps of gridded EIF population data.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: 
# OUTPUTS: 
################################################################################

#### RACE MAPS ####

# Hispanic Population

tm_shape(race_2023) +
  tm_polygons(col = "Hispanic", palette = "viridis",
              title = "Hispanic Population", lwd = 0,
              style = "cont",
              lengend.hist = TRUE) +
  tm_layout(
    main.title = "Charlottesville, circa 2023", 
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),
    legend.outside = TRUE
  )

# White Population

tm_shape(race_2023) +
  tm_polygons(col = "White", palette = "viridis",
              title = "White Population", lwd = 0,
              style = "cont",
              lengend.hist = TRUE) +
  tm_layout(
    main.title = "Charlottesville, circa 2023", 
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),
    legend.outside = TRUE
  )

# Black Population

tm_shape(race_2023) +
  tm_polygons(col = "Black", palette = "viridis",
              title = "Black Population", lwd = 0,
              style = "cont",
              lengend.hist = TRUE) +
  tm_layout(
    main.title = "Charlottesville, circa 2023", 
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),
    legend.outside = TRUE
  )
