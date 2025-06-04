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

tm_basemap("CartoDB.Positron") +
  tm_shape(race_2023) +
  tm_polygons(col = "total", palette = "-viridis",
              title = "Total Population", lwd = 0,
              style = "cont",
              lengend.hist = TRUE,
              colorNA = "transparent",
              alpha = 0.6) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, circa 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )


#### RACE MAPS ####

# Hispanic Population Share

tm_basemap("CartoDB.Positron") +
tm_shape(race_2023) +
  tm_polygons(col = "hispanic_share", palette = "-viridis",
              title = "Hispanic Population Share", lwd = 0,
              style = "cont",
              lengend.hist = TRUE,
              colorNA = "transparent",
              alpha = 0.6) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, circa 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )

# White Population Share

tm_basemap("CartoDB.Positron") +
  tm_shape(race_2023) +
  tm_polygons(col = "white_share", palette = "-viridis",
              title = "White Population Share", lwd = 0,
              style = "cont",
              lengend.hist = TRUE,
              colorNA = "transparent",
              alpha = 0.6) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, circa 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )

# Black Population

tm_basemap("CartoDB.Positron") +
  tm_shape(race_2023) +
  tm_polygons(col = "black_share", palette = "-viridis",
              title = "Black Population Share", lwd = 0,
              style = "cont",
              lengend.hist = TRUE,
              colorNA = "transparent",
              alpha = 0.6) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, circa 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )

#### SEX MAPS ####

# Male

tm_basemap("CartoDB.Positron") +
  tm_shape(sex_2023) +
  tm_polygons(col = "male_share", palette = "-viridis",
              title = "Male Population Share", lwd = 0,
              style = "cont",
              lengend.hist = TRUE,
              colorNA = "transparent",
              alpha = 0.6) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville, circa 2023", 
    main.title.position = "center",
    legend.outside = TRUE
  )

