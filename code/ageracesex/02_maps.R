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
  tm_shape(data_2023_hex) +
  tm_polygons(col = "total", palette = "-viridis",
              title = "Total Population",
              style = "cont",
              lengend.hist = TRUE,
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

ggsave("total_population_distribution_map.pdf", width = 10, height = 8)


#### RACE SHARE MAPS ####

# Hispanic Population Share

tm_basemap("CartoDB.Positron") +
tm_shape(data_2023_hex) +
  tm_polygons(col = "hispanic_share", palette = "-viridis",
              title = "Hispanic Population Share",
              style = "cont",
              lengend.hist = TRUE,
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

ggsave("hispanic_population_share_map.png", width = 10, height = 8)

# White Population Share

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "white_share", palette = "-viridis",
              title = "White Population Share",
              style = "cont",
              lengend.hist = TRUE,
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

# Black Population Share

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "black_share", palette = "-viridis",
              title = "Black Population Share",
              style = "cont",
              lengend.hist = TRUE,
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

#### RACE MAPS ####

# Hispanic Population

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "hispanic", palette = "-viridis",
              title = "Hispanic Population",
              style = "cont",
              lengend.hist = TRUE,
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

# White Population

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "white", palette = "-viridis",
              title = "White Population",
              style = "cont",
              lengend.hist = TRUE,
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

# Black Population

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "black", palette = "-viridis",
              title = "Black Population",
              style = "cont",
              lengend.hist = TRUE,
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

#### SEX MAPS ####

# Male

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "male_share", palette = "-viridis",
              title = "Male Population Share",
              style = "cont",
              lengend.hist = TRUE,
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

# Female 

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "female_share", palette = "-viridis",
              title = "Female Population Share",
              style = "cont",
              lengend.hist = TRUE,
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

#### AGE MAPS ####

# Under 18 Population

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "under_18", palette = "-viridis",
              title = "Under 18 Population",
              style = "cont",
              lengend.hist = TRUE,
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

# 19-65 Population

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "bet_19_65", palette = "-viridis",
              title = "19-65 Population",
              style = "cont",
              lengend.hist = TRUE,
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

# Over 65 Population

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "over_65", palette = "-viridis",
              title = "65+ Population",
              style = "cont",
              lengend.hist = TRUE,
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


# Under 18 Population Share

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "under_18_share", palette = "-viridis",
              title = "Under 18 Population Share",
              style = "cont",
              lengend.hist = TRUE,
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

# 19-65 Population Share

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "bet_19_65_share", palette = "-viridis",
              title = "19-65 Population Share",
              style = "cont",
              lengend.hist = TRUE,
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

# Over 65 Population Share

tm_basemap("CartoDB.Positron") +
  tm_shape(data_2023_hex) +
  tm_polygons(col = "over_65_share", palette = "-viridis",
              title = "65+ Population Share",
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
