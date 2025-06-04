################################################################################
# FILE: 02_maps.R
# PURPOSE: This script generates maps of gridded EIF population data.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: 
# OUTPUTS: 
################################################################################

tm_shape(ageracesex_2020) +
  tm_polygons(col = "Hispanic", palette = "viridis",
              title = "Hispanic Population", lwd = 0,
              style = "cont", breaks = seq(0, 300, 50),
              lengend.hist = TRUE) +
  tm_layout(
    main.title = "Gridded EIF Hispanic Population (2020)",  # ← key change
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),  # top, right, bottom, left
    legend.outside = TRUE
  )

tm_shape(ageracesex_2020) +
  tm_polygons(col = "Black", palette = "viridis",
              title = "Black Population", lwd = 0,
              style = "cont", breaks = seq(0, 500, 50),
              lengend.hist = TRUE) +
  tm_layout(
    main.title = "Gridded EIF Black Population (2020)",  # ← key change
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),  # top, right, bottom, left
    legend.outside = TRUE
  )

tm_shape(ageracesex_2020) +
  tm_polygons(col = "White", palette = "viridis",
              title = "White Population", lwd = 0) +
  tm_layout(
    main.title = "Gridded EIF White Population (2020)",  # ← key change
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),  # top, right, bottom, left
    legend.outside = TRUE
  )

tm_shape(ageracesex_2020) +
  tm_polygons(col = "Asian", palette = "viridis",
              title = "Asian Population", lwd = 0) +
  tm_layout(
    main.title = "Gridded EIF Asian Population (2020)",  # ← key change
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),  # top, right, bottom, left
    legend.outside = TRUE
  )

tm_shape(ageracesex_2020) +
  tm_polygons(col = "AIAN", palette = "viridis",
              title = "Native American Population", lwd = 0) +
  tm_layout(
    main.title = "Gridded EIF Native American Population (2020)",  # ← key change
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),  # top, right, bottom, left
    legend.outside = TRUE
  )

tm_shape(ageracesex_2020) +
  tm_polygons(col = "Other/Unknown", palette = "viridis",
              title = "Other/Unknown Race", lwd = 0) +
  tm_layout(
    main.title = "Gridded EIF Other/Unknown Race Population (2020)",  # ← key change
    main.title.position = "center",
    outer.margins = c(0.05, 0, 0, 0),  # top, right, bottom, left
    legend.outside = TRUE
  )
