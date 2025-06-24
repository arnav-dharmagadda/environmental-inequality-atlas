################################################################################
# FILE: 01_map.R
# PURPOSE: Creates the map for the overview page of the brief.
# files.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: 
# OUTPUTS: total_pop_map.jpeg
################################################################################

#### Load Packages ####

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(sf, terra, dplyr, ggplot2, tmap, arrow, dplyr, tidyr, scales, haven, stringr, purrr, forcats)


#### Setting working directory and file paths ####

setwd("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/")

rda_path_ri <- "data/processed/raceincome_rda/"

output_path <- "output/overview/"

#### Import data and clean ####

file_path <- paste0(rda_path_ri, "raceincome_2023_point_hex.rda")
load(file_path)
data_2023_hex <- people_points_sf

file_path <- paste0(rda_path_ri, "raceincome_2023_point.rda")
load(file_path)

file_path <- paste0(rda_path_ri, "raceincome_2023.rda")
load(file_path)
data_2023 <- merged_year

file_path <- paste0(rda_path_ri, "raceincome_long.rda")
load(file_path)

# Convert income_decile to numeric
people_points_sf <- people_points_sf %>%
  mutate(income_decile = as.numeric(income_decile),
         total = 1)

# Collapse by year for time series

total_aland <- data_2023 %>%
  distinct(COUNTYFP, ALAND) %>%
  summarise(total_ALAND = sum(ALAND, na.rm = TRUE))

raceincome_combined <- raceincome_combined %>%
  group_by(year) %>%
  summarise(n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE)) %>%
  mutate(total_ALAND = total_aland$total_ALAND)

data_2023_hex <- data_2023_hex %>%
  mutate(across(starts_with("income_decile_"), as.numeric)) %>%
  mutate(
    total = rowSums(
      select(., starts_with("income_decile_")) %>%
        mutate(across(everything(), as.numeric)),
      na.rm = TRUE
    )
  )

grid_2023_total <- data_2023 %>%
  group_by(grid_lon, grid_lat) %>%
  summarise(total = ifelse(all(is.na(n_noise_postprocessed)), 
                          NA, 
                          sum(n_noise_postprocessed, na.rm = TRUE)), 
            .groups = "keep")

#### Generate map ####

# Plot with a gradient color scale
tm_basemap("OpenStreetMap", alpha = 0.7) +  # Added alpha for basemap transparency (0.1-1.0)
  tm_shape(people_points_sf) +
  tm_dots(
    col = "#232d4b",
    title = "Population",
    colorNA = "transparent",
    alpha = 0.8,  # Increased dot alpha since basemap is more transparent
    size = 0.01  # adjust for visibility
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_add_legend(
    type = "symbol",
    labels = "Individual",
    col = "#232d4b",
    size = 0.2  # Adjust as needed to match tm_dots size
    #title = "Legend"
  ) +
  tm_layout(
    main.title = "Charlottesville Population (2023)", 
    #main.title.position = "left",         # left-aligned like ggplot
    main.title.size = 1.6,                # ~18 pt equivalent in tmap
    main.title.fontface = "bold",         # match bold styling
    legend.outside = FALSE,
    legend.position = c("right", "bottom"),
    bg.color = "transparent",
    outer.bg.color = "transparent"
  )

file_path <- paste0(output_path, "total_pop_map.png")

tmap_save(filename = file_path, bg = NA, width = 4, height = 5)

# Test with hexagon

tm_basemap("OpenStreetMap", alpha = 0.7) +  # Added alpha for basemap transparency (0.1-1.0)
  tm_shape(data_2023_hex) +
  tm_polygons(
    col = "total",                         # Color by a variable (or use a fixed color)
    palette = "Blues",                     # Color palette for the polygons
    title = "Population",
    alpha = 0.7,                          # Polygon transparency
    border.col = "white",                  # Border color
    border.lwd = 0.2,                     # Border line width
    colorNA = "transparent"
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_add_legend(
    type = "fill",                        # Changed from "symbol" to "fill" for polygons
    labels = "Hexagon Area",              # Updated label
    col = "lightblue",                    # Color to match polygon style
    alpha = 0.7                          # Match polygon transparency
    #title = "Legend"
  ) +
  tm_layout(
    main.title = "Charlottesville Population (2023)", 
    #main.title.position = "left",         # left-aligned like ggplot
    main.title.size = 1.6,                # ~18 pt equivalent in tmap
    main.title.fontface = "bold",         # match bold styling
    legend.outside = FALSE,
    legend.position = c("right", "bottom"),
    bg.color = "transparent",
    outer.bg.color = "transparent"
  )

# Test with Grid

tm_basemap("OpenStreetMap", alpha = 0.7) +  # Added alpha for basemap transparency (0.1-1.0)
  tm_shape(grid_2023_total) +
  tm_polygons(
    col = "total",                         # Color by a variable (or use a fixed color)
    palette = "Blues",                     # Color palette for the polygons
    title = "Population",
    alpha = 0.7,                          # Polygon transparency
    border.col = "white",                  # Border color
    border.lwd = 0.2,                     # Border line width
    colorNA = "transparent"
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_add_legend(
    type = "fill",                        # Changed from "symbol" to "fill" for polygons
    labels = "Hexagon Area",              # Updated label
    col = "lightblue",                    # Color to match polygon style
    alpha = 0.7                          # Match polygon transparency
    #title = "Legend"
  ) +
  tm_layout(
    main.title = "Charlottesville Population (2023)", 
    #main.title.position = "left",         # left-aligned like ggplot
    main.title.size = 1.6,                # ~18 pt equivalent in tmap
    main.title.fontface = "bold",         # match bold styling
    legend.outside = FALSE,
    legend.position = c("right", "bottom"),
    bg.color = "transparent",
    outer.bg.color = "transparent"
  )

#### Generate time series ####

ggplot(raceincome_combined, aes(x = year, y = n_noise_postprocessed)) +
  geom_area(fill = "#232d4b", alpha = 0.2) +
  geom_line(color = "#232d4b", size = 1.2) +
  geom_point(color = "#232d4b", size = 2) +
  labs(
    title = "Charlottesville and Albemarle County Population (1999–2023)",
    x = "Year",
    y = "Population"
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 20)
  )

file_path <- paste0(output_path, "pop_time_series.png")

ggsave(
  filename = file_path,
  plot = last_plot(),        # or specify your plot object if named
  width = 8,                 # in inches
  height = 5,                # adjust as needed
  dpi = 300,                 # high resolution
  bg = NA               # or "transparent" if needed
)

#### Calculate average growth rate ####

# Get population values
pop_vals <- raceincome_combined %>%
  filter(year %in% c(1999, 2023)) %>%
  arrange(year)

# Calculate CAGR
pop_initial <- pop_vals$n_noise_postprocessed[1]
pop_final <- pop_vals$n_noise_postprocessed[2]
n_years <- diff(pop_vals$year)

cagr <- (pop_final / pop_initial)^(1 / n_years) - 1

# Print as percent
cat("Average annual growth rate (CAGR):", scales::percent(cagr, accuracy = 0.01), "\n")

# Print absolute population change
cat("Population change from 1999 to 2023:", format(round(pop_final - pop_initial), big.mark = ","), "\n")

#### Calculate population density ####

pop_density_2023 <- raceincome_combined %>%
  filter(year == 2023)

# Compute population density (people per square kilometer)
pop_density_2023 <- pop_density_2023$n_noise_postprocessed / (pop_density_2023$total_ALAND / 1e6)

# Print result
cat("Population density in 2023:", round(pop_density_2023, 2), "people/km²\n")

#### Horizontal Bar Chart ####

data_plot <- data_2023 %>%
  filter(!is.na(n_noise_postprocessed)) %>%
  mutate(COUNTYFP = fct_reorder(NAME, n_noise_postprocessed))

ggplot(data_plot, aes(x = NAME, y = n_noise_postprocessed)) +
  geom_col(fill = "#f7f5dc") +
  coord_flip() +
  labs(
    x = "County",
    y = "Population",
    title = "Population by County (2023)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 11)
  ) +
  scale_y_continuous(labels = scales::comma)

file_path <- paste0(output_path, "pop_by_county.png")

ggsave(
  filename = file_path,
  plot = last_plot(),        # or specify your plot object if named
  width = 8,                 # in inches
  height = 5,                # adjust as needed
  dpi = 300,                 # high resolution
  bg = NA               # or "transparent" if needed
)
