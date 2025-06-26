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

### Setting working directory and file paths ####

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

# Create separate data for each county from the original raceincome_combined
raceincome_by_county <- raceincome_combined %>%
  group_by(year, COUNTYFP, NAME) %>%
  summarise(n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop")

# Aggregate for total population calculations
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
  summarise(total = sum(n_noise_postprocessed, na.rm = TRUE), 
            .groups = "keep")

#### Generate map ####

# Plot with a gradient color scale
tm_basemap("OpenStreetMap", alpha = 1) +  # Added alpha for basemap transparency (0.1-1.0)
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

file_path <- paste0(output_path, "total_pop_point_map.png")

tmap_save(filename = file_path, bg = NA, width = 6, height = 8)

# Test with hexagon

tm_basemap("OpenStreetMap", alpha = 1) +  # Added alpha for basemap transparency (0.1-1.0)
  tm_shape(data_2023_hex) +
  tm_polygons(
    col = "total",                         # Color by a variable (or use a fixed color)
    palette = "Blues",                     # Color palette for the polygons
    title = "Population",
    style = "cont",                        # Continuous gradient style
    alpha = 0.7,                          # Polygon transparency
    border.col = "white",                  # Border color
    border.lwd = 0.2,                     # Border line width
    colorNA = "transparent",
    textNA = ""                           # Remove "missing" text from legend
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville Population (2023)", 
    #main.title.position = "left",         # left-aligned like ggplot
    main.title.size = 1.6,                # ~18 pt equivalent in tmap
    main.title.fontface = "bold",         # match bold styling
    legend.outside = FALSE,
    legend.position = c("right", "bottom"),
    legend.text.size = 0.4,               # Larger text for readability
    legend.title.size = 1.0,              # Normal size title
    bg.color = "transparent",
    outer.bg.color = "transparent"
  )

file_path <- paste0(output_path, "total_pop_hex_map.png")

tmap_save(filename = file_path, bg = NA, width = 6, height = 8)

# Test with Grid

tm_basemap("OpenStreetMap", alpha = 1) +  # Added alpha for basemap transparency (0.1-1.0)
  tm_shape(grid_2023_total) +
  tm_polygons(
    col = "total",                         # Color by a variable (or use a fixed color)
    palette = "Blues",                     # Color palette for the polygons
    title = "Population",
    alpha = 0.7, 
    style = "cont",                        # Continuous gradient style
    border.col = "white",                  # Border color
    border.lwd = 0.2,                     # Border line width
    colorNA = "transparent",
    textNA = ""                           # Remove "missing" text from legend
  ) +
  tm_graticules(
    n.x = 5,
    n.y = 5,
    col = "gray70",
    lwd = 0.3,
    labels.size = 0.8
  ) +
  tm_layout(
    main.title = "Charlottesville Population (2023)", 
    main.title.size = 1.6,                # ~18 pt equivalent in tmap
    main.title.fontface = "bold",         # match bold styling
    legend.outside = FALSE,
    legend.position = c("right", "bottom"),
    legend.text.size = 0.4,               # Larger text for readability
    legend.title.size = 1.0,              # Normal size title
    bg.color = "transparent",
    outer.bg.color = "transparent"
  )

file_path <- paste0(output_path, "total_pop_grid_map.png")

tmap_save(filename = file_path, bg = NA, width = 6, height = 8)

#### Generate time series ####

# Combine county data with total data for plotting
plot_data <- bind_rows(
  raceincome_by_county,
  raceincome_combined %>% mutate(NAME = "Total", COUNTYFP = "Total")
)

ggplot(plot_data, aes(x = year, y = n_noise_postprocessed, color = NAME)) +
  geom_line(size = 1.5, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 0.9) +
  labs(
    title = "Population Trends in Charlottesville and Albemarle County",
    subtitle = "1999–2023",
    x = "Year",
    y = "Population",
    color = ""
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2020, by = 5),
    minor_breaks = seq(1999, 2023, by = 1),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    labels = scales::comma_format(scale = 1e-3, suffix = "K"),
    expand = c(0.02, 0)
  ) +
  scale_color_manual(
    values = c("Albemarle" = "#232d4b", "Charlottesville" = "#e57200", "Total" = "maroon"),
    labels = c("Albemarle County", "Charlottesville City", "Combined Total")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Plot titles
    plot.title = element_text(
      face = "bold", 
      size = 16, 
      hjust = 0,
      margin = margin(b = 5),
      color = "black"
    ),
    plot.subtitle = element_text(
      size = 14,
      hjust = 0,
      margin = margin(b = 20),
      color = "gray30"
    ),
    plot.caption = element_text(
      size = 9,
      hjust = 0,
      margin = margin(t = 15),
      color = "gray50",
      lineheight = 1.1
    ),
    
    # Axes
    axis.title = element_text(size = 12, color = "gray20"),
    axis.text = element_text(size = 10, color = "gray30"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    
    # Grid
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "gray20"),
    legend.key.width = unit(1.5, "cm"),
    legend.margin = margin(t = 15),
    legend.box.spacing = unit(0, "pt"),
    
    # Plot margins
    plot.margin = margin(20, 25, 15, 20),
    
    # Remove plot background
    plot.background = element_rect(fill = "white", color = NA)
  )

file_path <- paste0(output_path, "pop_time_series.png")

ggsave(
  filename = file_path,
  plot = last_plot(),
  width = 10,                # Wider for better proportions
  height = 6,                # Publication aspect ratio
  dpi = 300,                 # High resolution for print
  bg = "white",              # White background for publications
  device = "png"             # PNG format
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

#### RACE MAP ####

# Create faceted map by race using specified race columns
race_columns <- c("race_ethnicity_AIAN", "race_ethnicity_Asian", "race_ethnicity_Black", "race_ethnicity_Hispanic", "race_ethnicity_White")

# Check if all race columns exist in the data
missing_cols <- setdiff(race_columns, names(data_2023_hex))
if (length(missing_cols) > 0) {
  cat("Warning: Missing columns in data:", paste(missing_cols, collapse = ", "), "\n")
  race_columns <- intersect(race_columns, names(data_2023_hex))
}

if (length(race_columns) > 0) {
  # Convert race columns to numeric and reshape data for faceting
  data_race_long <- data_2023_hex %>%
    select(hex_id, geometry, all_of(race_columns)) %>%
    mutate(across(all_of(race_columns), as.numeric)) %>%
    pivot_longer(
      cols = all_of(race_columns),
      names_to = "race_ethnicity",
      values_to = "population"
    ) %>%
    # Clean up race labels for better display
    mutate(
      race_ethnicity = case_when(
        race_ethnicity == "race_ethnicity_AIAN" ~ "American Indian/Alaska Native",
        race_ethnicity == "race_ethnicity_Asian" ~ "Asian",
        race_ethnicity == "race_ethnicity_Black" ~ "Black/African American",
        race_ethnicity == "race_ethnicity_Hispanic" ~ "Hispanic/Latino",
        race_ethnicity == "race_ethnicity_White" ~ "White",
        TRUE ~ race_ethnicity
      )
    )
  
  # Create faceted map with individual legends
  race_map <- tm_basemap("OpenStreetMap", alpha = 0.3) +
    tm_shape(data_race_long) +
    tm_polygons(
      col = "population",
      palette = "Blues",
      title = "",  # Remove legend title
      style = "cont",
      alpha = 0.8,
      border.col = "white",
      border.lwd = 0.1,
      colorNA = "transparent",
      textNA = "",
      legend.show = TRUE  # Enable individual legends
    ) +
    tm_facets(
      by = "race_ethnicity",
      nrow = 2,
      ncol = 3,
      free.coords = FALSE,
      free.scales = TRUE,  # Allow different scales for each panel
      showNA = FALSE
    ) +
    tm_layout(
      title = "",  # Main title restored
      title.size = 1.2,
      title.position = c("center", "top"),
      panel.labels = c("AIAN", "Asian", "Black", "Hispanic", "White"),
      panel.label.size = 1.0,
      panel.label.fontface = "bold",
      legend.outside = FALSE,  # Keep legends inside each panel
      legend.position = c("right", "bottom"),  # Position within each panel
      legend.text.size = 0.25,
      legend.height = 16,  # Fixed legend height for consistency
      legend.width = 4,   # Fixed legend width for consistency
      bg.color = "transparent",
      outer.bg.color = "transparent",
      between.margin = 0.5
    )
  
  # Save the faceted race map
  file_path <- paste0(output_path, "population_by_race_faceted_map.png")
  
  tmap_save(
    tm = race_map,
    filename = file_path,
    bg = "NA",
    width = 7,
    height = 5,
    dpi = 300
  )
  
  cat("Faceted race map saved to:", file_path, "\n")
  
} else {
  cat("No valid race columns found in the data.\n")
}

