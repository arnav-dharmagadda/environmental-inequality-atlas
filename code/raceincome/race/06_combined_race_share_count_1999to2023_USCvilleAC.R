################################################################################
# FILE: 06_combined_race_share_count_1999to2023_USCvilleAC.R
# PURPOSE: Use gridded EIF data for race/income from 1999 to 2023 to generate 
# one table with columns race/ethnicity, 2023 population counts, trend in count,
# race shares, shares trend, and U.S. shares trend in the form of a sparkline.
# AUTHOR: Elizabeth Shiker
# CREATED: June 26th, 2025
################################################################################
# INPUTS: raceincome_year.rda
# OUTPUTS: race_combined_count_share_1999to2023_USCvilleAC.html
################################################################################

# Install and load required packages
install.packages("reactable")
install.packages("sparkline")
install.packages("htmltools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("purrr")
install.packages("scales")
install.packages("arrow")
install.packages("sf")

library(dplyr)
library(tidyr)
library(purrr)
library(reactable)
library(sparkline)
library(htmltools)
library(scales)
library(arrow)
library(sf)

##### Creating U.S. Data Set #####

# Define the range of years and paths
years <- 1999:2023
base_path <- "/Users/elizabethshiker/The Lab Dropbox/Elizabeth Shiker/gridded_eif_data/raceincome/"
output_path <- "/Users/elizabethshiker/The Lab Dropbox/Elizabeth Shiker/GitHub/environmental-inequality-atlas/data/processed/raceincome_rda/raceincome_national.rda"

# Function to summarize each parquet file
summarize_parquet <- function(year) {
  file <- paste0(base_path, "gridded_eif_pop_raceincome_", year, ".parquet")
  
  arrow::read_parquet(file) %>%
    group_by(race_ethnicity) %>%
    summarize(
      n_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = year)
}

# Apply function across all years and combine results
raceincome_national <- map_dfr(years, summarize_parquet)

# National share trend (includes Other/Unknown in total)
race_share_long_natl <- raceincome_national %>%
  group_by(year) %>%
  mutate(total = sum(n_postprocessed, na.rm = TRUE)) %>%
  group_by(race_ethnicity, year) %>%
  summarise(
    share = sum(n_postprocessed, na.rm = TRUE) / first(total),
    .groups = "drop"
  )

# Filter out Other/Unknown AFTER share calc
race_share_wide_natl <- race_share_long_natl %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  arrange(year) %>%
  group_by(race_ethnicity) %>%
  summarise(
    US_Share_Trend = list(as.numeric(share)),
    .groups = "drop"
  ) %>%
  rename ("Race/Ethnicity" = race_ethnicity)

# Set working directory
setwd("/Users/elizabethshiker/The Lab Dropbox/Elizabeth Shiker/GitHub/environmental-inequality-atlas/")

# Define years and data path
years <- 1999:2023
data_path <- "data/processed/raceincome_rda"

### Step 1: Load and calculate share trends ###

race_share_list <- lapply(years, function(yr) {
  file <- file.path(data_path, paste0("raceincome_", yr, ".rda"))
  env <- new.env()
  load(file, envir = env)
  
  # Safely get the loaded data
  data <- env[[ls(env)[1]]]
  
  # Drop spatial geometry if present (convert sf → data.frame)
  data <- sf::st_drop_geometry(data)
  
  # Now continue with your summary
  total_pop <- sum(data$n_noise_postprocessed, na.rm = TRUE)
  
  data %>%
    group_by(race_ethnicity) %>%
    summarise(group_pop = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = yr, share = group_pop / total_pop) %>%
    select(race_ethnicity, year, share)
})

race_share_long <- bind_rows(race_share_list)

# Collapse to latest year and full trend
race_share_wide <- race_share_long %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  arrange(year) %>%
  group_by(race_ethnicity) %>%
  summarise(
    Share2023 = share[year == 2023],
    Trend = list(as.numeric(share)),
    .groups = "drop"
  )

### Step 2: Load and calculate count trends ###

race_count_list <- lapply(years, function(yr) {
  file <- file.path(data_path, paste0("raceincome_", yr, ".rda"))
  obj_name <- load(file)
  data <- get(obj_name)
  
  data %>%
    group_by(race_ethnicity) %>%
    summarise(group_pop = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = yr) %>%
    select(race_ethnicity, year, group_pop)
})

race_count_long <- bind_rows(race_count_list)

race_count_wide <- race_count_long %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  arrange(year) %>%
  group_by(race_ethnicity) %>%
  summarise(
    Count2023 = group_pop[year == 2023],
    Trend = list(as.numeric(group_pop)),
    .groups = "drop"
  )

### Step 3: Combine counts and shares into one table ###

race_combined <- left_join(race_count_wide, race_share_wide, by = "race_ethnicity") %>%
  rename(
    `Race/Ethnicity` = race_ethnicity,
    Count = Count2023,
    Count_Trend = Trend.x,
    Share = Share2023,
    Share_Trend = Trend.y
  ) %>%
  arrange(desc(Count)) # sort table by total population

####Add US data####

race_combined_extended <- left_join(race_combined, race_share_wide_natl, by = "Race/Ethnicity")

# Strip geometry column AND remove sf class completely
race_combined_extended <- race_combined_extended %>%
  sf::st_drop_geometry() %>%
  as.data.frame()  # this fully drops sf class, preventing reattachment

### Step 4: Build interactive table with sparklines ###

htmltools::save_html(
  reactable(
    race_combined_extended,
    columns = list(
      `Race/Ethnicity` = colDef(name = "Race/Ethnicity"),
      
      Count = colDef(
        name = "Population Count in 2023",
        format = colFormat(separators = TRUE, digits = 0),
        align = "center"
      ),
      
      Count_Trend = colDef(
        name = "Count Trend (1999–2023)",
        cell = function(values) {
          n <- length(values)
          left_label <- formatC(values[1], format = "f", big.mark = ",", digits = 0)
          right_label <- formatC(values[n], format = "f", big.mark = ",", digits = 0)
          
          spots <- list()
          spots[["1"]] <- "black"
          spots[[as.character(n)]] <- "black"
          
          spark <- sparkline(
            values,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = max(values, na.rm = TRUE),
            lineColor = "black",
            lineWidth = 1,
            fillColor = "rgba(0, 0, 0, 0.15)",
            spotRadius = 1.5,
            valueSpots = spots,
            spotColor = "black",
            highlightSpotColor = NULL,
            highlightLineColor = NULL,
            minSpotColor = FALSE,
            maxSpotColor = FALSE,
            lastSpotColor = FALSE,
            width = 100,
            height = 40
          )
          
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            tags$span(left_label, style = "font-size: 10px; color: black;"),
            tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
            tags$span(right_label, style = "font-size: 10px; color: black;")
          )
        },
        html = TRUE,
        align = "center"
      ),
      
      Share = colDef(
        name = "Share in 2023",
        format = colFormat(percent = TRUE, digits = 1),
        align = "center"
      ),
      
      Share_Trend = colDef(
        name = "Share Trend (1999–2023)",
        cell = function(values) {
          n <- length(values)
          left_label <- percent(values[1], accuracy = 0.1)
          right_label <- percent(values[n], accuracy = 0.1)
          
          spots <- list()
          spots[["1"]] <- "black"
          spots[[as.character(n)]] <- "black"
          
          spark <- sparkline(
            values,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = 1,
            lineColor = "black",
            lineWidth = 1,
            fillColor = "rgba(0, 0, 0, 0.15)",
            spotRadius = 1.5,
            valueSpots = spots,
            spotColor = "black",
            highlightSpotColor = NULL,
            highlightLineColor = NULL,
            minSpotColor = FALSE,
            maxSpotColor = FALSE,
            lastSpotColor = FALSE,
            width = 100,
            height = 40
          )
          
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            tags$span(left_label, style = "font-size: 10px; color: black;"),
            tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
            tags$span(right_label, style = "font-size: 10px; color: black;")
          )
        },
        html = TRUE,
        align = "center"
      ),
      
      US_Share_Trend = colDef(
        name = "US Share Trend (1999–2023)",
        cell = function(values) {
          n <- length(values)
          left_label <- percent(values[1], accuracy = 0.1)
          right_label <- percent(values[n], accuracy = 0.1)
          
          spots <- list()
          spots[["1"]] <- "black"
          spots[[as.character(n)]] <- "black"
          
          spark <- sparkline(
            values,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = 1,
            lineColor = "black",
            lineWidth = 1,
            fillColor = "rgba(0, 0, 0, 0.15)",
            spotRadius = 1.5,
            valueSpots = spots,
            spotColor = "black",
            highlightSpotColor = NULL,
            highlightLineColor = NULL,
            minSpotColor = FALSE,
            maxSpotColor = FALSE,
            lastSpotColor = FALSE,
            width = 100,
            height = 40
          )
          
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            tags$span(left_label, style = "font-size: 10px; color: black;"),
            tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
            tags$span(right_label, style = "font-size: 10px; color: black;")
          )
        },
        html = TRUE,
        align = "center"
      )
    ),
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE
  ),
  file = "output/raceincome/race/race_combined_count_share_1999to2023_USCvilleAC.html"
)
