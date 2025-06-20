################################################################################
# FILE: 07_avgIncomedecile_byRace_sparklineTable_CvilleAC1999to2023.R
# PURPOSE: Use gridded EIF data for race/income from 1999 to 2023 to generate 
# table with columns race/ethnicity, average income decile trend between 1999
# and 2023, and avergae income decile in 2023.
# AUTHOR: Elizabeth Shiker
# CREATED: June 20th, 2025
################################################################################
# INPUTS: raceincome_year.rda
# OUTPUTS: avgIncomeDecilebyRace_1999to2023_CvilleAC_table.html
################################################################################

# Install Packages and Load libraries
install.packages("htmltools")
install.packages("sparkline")
install.packages("reactable")

library(dplyr)
library(tidyr)
library(purrr)
library(reactable)
library(sparkline)
library(htmltools)
library(scales)

# Set working directory
setwd("/Users/elizabethshiker/The Lab Dropbox/Elizabeth Shiker/environmental-inequality-atlas/")

# Define years and data path
years <- 1999:2023
data_path <- "data/processed/raceincome_rda"

# Compute average income decile by race for each year 
avg_income_list <- lapply(years, function(yr) {
  file <- file.path(data_path, paste0("raceincome_", yr, ".rda"))
  obj_name <- load(file)
  data <- get(obj_name)
  
  data <- data %>%
    filter(!is.na(race_ethnicity), !is.na(income_decile), !is.na(n_noise_postprocessed))
  
  # By race
  race_avg <- data %>%
    group_by(race_ethnicity) %>%
    summarise(
      year = yr,
      avg_income_decile = weighted.mean(income_decile, w = n_noise_postprocessed, na.rm = TRUE),
      .groups = "drop"
    )
  
  # "All Races" (including Other/Unknown)
  all_races <- data %>%
    summarise(
      race_ethnicity = "All Races",
      year = yr,
      avg_income_decile = weighted.mean(income_decile, w = n_noise_postprocessed, na.rm = TRUE)
    )
  
  bind_rows(race_avg, all_races)
})

# Combine all years into a single long-format table
avg_income_long <- bind_rows(avg_income_list)

# Step 3: Create sparkline input column
income_trend_wide <- avg_income_long %>%
  group_by(race_ethnicity) %>%
  arrange(year) %>%
  summarise(
    Trend = list(avg_income_decile),
    .groups = "drop"
  )

# Step 4: Pull 2023 income deciles
avg_income_2023 <- avg_income_long %>%
  filter(year == 2023) %>%
  select(race_ethnicity, avg_income_decile)

# Step 5: Merge and finalize display table
race_table_data <- income_trend_wide %>%
  left_join(avg_income_2023, by = "race_ethnicity") %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  arrange(desc(race_ethnicity == "All Races"))  # Put "All Races" first

# Step 6: Render interactive table
htmltools::save_html(
  reactable(
    race_table_data,
    columns = list(
      race_ethnicity = colDef(name = "Race/Ethnicity"),
      
      avg_income_decile = colDef(
        name = "Avg. Income Decile (2023)",
        format = colFormat(digits = 1),
        align = "center"
      ),
      
      Trend = colDef(
        name = "Trend (1999–2023)",
        cell = function(values) {
          n <- length(values)
          left_label <- formatC(round(values[1], 1), format = "f", digits = 1)
          right_label <- formatC(round(values[n], 1), format = "f", digits = 1)
          
          spots <- list()
          spots[["1"]] <- "black"
          spots[[as.character(n)]] <- "black"
          
          spark <- sparkline(
            values,
            type = "line",
            chartRangeMin = 3,
            chartRangeMax = 7,
            lineColor = "black",
            lineWidth = 2,
            fillColor = "rgba(0, 0, 0, 0.15)",
            spotRadius = 4,
            valueSpots = spots,
            spotColor = "black",
            highlightSpotColor = "black",
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
  file = "output/raceincome/avgIncomeDecilebyRace_1999to2023_CvilleAC_table.html"
)
