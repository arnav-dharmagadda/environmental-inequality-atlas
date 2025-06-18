################################################################################
# FILE: 05_race_share_count_1999to2023_CvilleAC_tables.R
# PURPOSE: Use gridded EIF data for race/income from 1999 to 2023 to generate 
# two tables. One table has columns race/ethnicity, 2023 population race 
# shares, and the general trend between 1999 and 2023 of race shares for 
# Cville/AC in the form of a sparkline. Table two uses raw population counts.
# AUTHOR: Elizabeth Shiker
# CREATED: June 18th, 2025
################################################################################
# INPUTS: raceincome_year.rda
# OUTPUTS: race_share_1999to2023_CvilleAC_table.html,
# race_count_1999to2023_CvilleAC_table.html
################################################################################

# Install and Load required packages

install.packages("reactable") #interactive table
install.packages("sparkline") #sparklines
install.packages("htmltools") #HTML content within R

library(dplyr) #data wrangling
library(tidyr) #reshaping data
library(purrr) #functional programming
library(reactable) #build interactive table
library(sparkline) #generating sparkline
library(htmltools) #HTML formatting
library(scales) #formatting values

# Set working directory

setwd("/Users/elizabethshiker/The Lab Dropbox/Elizabeth Shiker/environmental-inequality-atlas/")

# Define years and data path

years <- 1999:2023
data_path <- "data/processed/raceincome_rda"

# Load each year's RDA file, calculate racial shares, and store in a list

race_share_list <- lapply(years, function(yr) {
  file <- file.path(data_path, paste0("raceincome_", yr, ".rda"))
  obj_name <- load(file)
  data <- get(obj_name)
  
  total_pop <- sum(data$n_noise_postprocessed, na.rm = TRUE)

  #Summarize population by race and calculate racial share  
  data %>%
    group_by(race_ethnicity) %>%
    summarise(group_pop = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      year = yr,
      share = group_pop / total_pop
    ) %>%
    select(race_ethnicity, year, share)
})

# Combine all years

race_share_long <- bind_rows(race_share_list)

# Format for table

race_share_wide <- race_share_long %>%
  filter(race_ethnicity != "Other/Unknown") %>% #exclude unknown category from table only
  arrange(year) %>% #chronologically sorted
  group_by(race_ethnicity) %>%
  summarise(
    Share2023 = share[year == 2023], #extract latest share
    Trend = list(as.numeric(share)), # store full share trend as a list
    .groups = "drop"
  ) %>%
  arrange(desc(Share2023)) #sort table by descending 2023 share

# Build interactive table with sparkline trends and endpoint labels

htmltools::save_html(
  reactable(
    race_share_wide,
    columns = list(
      race_ethnicity = colDef(name = "Race/Ethnicity"), #first column
      
      Share2023 = colDef(
        name = "Share in 2023",
        format = colFormat(percent = TRUE, digits = 1), #format as %
        align = "center"
      ),
      
      Trend = colDef(
        name = "Trend (1999–2023)",
        cell = function(values) {
          n <- length(values) #total number of points
          
          #format first and last values as percentages
          left_label <- percent(values[1], accuracy = 0.1)
          right_label <- percent(values[n], accuracy = 0.1)
          
          #create a named list of value spots (dots at both ends of the line)
          spots <- list()
          spots[["1"]] <- "black"
          spots[[as.character(n)]] <- "black"
          
          #create sparkline with shaded area and endpoint dots
          spark <- sparkline(
            values,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = 1,
            lineColor = "black",
            lineWidth = 2,
            fillColor = "rgba(0, 0, 0, 0.15)", #light gray shaded area
            spotRadius = 4,
            valueSpots = spots, #add endpoint dots
            spotColor = "black",
            highlightSpotColor = "black",
            highlightLineColor = NULL,
            minSpotColor = FALSE,
            maxSpotColor = FALSE,
            lastSpotColor = FALSE,
            width = 100, #compress horizontal scale
            height = 40
          )
          
          #combine labels and sparkline into horizontal layout
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
    bordered = TRUE, #add border to table
    striped = TRUE, #alternate row shading
    highlight = TRUE, #highlight row on hover
    compact = TRUE #tight spacing
  ),
  file = "output/raceincome/race/race_share_1999to2023_CvilleAC_table.html"
)

####RACE RAW COUNTS TABLE####

# Load yearly data and compute raw population counts per race
race_count_list <- lapply(years, function(yr) {
  file <- file.path(data_path, paste0("raceincome_", yr, ".rda"))
  obj_name <- load(file)
  data <- get(obj_name)
  
  # Summarize by race (no share, just total counts)
  data %>%
    group_by(race_ethnicity) %>%
    summarise(group_pop = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = yr) %>%
    select(race_ethnicity, year, group_pop)
})

# Combine yearly results into long format
race_count_long <- bind_rows(race_count_list)

# Reshape to wide format with counts and trend list
race_count_wide <- race_count_long %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  arrange(year) %>%
  group_by(race_ethnicity) %>%
  summarise(
    Count2023 = group_pop[year == 2023],
    Trend = list(as.numeric(group_pop)),
    .groups = "drop"
  ) %>%
  arrange(desc(Count2023))

htmltools::save_html(
  reactable(
    race_count_wide,
    columns = list(
      race_ethnicity = colDef(name = "Race/Ethnicity"),
      Count2023 = colDef(
        name = "Population in 2023",
        format = colFormat(separators = TRUE),  # format with commas
        align = "center"
      ),
      Trend = colDef(
        name = "Trend (1999–2023)",
        cell = function(values) {
          n <- length(values)
          left_label <- formatC(values[1], big.mark = ",")
          right_label <- formatC(values[n], big.mark = ",")
          
          spots <- list()
          spots[["1"]] <- "black"
          spots[[as.character(n)]] <- "black"
          
          spark <- sparkline(
            values,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = max(values, na.rm = TRUE),
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
  file = "output/raceincome/race/race_count_1999to2023_CvilleAC_table.html"
)
