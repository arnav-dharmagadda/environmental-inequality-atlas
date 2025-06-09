################################################################################
# FILE: AvgIncomebyRaceCvilleAC2023.R
# PURPOSE: Use gridded EIF data to create a bar chart on average income decile by
# race/ethnicity in Charlottesville and Albemarle County in 2023.
# AUTHOR: Elizabeth Shiker
# CREATED: June 5th, 2025
################################################################################
# INPUTS: gridpoints_with_county_2020.rda, gridded_eif_pop_raceincome_2023.parquet
# OUTPUTS: None.
################################################################################

#### Clear Environment ####

rm(list = ls())

# Load libraries
library(arrow)     # for reading parquet
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)

setwd("/Users/elizabethshiker/Dropbox/environmental-inequality-atlas/")

# Load CBSA gridpoints

load("data/gridpoints_with_county_2020.rda")
county <- df

# Load population data 

pop <- read_parquet("data/eif_pop_raceincome/gridded_eif_pop_raceincome_2023.parquet")

names(pop)
names(county)

# Filter the county data for Charlottesville and Albemarle County
# Charlottesville: STATEFP = "51", COUNTYFP = "540"
# Albemarle: STATEFP = "51", COUNTYFP = "003"

county_filtered <- county %>%
  filter(
    (STATEFP == "51" & COUNTYFP == "540")  |
      (STATEFP == "51" & COUNTYFP == "003")
  )

# Merge the data frames
merged_popraceincome <- county_filtered %>%
  inner_join(pop, by = c("pm25_grid_x"="grid_lon", "pm25_grid_y"="grid_lat"))
names(merged_popraceincome)

#Calculating average income for each race in Cville/AC
avg_income_by_race <- merged_popraceincome %>%
  group_by(race_ethnicity) %>%
  summarise(
    avg_income_decile = weighted.mean(income_decile, w = n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(avg_income_by_race, aes(x = reorder(race_ethnicity, -avg_income_decile), y = avg_income_decile)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Income Decile by Race/Ethnicity (2023)",
    x = "Race/Ethnicity",
    y = "Average Income Decile (Weighted by Population)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




