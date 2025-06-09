################################################################################
# FILE: PopShareRaceSeparatedCvilleAC2023.R
# PURPOSE: Use gridded EIF data to create a bar chart on population share by
# race/ethnicity in 2023, separated by Charlottesville and Albemarle County.
# AUTHOR: Elizabeth Shiker
# CREATED: June 6th, 2025
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

# sum population in each grid by race
pop_race <- pop %>%
  group_by(grid_lon, grid_lat, race_ethnicity) %>%
  summarise(
    n_noise = sum(n_noise, na.rm = TRUE),
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

pop_race_wide <- pop_race %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = c(n_noise, n_noise_postprocessed),
    names_sep = "_"
  )

# Filter the county data for Charlottesville and Albemarle County
# Charlottesville: STATEFP = "51", COUNTYFP = "540"
# Albemarle: STATEFP = "51", COUNTYFP = "003"

county_filtered <- county %>%
  filter(
    (STATEFP == "51" & COUNTYFP == "540")  |
      (STATEFP == "51" & COUNTYFP == "003")
  )

# Merge the dataframes

merged_poprace <- county_filtered %>%
  inner_join(pop_race_wide, by = c("pm25_grid_x"="grid_lon", "pm25_grid_y"="grid_lat"))

# Step 1: Summarize postprocessed population by race and county
race_shares_by_county <- merged_poprace %>%
  mutate(
    county_name = case_when(
      COUNTYFP == "540" ~ "Charlottesville",
      COUNTYFP == "003" ~ "Albemarle",
      TRUE ~ "Other"
    )
  ) %>%
  # Sum pop by race within each county
  group_by(county_name) %>%
  summarise(across(starts_with("n_noise_postprocessed_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = starts_with("n_noise_postprocessed_"),
    names_to = "race",
    values_to = "total_population"
  ) %>%
  mutate(
    race = gsub("n_noise_postprocessed_", "", race)
  ) %>%
  group_by(county_name) %>%
  mutate(
    share = total_population / sum(total_population)
  ) %>%
  ungroup()

# Step 2: Plot it
library(scales)

ggplot(race_shares_by_county, aes(x = reorder(race, -share), y = share, fill = county_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Population Share by Race/Ethnicity and County (2023)",
    x = "Race/Ethnicity",
    y = "Share of County Population",
    fill = "County"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))