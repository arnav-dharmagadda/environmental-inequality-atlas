################################################################################
# FILE: PopRaceCvilleAlbemarle2023.R
# PURPOSE: Use gridded EIF data to create a bar chart on population share by
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

#EXAMPLE SUMMATION WHITE
#total_white_population <- merged_poprace %>%
  #summarise(total_people = sum(n_noise_postprocessed_White, na.rm = TRUE))

#print(total_white_population)

# Sum total post processed population by race/ethnicity
#race_totals <- merged_poprace %>%
  #summarise(across(starts_with("n_noise_postprocessed_"), ~sum(.x, na.rm = TRUE))) %>%
  #pivot_longer(
    #everything(),
    #names_to = "race",
    #values_to = "total_population"
  #) %>%
  #mutate(
    #race = gsub("n_noise_postprocessed_", "", race)  # clean race names
  #)

# Sum total post processed population by race/ethnicity and calculate shares
race_totals <- merged_poprace %>%
  summarise(across(starts_with("n_noise_postprocessed_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    everything(),
    names_to = "race",
    values_to = "total_population"
  ) %>%
  mutate(
    race = gsub("n_noise_postprocessed_", "", race),
    share = total_population / sum(total_population)  # calculate share
  )



# View result
print(race_totals)

# Plot as a histogram (bar chart)
#ggplot(race_totals, aes(x = reorder(race, -total_population), y = total_population)) +
  #geom_bar(stat = "identity", fill = "steelblue") +
  #labs(title = "Total Population by Race/Ethnicity in Charlottesville and Albemarle County (2023)",
       #x = "Race/Ethnicity",
       #y = "Total Postprocessed Population") +
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plotting Shares
library(scales)  # for percent_format()

ggplot(race_totals, aes(x = reorder(race, -share), y = share)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Population Share by Race/Ethnicity in Charlottesville and Albemarle County (2023)",
    x = "Race/Ethnicity",
    y = "Share of Total Population"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))