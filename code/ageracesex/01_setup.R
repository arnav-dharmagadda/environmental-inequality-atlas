################################################################################
# FILE: 01_setup.R
# PURPOSE: Set up the environment for cleaning and analyzing the gridded EIF
# age, race, and sex files.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 3rd, 2025
################################################################################
# INPUTS: gridpoints_with_county_2020.rda, gridded_eif_pop_ageracesex files
# (1999-2023)
# OUTPUTS: None.
################################################################################

#### Setting working directory and file paths ####

setwd("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/")

# File Paths

processed <- "data/processed/"
maps_output <- "output/ageracesex/02_maps/"

#### Load Libraries ####

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(sf, terra, dplyr, ggplot2, tmap, arrow, dplyr, tidyr, scales, haven, tigris)

#### Load Data ####

file_path <- paste0(processed, "ageracesex_rda/ageracesex_year_long.rda")
load(file_path)
data_all <- ageracesex_year_long
data_2023 <- ageracesex_year_long

file_path <- paste0(processed, "ageracesex_rda/ageracesex_year_long_hex.rda")
load(file_path)
data_2023_hex <- ageracesex_year_long

#### Filter and Clean Data ####

file_path <- paste0(processed, "ageracesex_rda/ageracesex_2023.rda")
load(file_path)
data_2023_hex <- merged_year

data_2023_hex %>% 
  summarize(total_sum = sum(n_noise, na.rm = TRUE))

data_2023 <- data_2023 %>%
  filter(year == 2023) %>%
  filter(STATEFP == "51" & COUNTYFP == "003" | COUNTYFP == "540") %>%
  rowwise() %>%
  mutate(total = sum(c(M, F, NA_sex, na_sex), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    black_share = Black / total,
    white_share = White / total,
    hispanic_share = Hispanic / total,
    asian_share = Asian / total,
    male_share = M / total,
    female_share = F / total,
    under_18_share = under_18 / total,
    bet_19_65_share = bet_19_65 / total,
    over_65_share = over_65 / total,
  )

data_2023_hex <- data_2023_hex %>%
  filter(year == 2023) %>%
  filter(STATEFP == "51" & COUNTYFP == "003" | COUNTYFP == "540") %>%
  rowwise() %>%
  mutate(total = sum(c(M, F, NA_sex, na_sex), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    black_share = Black / total,
    white_share = White / total,
    hispanic_share = Hispanic / total,
    asian_share = Asian / total,
    male_share = M / total,
    female_share = F / total,
    under_18_share = under_18 / total,
    bet_19_65_share = bet_19_65 / total,
    over_65_share = over_65 / total,
  )