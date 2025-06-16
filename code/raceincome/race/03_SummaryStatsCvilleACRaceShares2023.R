################################################################################
# FILE: 03_SummaryStatsCvilleACRaceShares2023.R
# PURPOSE: Use gridded EIF data for race/income in 2023 to generate summary 
# statistics on population race shares for U.S., Virginia, and Cville/AC. Create
# table with raw population numbers and shares.
# AUTHOR: Elizabeth Shiker
# CREATED: June 11th, 2025
################################################################################
# INPUTS: raceincome_2023.rda
# OUTPUTS: race_summary_table.xlsx
################################################################################

# Load in race and income data from 2023

raceincome_2023 <- load("data/processed/raceincome_rda/raceincome_2023.rda")
raceincome2023 <- get(raceincome_2023)

#### United States ####

# sum population in each grid by race

USsumbygrid <- raceincome2023 %>%
  filter(!is.na(race_ethnicity)) %>%
  group_by(grid_lon, grid_lat, race_ethnicity) %>%
  summarise(
    n_noise = sum(n_noise, na.rm = TRUE),
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

US_2023_wide <- USsumbygrid %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = c(n_noise, n_noise_postprocessed),
    names_sep = "_"
  )

# Sum total post processed population by race/ethnicity and calculate shares

US_totals <- US_2023_wide %>%
  summarise(across(starts_with("n_noise_postprocessed_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    everything(),
    names_to = "race",
    values_to = "total_population"
  ) %>%
  mutate(race = gsub("n_noise_postprocessed_", "", race)) %>%
  mutate(share = total_population / sum(total_population))

#### Virginia ####

#Filter for Virginia

Va2023 <- raceincome2023 %>%
  filter(STATEFP == "51")

# sum population in each grid by race

Vasumbygrid <- Va2023 %>%
  filter(!is.na(race_ethnicity)) %>%
  group_by(grid_lon, grid_lat, race_ethnicity) %>%
  summarise(
    n_noise = sum(n_noise, na.rm = TRUE),
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

Va_2023_wide <- Vasumbygrid %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = c(n_noise, n_noise_postprocessed),
    names_sep = "_"
  )

# Sum total post processed population by race/ethnicity and calculate shares

Va_totals <- Va_2023_wide %>%
  summarise(across(starts_with("n_noise_postprocessed_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    everything(),
    names_to = "race",
    values_to = "total_population"
  ) %>%
  mutate(race = gsub("n_noise_postprocessed_", "", race)) %>%
  mutate(share = total_population / sum(total_population))

#### CVILLE/AC ####

#Filter for Cville/AC

CvilleAC2023 <- raceincome2023 %>%
  filter(
    STATEFP == "51" & COUNTYFP %in% c("540", "003")
  )

# sum population in each grid by race

CvilleACsumbygrid <- CvilleAC2023 %>%
  filter(!is.na(race_ethnicity)) %>%
  group_by(grid_lon, grid_lat, race_ethnicity) %>%
  summarise(
    n_noise = sum(n_noise, na.rm = TRUE),
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

CvilleAC_2023_wide <- CvilleACsumbygrid %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = c(n_noise, n_noise_postprocessed),
    names_sep = "_"
  )

# Sum total post processed population by race/ethnicity and calculate shares

CvilleAC_totals <- CvilleAC_2023_wide %>%
  summarise(across(starts_with("n_noise_postprocessed_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    everything(),
    names_to = "race",
    values_to = "total_population"
  ) %>%
  mutate(race = gsub("n_noise_postprocessed_", "", race)) %>%
  mutate(share = total_population / sum(total_population))

#####Combined Table US, VA, and Cville/AC#####

# Rename columns before joining

US_clean <- US_totals %>%
  rename(US_raw = total_population, US_share = share)

Va_clean <- Va_totals %>%
  rename(VA_raw = total_population, VA_share = share)

CvilleAC_clean <- CvilleAC_totals %>%
  rename(CvilleAC_raw = total_population, CvilleAC_share = share)

#Join all three tables by 'race'

combined_totals <- US_clean %>%
  full_join(Va_clean, by = "race") %>%
  full_join(CvilleAC_clean, by = "race")

#Take out Other/Unknown group from table  

combined_totals_display <- combined_totals %>%
  filter(race != "Other/Unknown") %>%
  arrange(desc(US_raw))

#Create a total row and bind to bottom of table

totals_row <- combined_totals %>%
  summarise(
    race = "Total", 
    US_raw = sum(US_raw, na.rm = TRUE),
    US_share = sum(US_share, na.rm = TRUE),
    VA_raw = sum(VA_raw, na.rm = TRUE),
    VA_share = sum(VA_share, na.rm = TRUE),
    CvilleAC_raw = sum(CvilleAC_raw, na.rm = TRUE),
    CvilleAC_share = sum(CvilleAC_share, na.rm = TRUE)
  )

combined_totals_display_with_total <- bind_rows(combined_totals_display, totals_row)

View(combined_totals_display_with_total)

#Install packages for saving

install.packages("openxlsx")
library(openxlsx)

# ✅ Create the full nested folder path first
dir.create("output/raceincome/race", showWarnings = FALSE, recursive = TRUE)

# Create workbook and add worksheet

wb <- createWorkbook()
addWorksheet(wb, "Race Summary")

# Write data

writeData(wb, "Race Summary", combined_totals_display_with_total)

# Formatting 

bold_text <- createStyle(textDecoration = "bold")

# Bold the word "Total"

total_row_index <- which(combined_totals_display_with_total$race == "Total") + 1  # +1 for header
addStyle(
  wb,
  sheet = "Race Summary",
  style = bold_text,
  rows = total_row_index,
  cols = 1,  # Only race column
  gridExpand = FALSE
)

# Bold the header row
addStyle(
  wb,
  sheet = "Race Summary",
  style = bold_text,
  rows = 1,
  cols = 1:ncol(combined_totals_display_with_total),
  gridExpand = TRUE
)
# Save
saveWorkbook(wb, "output/raceincome/race/race_summary_table.xlsx", overwrite = TRUE)
  


