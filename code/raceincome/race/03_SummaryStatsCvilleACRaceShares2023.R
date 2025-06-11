################################################################################
# FILE: 03_SummaryStatsCvilleACRaceShares2023.R
# PURPOSE: Use gridded EIF data for race/income in 2023 to generate summary 
# statistics on population race shares for U.S., Virginia, and Cville/AC
# AUTHOR: Elizabeth Shiker
# CREATED: June 11th, 2025
################################################################################
# INPUTS: raceincome_2023.rda
# OUTPUTS: None. 
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
  filter(race != "Other/Unknown") %>% #exclude before computing share
  mutate(share = total_population / sum(total_population))

View(US_totals)

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
  filter(race != "Other/Unknown") %>% #exclude before computing share
  mutate(share = total_population / sum(total_population))

View(Va_totals)

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
  filter(race != "Other/Unknown") %>% #exclude before computing share
  mutate(share = total_population / sum(total_population))

View(CvilleAC_totals)

