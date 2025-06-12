################################################################################
# FILE: 05SummaryStatsRacePopulationSharesinIncomeDecilesCvilleAC2023.R
# PURPOSE: Use 2023 race/income data from the gridded EIF to generate summary 
# statistics for race population shares at low, mid, and high income deciles in the United 
# States, Virginia, and Charlottesville and Albemarle County.
# AUTHOR: Elizabeth Shiker
# CREATED: June 11th, 2025
################################################################################
# INPUTS: raceincome_2023.rda
# OUTPUTS: None.
################################################################################

# Load in race and income data from 2023

raceincome_2023 <- load("data/processed/raceincome_rda/raceincome_2023.rda")
raceincome2023 <- get(raceincome_2023)

####UNITED STATES####

# Create income groups
USincomegroup_2023 <- raceincome2023 %>%
  mutate(
    income_group = case_when(
      income_decile %in% 0:3 ~ "Low",
      income_decile %in% 4:7 ~ "Mid",
      income_decile %in% 8:10 ~ "High",
      TRUE ~ NA_character_
    )
  )

# Calculate racial shares per income group
US_raceShares_byIncomeDecile <- USincomegroup_2023 %>%
  filter(
    !is.na(income_group),
    !is.na(race_ethnicity),
    race_ethnicity != "Other/Unknown"
  ) %>%
  group_by(income_group, race_ethnicity) %>%
  summarise(pop = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop") %>%
  group_by(income_group) %>%
  mutate(
    total_pop = sum(pop),
    race_share = pop / total_pop
  ) %>%
  select(income_group, race_ethnicity, race_share) %>%
  pivot_wider(names_from = race_ethnicity, values_from = race_share) %>%
  arrange(income_group)

View(US_raceShares_byIncomeDecile)

####VIRGINIA####

#Filter for Virginia

Va2023 <- raceincome2023 %>%
  filter(STATEFP == "51")

# Create income groups
Va2023 <- Va2023 %>%
  mutate(
    income_group = case_when(
      income_decile %in% 0:3 ~ "Low",
      income_decile %in% 4:7 ~ "Mid",
      income_decile %in% 8:10 ~ "High",
      TRUE ~ NA_character_
    )
  )

# Calculate racial shares per income group
Va_raceShares_byIncomeDecile <- Va2023 %>%
  filter(
    !is.na(income_group),
    !is.na(race_ethnicity),
    race_ethnicity != "Other/Unknown"
  ) %>%
  group_by(income_group, race_ethnicity) %>%
  summarise(pop = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop") %>%
  group_by(income_group) %>%
  mutate(
    total_pop = sum(pop),
    race_share = pop / total_pop
  ) %>%
  select(income_group, race_ethnicity, race_share) %>%
  pivot_wider(names_from = race_ethnicity, values_from = race_share) %>%
  arrange(income_group)

View(Va_raceShares_byIncomeDecile)

####CVILLE + AC####

# Filter for Charlottesville + Albemarle
CvilleAC2023 <- raceincome2023 %>%
  filter(
    STATEFP == "51" & COUNTYFP %in% c("540", "003")
  )

# Create income groups
CvilleAC2023 <- CvilleAC2023 %>%
  mutate(
    income_group = case_when(
      income_decile %in% 0:3 ~ "Low",
      income_decile %in% 4:7 ~ "Mid",
      income_decile %in% 8:10 ~ "High",
      TRUE ~ NA_character_
    )
  )

# Calculate racial shares per income group
CvilleAC_raceShares_byIncomeDecile <- CvilleAC2023 %>%
  filter(
    !is.na(income_group),
    !is.na(race_ethnicity),
    race_ethnicity != "Other/Unknown"
  ) %>%
  group_by(income_group, race_ethnicity) %>%
  summarise(pop = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop") %>%
  group_by(income_group) %>%
  mutate(
    total_pop = sum(pop),
    race_share = pop / total_pop
  ) %>%
  select(income_group, race_ethnicity, race_share) %>%
  pivot_wider(names_from = race_ethnicity, values_from = race_share) %>%
  arrange(income_group)

View(CvilleAC_raceShares_byIncomeDecile)