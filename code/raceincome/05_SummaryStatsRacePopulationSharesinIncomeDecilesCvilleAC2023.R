################################################################################
# FILE: 05_SummaryStatsRacePopulationSharesinIncomeDecilesCvilleAC2023.R
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

# 2. Aggregate postprocessed population by race and income group
race_income_group_US2023 <- USincomegroup_2023 %>%
  filter(
    !is.na(race_ethnicity),
    !is.na(income_group)
  ) %>%
  group_by(race_ethnicity, income_group) %>%
  summarise(
    pop = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Get total population by race (denominator)
race_totals_US2023 <- race_income_group_US2023 %>%
  group_by(race_ethnicity) %>%
  summarise(
    total_pop = sum(pop),
    .groups = "drop"
  )

# 4. Compute share of each race's population in each income group
race_shares_long_US2023 <- race_income_group_US2023 %>%
  left_join(race_totals_US2023, by = "race_ethnicity") %>%
  mutate(
    share_within_race = pop / total_pop
  ) %>%
  select(income_group, race_ethnicity, share_within_race)

# 5. Pivot: income groups as rows, races as columns
race_shares_wide_US2023 <- race_shares_long_US2023 %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = share_within_race
  ) %>%
  arrange(factor(income_group, levels = c("Low", "Mid", "High")))

View(race_shares_wide_US2023)

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

# 2. Aggregate postprocessed population by race and income group
race_income_group_Va2023 <- Va2023 %>%
  filter(
    !is.na(race_ethnicity),
    !is.na(income_group),
  ) %>%
  group_by(race_ethnicity, income_group) %>%
  summarise(
    pop = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Get total population by race (denominator)
race_totals_Va2023 <- race_income_group_Va2023 %>%
  group_by(race_ethnicity) %>%
  summarise(
    total_pop = sum(pop),
    .groups = "drop"
  )

# 4. Compute share of each race's population in each income group
race_shares_long_Va2023 <- race_income_group_Va2023 %>%
  left_join(race_totals_Va2023, by = "race_ethnicity") %>%
  mutate(
    share_within_race = pop / total_pop
  ) %>%
  select(income_group, race_ethnicity, share_within_race)

# 5. Pivot: income groups as rows, races as columns
race_shares_wide_Va2023 <- race_shares_long_Va2023 %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = share_within_race
  ) %>%
  arrange(factor(income_group, levels = c("Low", "Mid", "High")))

View(race_shares_wide_Va2023)

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

# 2. Aggregate postprocessed population by race and income group
race_income_group_CvilleAC2023 <- CvilleAC2023 %>%
  filter(
    !is.na(race_ethnicity),
    !is.na(income_group),
  ) %>%
  group_by(race_ethnicity, income_group) %>%
  summarise(
    pop = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Get total population by race (denominator)
race_totals_CvilleAC2023 <- race_income_group_CvilleAC2023 %>%
  group_by(race_ethnicity) %>%
  summarise(
    total_pop = sum(pop),
    .groups = "drop"
  )

# 4. Compute share of each race's population in each income group
race_shares_long_CvilleAC2023 <- race_income_group_CvilleAC2023 %>%
  left_join(race_totals_CvilleAC2023, by = "race_ethnicity") %>%
  mutate(
    share_within_race = pop / total_pop
  ) %>%
  select(income_group, race_ethnicity, share_within_race)

# 5. Pivot: income groups as rows, races as columns
race_shares_wide_CvilleAC2023 <- race_shares_long_CvilleAC2023 %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = share_within_race
  ) %>%
  arrange(factor(income_group, levels = c("Low", "Mid", "High")))

View(race_shares_wide_CvilleAC2023)
