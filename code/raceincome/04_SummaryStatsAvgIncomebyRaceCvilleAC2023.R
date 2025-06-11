################################################################################
# FILE: SummaryStatsAvgIncomebyRaceCvilleAC2023.R
# PURPOSE: Use 2023 race/income data from the gridded EIF to generate summary 
# statistics for average income decile by race/ethnicity in the United States,
# Virginia, and Charlottesville and Albemarle County.
# AUTHOR: Elizabeth Shiker
# CREATED: June 11th, 2025
################################################################################
# INPUTS: raceincome_2023.rda
# OUTPUTS: None.
################################################################################

# Load in race and income data from 2023

raceincome_2023 <- load("data/processed/raceincome_rda/raceincome_2023.rda")
raceincome2023 <- get(raceincome_2023)

####United States####

#Calculating average income for each race in U.S.

US_avg_income_by_race <- raceincome2023 %>%
  filter(!is.na(race_ethnicity), race_ethnicity != "Other/Unknown") %>%
  group_by(race_ethnicity) %>%
  summarise(
    avg_income_decile = weighted.mean(income_decile, w = n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

View(US_avg_income_by_race)

####Virginia####

#Filter for Virginia

Va2023 <- raceincome2023 %>%
  filter(STATEFP == "51")

#Calculating average income for each race in Va

Va_avg_income_by_race <- Va2023 %>%
  filter(!is.na(race_ethnicity), race_ethnicity != "Other/Unknown") %>%
  group_by(race_ethnicity) %>%
  summarise(
    avg_income_decile = weighted.mean(income_decile, w = n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

View(Va_avg_income_by_race)

####Cville/AC####

#Filter for Cville/AC

CvilleAC2023 <- raceincome2023 %>%
  filter(
    STATEFP == "51" & COUNTYFP %in% c("540", "003")
  )

#Calculating average income for each race in Cville/AC

CvilleAC_avg_income_by_race <- CvilleAC2023 %>%
  filter(!is.na(race_ethnicity), race_ethnicity != "Other/Unknown") %>%
  group_by(race_ethnicity) %>%
  summarise(
    avg_income_decile = weighted.mean(income_decile, w = n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

View(CvilleAC_avg_income_by_race)
