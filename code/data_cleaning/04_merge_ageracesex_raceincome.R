################################################################################
# FILE: 04_merge_ageracesex_raceincome.R
# PURPOSE: Merge race-income and age-race-sex files to create a combined data
# set.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 9th, 2025
################################################################################
# INPUTS: gridded_eif_pop_raceincome files (1999-2023)
# OUTPUTS: None.
################################################################################

#### YEAR LONG FILE ####

ageracesex_income_year <- ageracesex_year_long %>%
  left_join(raceincome_year_long,   by = c("grid_lon", "grid_lat", "year")) %>%
  rename_with(~ str_replace_all(., c("\\.x$" = "_x", "\\.y$" = "_y")))

save(ageracesex_income_year, file = paste0(processed_path, "ageracesex_income_year.rda"))
write_dta(ageracesex_income_year, paste0(processed_path, "ageracesex_income_year.dta"))

#### WIDE FILE ####

ageracesex_income <- ageracesex_wide %>%
  left_join(raceincome_wide, by = c("grid_lon", "grid_lat")) %>%
  rename_with(~ str_replace_all(., c("\\.x$" = "_x", "\\.y$" = "_y")))

save(ageracesex_income, file = paste0(processed_path, "ageracesex_income.rda"))
write_dta(ageracesex_income, paste0(processed_path, "ageracesex_income.dta"))
