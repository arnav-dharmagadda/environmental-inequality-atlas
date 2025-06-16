################################################################################
# FILE: 04_merge_ageracesex_raceincome.R
# PURPOSE: Merge race-income and age-race-sex files to create a combined data
# set.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 9th, 2025
################################################################################
# INPUTS: None.
# OUTPUTS: ageracesex_income_year.rda/.dta
################################################################################

#### YEAR LONG FILE ####

ageracesex_income_year <- ageracesex_year_long %>%
  left_join(raceincome_year_long,   by = c("grid_lon", "grid_lat", "year")) %>%
  rename_with(~ str_replace_all(., c("\\.x$" = "", "\\.y$" = "_y")))

save(ageracesex_income_year, file = paste0(processed_path, "ageracesex_income_year.rda"))
write_dta(ageracesex_income_year, paste0(processed_path, "ageracesex_income_year.dta"))
