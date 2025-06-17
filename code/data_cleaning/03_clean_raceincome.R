################################################################################
# FILE: 03_clean_raceincome.R
# PURPOSE: Download raceincome files as rda and dta for 1999-2023, save long and
# reshaped versions.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 9th, 2025
################################################################################
# INPUTS: gridded_eif_pop_raceincome files (1999-2023)
# OUTPUTS: raceincome_{YEAR}.dta/.rda, raceincome_combined.rda/.dta,
#          raceincome_year_long.rda/.dta
################################################################################

# Loop through 1999-2023

for (year in 1999:2023) {
  # Construct path to parquet file
  parquet_path <- paste0(data_path, "/raceincome/gridded_eif_pop_raceincome_", year, ".parquet")
  
  # Read the parquet file
  raceincome_year <- read_parquet(parquet_path)
  
  # Merge with filtered gridpoints
  merged_year <- left_join(gridpoints, raceincome_year, by = c("grid_lon", "grid_lat"))
  
  # Write to Stata .dta
  output_file <- paste0(dta_path_ri, "raceincome_", year, ".dta")
  write_dta(merged_year, output_file)
  
  # Write to .rda
  output_file_rda <- paste0(rda_path_ri, "raceincome_", year, ".rda")
  save(merged_year, file = output_file_rda)
}

for (year in 2023) {
  # Construct path to parquet file
  parquet_path <- paste0(data_path, "raceincome/gridded_eif_pop_raceincome_", year, ".parquet")
  
  # Read the parquet file
  raceincome_year <- read_parquet(parquet_path)
  
  # Merge with filtered gridpoints
  merged_year <- left_join(gridpoints_nat, raceincome_year, by = c("grid_lon", "grid_lat"))
  
  # Write to Stata .dta
  output_file <- paste0(dta_path_ri, "nat_raceincome_", year, ".dta")
  write_dta(merged_year, output_file)
  
  # Write to .rda
  output_file_rda <- paste0(rda_path_ri, "nat_raceincome_", year, ".rda")
  save(merged_year, file = output_file_rda)
}

# Append

rda_files <- list.files(path = rda_path_ri, pattern = "^raceincome_\\d{4}\\.rda$", full.names = TRUE)
all_raceincome <- list()

for (file in rda_files) {
  load(file)  # loads object called `merged_year`
  
  # Extract year from filename and add as a column
  year <- as.numeric(gsub(".*_(\\d{4})\\.rda$", "\\1", file))
  merged_year$year <- year
  
  all_raceincome[[length(all_raceincome) + 1]] <- merged_year
}

# Combine all into a single dataframe
raceincome_combined <- do.call(rbind, all_raceincome)

save(raceincome_combined, file = paste0(rda_path_ri, "raceincome_long.rda"))
write_dta(raceincome_combined, paste0(dta_path_ri, "raceincome_long.dta"))

# Change observation values for compatibility

raceincome_combined <- raceincome_combined %>%
  mutate(
    race_ethnicity = case_when(
      race_ethnicity == "Other/Unknown" ~ "other_race",
      TRUE ~ race_ethnicity
    ),
    income_decile = as.character(income_decile),  # fix type mismatch
    income_decile = case_when(
      income_decile == "0" ~ "inc_0",
      income_decile == "1" ~ "inc_1",
      income_decile == "2" ~ "inc_2",
      income_decile == "3" ~ "inc_3",
      income_decile == "4" ~ "inc_4",
      income_decile == "5" ~ "inc_5",
      income_decile == "6" ~ "inc_6",
      income_decile == "7" ~ "inc_7",
      income_decile == "8" ~ "inc_8",
      income_decile == "9" ~ "inc_9",
      income_decile == "10" ~ "inc_10",
      TRUE ~ income_decile
    ),
    race_income = paste(race_ethnicity, income_decile, sep = "_")
  )

# Reshape for raceincome variables

raceincome_wide <- raceincome_combined %>%
  select(year, grid_lon, grid_lat, STATEFP, COUNTYFP, GEOID, NAME, race_income, n_noise_postprocessed) %>%
  pivot_wider(
    names_from = race_income,
    values_from = n_noise_postprocessed
  )

# Reshape for income variables

income_wide <- raceincome_combined %>%
  group_by(year, grid_lon, grid_lat, income_decile) %>%
  summarise(total_inc_pop = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop") %>%
  select(year, grid_lon, grid_lat, income_decile, total_inc_pop) %>%
  pivot_wider(
    names_from = income_decile,
    values_from = total_inc_pop
  ) %>%
  rename(NA_inc = `NA`)

# Combine all into a single dataframe with year

raceincome_year_long <- raceincome_wide %>%
  left_join(income_wide, by = c("grid_lon", "grid_lat", "year"))

save(raceincome_year_long, file = paste0(rda_path_ri, "raceincome_long_year.rda"))

write_dta(raceincome_year_long, paste0(dta_path_ri, "raceincome_long_year.dta"))
