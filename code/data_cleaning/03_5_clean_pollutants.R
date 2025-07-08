################################################################################
# FILE: 03_5_clean_pollutants.R
# PURPOSE: Download pollutants files as rda and dta for 1999-2023, save long and
# reshaped versions.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 9th, 2025
################################################################################
# INPUTS: gridded_eif_pop_pollutants files (1999-2023)
# OUTPUTS: ageracesex_{YEAR}.dta/.rda, ageracesex_combined.rda/.dta,
#          ageracesex_year_long.rda/.dta
################################################################################

rda_path_pol <- paste0(git_path, "data/processed/pollutants_rda/")

# Loop 1999 through 2023

for (year in 1999:2023) {
  # Construct path to parquet file
  parquet_path <- paste0(data_path, "/pollution/gridded_eif_pollutants_", year, ".parquet")
  
  # Read the parquet file
  pollutants_year <- read_parquet(parquet_path)
  
  # Merge with filtered gridpoints
  merged_year <- left_join(gridpoints, pollutants_year, by = c("grid_lon", "grid_lat"))

  # Replace NAs in numeric columns (e.g., population counts) with 0, but do not modify grid_lon or grid_lat
  num_cols <- sapply(merged_year, is.numeric)
  num_cols[names(num_cols) %in% c("grid_lon", "grid_lat")] <- FALSE
  merged_year[num_cols] <- lapply(merged_year[num_cols], function(x) ifelse(is.na(x), 0, x))
  
  # Write to .rda
  output_file_rda <- paste0(rda_path_pol, "pollutants_", year, ".rda")
  save(merged_year, file = output_file_rda)
}

# NATIONAL POLLUTANTS: Only use gridpoints_nat for national grid
for (year in 2023) {
  # Construct path to parquet file
  parquet_path <- paste0(data_path, "/pollution/gridded_eif_pollutants_", year, ".parquet")
  
  # Read the parquet file
  pollutants_year <- read_parquet(parquet_path)
  
  # Merge with national gridpoints only
  merged_year_nat <- left_join(gridpoints_nat, pollutants_year, by = c("grid_lon", "grid_lat"))

  # Replace NAs in numeric columns (except grid_lon/grid_lat) with 0
  num_cols_nat <- sapply(merged_year_nat, is.numeric)
  num_cols_nat[names(num_cols_nat) %in% c("grid_lon", "grid_lat")] <- FALSE
  merged_year_nat[num_cols_nat] <- lapply(merged_year_nat[num_cols_nat], function(x) ifelse(is.na(x), 0, x))
  
  # Write to .rda
  output_file_rda_nat <- paste0(rda_path_pol, "nat_pollutants_", year, ".rda")
  save(merged_year_nat, file = output_file_rda_nat)
}

# Append

rda_files <- list.files(path = rda_path_pol, pattern = "^pollutants_\\d{4}\\.rda$", full.names = TRUE)
all_pollutants <- list()

for (file in rda_files) {
  load(file)  # loads object called `merged_year`
  
  # Extract year from filename and add as a column
  year <- as.numeric(gsub(".*_(\\d{4})\\.rda$", "\\1", file))
  merged_year$year <- year
  
  all_pollutants[[length(all_pollutants) + 1]] <- merged_year

}

all_names <- unique(unlist(lapply(all_pollutants, names)))

# Add missing columns to each data frame
all_pollutants <- lapply(all_pollutants, function(df) {
  missing <- setdiff(all_names, names(df))
  for (col in missing) df[[col]] <- NA
  df <- df[all_names]  # reorder columns
  df
})

pollutants_combined <- do.call(rbind, all_pollutants)

save(pollutants_combined, file = paste0(rda_path_pol, "pollutants_long.rda"))
