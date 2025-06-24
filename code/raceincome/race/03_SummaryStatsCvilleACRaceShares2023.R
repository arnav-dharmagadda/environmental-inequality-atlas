################################################################################
# FILE: 03_SummaryStatsCvilleACRaceShares2023.R
# PURPOSE: Use gridded EIF data for race/income in 2023 to generate summary 
# statistics on population race shares for U.S., Virginia, and Cville/AC. 
# Create table with raw population numbers and shares.
# Calculate percentage difference in white shares between 
# Charlottesville/Albemarle County and Virginia and the United States. 
# AUTHOR: Elizabeth Shiker
# CREATED: June 11th, 2025
################################################################################
# INPUTS: raceincome_2023.rda
# OUTPUTS: race_summary_table.xlsx
################################################################################

# Load in race and income data from 2023

raceincome_2023 <- load("data/processed/raceincome_rda/nat_raceincome_2023.rda")
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
    US_raw = round(sum(US_raw, na.rm = TRUE)),
    US_share = round(sum(US_share, na.rm = TRUE), 1),
    VA_raw = round(sum(VA_raw, na.rm = TRUE)),
    VA_share = round(sum(VA_share, na.rm = TRUE), 1),
    CvilleAC_raw = round(sum(CvilleAC_raw, na.rm = TRUE)),
    CvilleAC_share = round(sum(CvilleAC_share, na.rm = TRUE), 1)
  )

combined_totals_display_with_total <- bind_rows(combined_totals_display, totals_row)%>%
  mutate(
    US_raw = comma(round(US_raw)),
    VA_raw = comma(round(VA_raw)),
    CvilleAC_raw = comma(round(CvilleAC_raw)),
    US_share = paste0(round(US_share * 100, 1), "%"),
    VA_share = paste0(round(VA_share * 100, 1), "%"),
    CvilleAC_share = paste0(round(CvilleAC_share * 100, 1), "%")
  )

combined_totals_display_with_total <- combined_totals_display_with_total %>%
  select(
    race,
    CvilleAC_raw, CvilleAC_share,
    VA_raw, VA_share,
    US_raw, US_share
  )

combined_totals_display_with_total <- combined_totals_display_with_total %>%
  rename_with(~ gsub("_", " ", .x))

View(combined_totals_display_with_total)

write.csv(combined_totals_display_with_total, "output/raceincome/race/race_summaryTable_2023.csv", row.names = FALSE)


##### How much more white is Cville/AC than VA or the US? #####

# Extract White share values
white_shares <- combined_totals %>%
  filter(race == "White") %>%
  summarise(
    CvilleAC_share = CvilleAC_share,
    VA_share = VA_share,
    US_share = US_share
  )

# Calculate percent difference
percent_more_white_va <- ((white_shares$CvilleAC_share - white_shares$VA_share) / white_shares$VA_share) * 100
percent_more_white_us <- ((white_shares$CvilleAC_share - white_shares$US_share) / white_shares$US_share) * 100

# Print the results with rounding
cat("Charlottesville and Albemarle County are", round(percent_more_white_va, 1), "% more white than Virginia. \n")
cat("Charlottesville and Albemarle County are", round(percent_more_white_us, 1), "% more white than the United States. \n")
  


