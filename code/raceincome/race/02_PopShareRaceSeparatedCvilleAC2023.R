################################################################################
# FILE: PopShareRaceSeparatedCvilleAC2023.R
# PURPOSE: Use gridded EIF data to create a bar chart on population share by
# race/ethnicity in 2023, separated by Charlottesville and Albemarle County.
# AUTHOR: Elizabeth Shiker
# CREATED: June 10th, 2025
################################################################################
# INPUTS: raceincome_2023.rda
# OUTPUTS: population_share_by_race_county_2023.png
################################################################################

#Load in race and income 2023 data

raceincome_2023 <- load("data/processed/raceincome_rda/raceincome_2023.rda")
raceincome2023 <- get(raceincome_2023)

#Filter for Cville/AC
raceincome2023 <- raceincome2023 %>%
  filter(
    STATEFP == "51" & COUNTYFP %in% c("540", "003")
  )

# sum population in each grid by race

racesumbygrid <- raceincome2023 %>%
  filter(!is.na(race_ethnicity)) %>%
  group_by(grid_lon, grid_lat, COUNTYFP, race_ethnicity) %>%
  summarise(
    n_noise = sum(n_noise, na.rm = TRUE),
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

raceincome_2023_wide <- racesumbygrid %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = c(n_noise, n_noise_postprocessed),
    names_sep = "_"
  )

# Summarize postprocessed population by race and county
race_shares_by_county <- raceincome_2023_wide %>%
  mutate(
    county_name = case_when(
      COUNTYFP == "540" ~ "Charlottesville",
      COUNTYFP == "003" ~ "Albemarle",
      TRUE ~ "Other"
    )
  ) %>%
  # Sum pop by race within each county
  group_by(county_name) %>%
  summarise(across(starts_with("n_noise_postprocessed_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = starts_with("n_noise_postprocessed_"),
    names_to = "race",
    values_to = "total_population"
  ) %>%
  mutate(
    race = gsub("n_noise_postprocessed_", "", race)
  ) %>%
  filter(race != "Other/Unknown") %>%  # remove the group here
  group_by(county_name) %>%
  mutate(
    share = total_population / sum(total_population)
  ) %>%
  ungroup()

# Save plot of race shares in population separated by county in 2023
library(scales)

ggsave(
  "output/raceincome/race/population_share_by_race_county_2023.png",
  plot = ggplot(race_shares_by_county, aes(x = reorder(race, -share), y = share, fill = county_name)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Population Share by Race/Ethnicity and County (2023)",
      x = "Race/Ethnicity",
      y = "Share of County Population",
      fill = "County"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  width = 8, height = 6, dpi = 300
)