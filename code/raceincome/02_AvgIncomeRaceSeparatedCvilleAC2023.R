################################################################################
# FILE: AvgIncomeRaceSeparatedCvilleAC2023.R
# PURPOSE: Use gridded EIF data to create a bar chart on average income decile by
# race/ethnicity in 2023, separated by Charlottesville and Albemarle County.
# AUTHOR: Elizabeth Shiker
# CREATED: June 9th, 2025
################################################################################
# INPUTS: raceincome_2023.rda
# OUTPUTS: avg_income_by_race_county_2023.png
################################################################################

# Load in race and income data from 2023

raceincome_2023 <- load("data/processed/raceincome_rda/raceincome_2023.rda")
raceincome2023 <- get(raceincome_2023)

#Filter for Cville/AC
raceincome2023 <- raceincome2023 %>%
  filter(
    STATEFP == "51" & COUNTYFP %in% c("540", "003")
  )

#Calculating average income for each race seperately for Cville and AC

avg_income_by_race_county <- raceincome2023 %>%
  filter(!is.na(race_ethnicity), race_ethnicity != "Other/Unknown") %>%
  group_by(COUNTYFP, race_ethnicity) %>%
  summarise(
    avg_income_decile = weighted.mean(income_decile, w = n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

avg_income_by_race_county <- avg_income_by_race_county %>%
  mutate(
    county_name = case_when(
      COUNTYFP == "540" ~ "Charlottesville",
      COUNTYFP == "003" ~ "Albemarle",
      TRUE ~ "Other"
    )
  )

# Save bar chart comparing average income decile by race in cville and AC

ggsave(
  "output/raceincome/avg_income_by_race_county_2023.png",
  plot = ggplot(avg_income_by_race_county, aes(x = reorder(race_ethnicity, -avg_income_decile), y = avg_income_decile, fill = county_name)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Average Income Decile by Race/Ethnicity (2023)",
      x = "Race/Ethnicity",
      y = "Average Income Decile (Weighted by Population)",
      fill = "County"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  width = 8,
  height = 6
)


