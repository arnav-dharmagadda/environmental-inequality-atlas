################################################################################
# FILE: Raceincome_summary
# PURPOSE: Generate summary statistics for the race income data
# AUTHOR: Josie Fischman
# CREATED: June 10th, 2025
################################################################################
# INPUTS: race_income_wide
# OUTPUTS: None.
################################################################################
library(dplyr)
library(tidyr)
#filter for 2023
raceincome_long <- raceincome_wide %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "variable",
    values_to = "count"
  ) %>%
  filter(!is.na(count)) %>%
  mutate(
    race = gsub("_.*", "", variable),
    decile = as.integer(str_extract(variable, "(?<=_inc_)\\d+")),
    year = as.integer(str_extract(variable, "\\d{4}$"))
  ) %>%
  filter(year == 2023, race != "other", !is.na(decile))
View(raceincome_long)
#find total population
total_pop <- sum(raceincome_long$count, na.rm = TRUE)

#find total population for each race

race_totals <- raceincome_long %>%
  group_by(race) %>%
  summarise(total = sum(count, na.rm = TRUE))

show(race_totals)

#find total population for each income decile

decile_totals <- raceincome_long %>%
  group_by(decile) %>%
  summarise(total = sum(count, na.rm = TRUE))
show(decile_totals)

#find total income decile combinations

incomedecile_totals <- raceincome_long %>%
  group_by(decile, race) %>%
  summarize(total = sum(count, na.rm = TRUE)) %>%
  pivot_wider(names_from = race, values_from = total, names_prefix = "race_")
show(incomedecile_totals)


#find percentages

race_summary <- raceincome_long %>%
  group_by(race) %>%
  summarise(
    count = sum(count, na.rm = TRUE)
  ) %>%
  mutate(percent_of_total = 100 * count / total_pop)

income_summary <- raceincome_long %>%
  group_by(decile) %>%
  summarise(
    count = sum(count, na.rm = TRUE)
  ) %>%
  mutate(percent_of_total = 100 * count / total_pop)


