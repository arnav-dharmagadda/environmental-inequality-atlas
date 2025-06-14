################################################################################
# FILE: 03_maps.R
# PURPOSE: This script generates charts of gridded EIF population data.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 5th, 2025
################################################################################
# INPUTS: 
# OUTPUTS: 
################################################################################

# Age Distributions by Race

agerace_2023_long <- agerace_2023_long %>%
  filter(age_group %in% c("Under 18", "19-65", "Over 65"))

agerace_2023_long$age_group <- factor(
  agerace_2023_long$age_group,
  levels = c("Under 18", "19-65", "Over 65")
)

agerace_2023_long$race_ethnicity <- factor(
  agerace_2023_long$race_ethnicity,
  levels = c("White", "Black", "Hispanic", "Asian", "AIAN", "Other/Unknown")
)

plot_data <- agerace_2023_long %>%
  group_by(race_ethnicity, age_group) %>%
  summarise(population = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop")

ggplot(plot_data, aes(x = race_ethnicity, y = population, fill = age_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Age Distributions by Race",
       x = "Race",
       y = "Population Count",
       fill = "Age Group") +
  theme_minimal()

# Race Distributions by Age

ggplot(plot_data, aes(x = age_group, y = population, fill = race_ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Race Distributions by Age",
       x = "Age Group",
       y = "Population Count",
       fill = "Race/Ethnicity") +
  theme_minimal()

# Share Distributions

# Calculate total per race
race_totals <- plot_data %>%
  group_by(race_ethnicity) %>%
  summarise(total = sum(population, na.rm = TRUE))

# Join total back and compute share
df_shares <- plot_data %>%
  left_join(race_totals, by = "race_ethnicity") %>%
  mutate(share = population / total)

ggplot(df_shares, aes(x = age_group, y = share, fill = race_ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Age Group Share Within Each Race",
       x = "Age Group",
       y = "Share of Race Population",
       fill = "Race/Ethnicity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()
