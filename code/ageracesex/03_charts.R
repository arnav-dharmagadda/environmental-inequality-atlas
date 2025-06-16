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

plot_data <- data_2023 %>%
  summarise(
    Black_over_65 = sum(Black_over_65, na.rm = TRUE),
    White_bet_19_65 = sum(White_bet_19_65, na.rm = TRUE),
    Black_bet_19_65 = sum(Black_bet_19_65, na.rm = TRUE),
    Black_under_18 = sum(Black_under_18, na.rm = TRUE),
    White_over_65 = sum(White_over_65, na.rm = TRUE),
    White_under_18 = sum(White_under_18, na.rm = TRUE),
    other_race_under_18 = sum(other_race_under_18, na.rm = TRUE),
    other_race_bet_19_65 = sum(other_race_bet_19_65, na.rm = TRUE),
    other_race_over_65 = sum(other_race_over_65, na.rm = TRUE),
    Asian_bet_19_65 = sum(Asian_bet_19_65, na.rm = TRUE),
    Hispanic_bet_19_65 = sum(Hispanic_bet_19_65, na.rm = TRUE),
    Hispanic_under_18 = sum(Hispanic_under_18, na.rm = TRUE),
    Asian_under_18 = sum(Asian_under_18, na.rm = TRUE),
    AIAN_bet_19_65 = sum(AIAN_bet_19_65, na.rm = TRUE),
    other_race_na_age = sum(other_race_na_age, na.rm = TRUE),
    Hispanic_over_65 = sum(Hispanic_over_65, na.rm = TRUE),
    AIAN_under_18 = sum(AIAN_under_18, na.rm = TRUE),
    AIAN_over_65 = sum(AIAN_over_65, na.rm = TRUE),
    Asian_over_65 = sum(Asian_over_65, na.rm = TRUE),
    Black_na_age = sum(Black_na_age, na.rm = TRUE),
    White_na_age = sum(White_na_age, na.rm = TRUE),
    .groups = "drop"
  )

plot_data_long <- plot_data %>%
  st_drop_geometry() %>% 
  pivot_longer(
    everything(),
    names_to = "group",
    values_to = "population_count"
  ) %>%
  separate(group, into = c("race", "age_group"), sep = "_(?=under|bet|over|na)", remove = FALSE) %>%
  mutate(
    age_group = case_when(
      age_group == "under_18" ~ "Under 18",
      age_group == "bet_19_65" ~ "19-65",
      age_group == "over_65" ~ "Over 65",
      age_group == "na_age" ~ "NA Age",
      TRUE ~ age_group
    ),
    race = case_when(
      race == "other" ~ "Other/Unknown",
      race == "AIAN" ~ "AIAN",
      TRUE ~ tools::toTitleCase(race)
    )
  )

plot_data_long <- plot_data_long %>%
  filter(
    race != "Other_race",
    age_group != "NA Age"
  )

plot_data_long$age_group <- factor(plot_data_long$age_group, levels = c("Under 18", "19-65", "Over 65"))

ggplot(plot_data_long, aes(x = race, y = population_count, fill = race)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    aes(group = age_group)
  ) +
  geom_text(
    aes(label = age_group, group = age_group),
    position = position_dodge(width = 0.8),
    vjust = -0.4,  # raise text slightly above bar
    size = 3.5,
    color = "black"
  ) +
  labs(
    title = "Age Distributions by Race",
    x = "Race",
    y = "Population Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
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
