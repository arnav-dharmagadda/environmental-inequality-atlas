################################################################################
# FILE: 04_time_series.R
# PURPOSE: This script generates time series of gridded EIF population data.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 5th, 2025
################################################################################
# INPUTS: 
# OUTPUTS: 
################################################################################

data_collapsed <- data_all %>%
  group_by(year) %>%
  summarise(across(
    c(AIAN, Asian, Black, Hispanic, NA_race, White, other_race, under_18, bet_19_65, over_65, total),
    ~ sum(.x, na.rm = TRUE)
  )) %>%
  mutate(
    young_dep_ratio = under_18 / bet_19_65,
    old_dep_ratio = over_65 / bet_19_65,
    dep_ratio = (under_18 + over_65) / bet_19_65
  ) %>%
  ungroup()

data_collapsed <- data_collapsed %>%
  mutate(
    shade = if_else(year <= 2004, 1, 0),
    ymin = 0,
    ymax_count = if_else(year <= 2004, 150000, NA_real_),
    AIAN_share = AIAN / total,
    Asian_share = Asian / total,
    Black_share = Black / total,
    Hispanic_share = Hispanic / total,
    NA_race_share = NA_race / total,
    White_share = White / total,
    other_race_share = other_race / total,
    ymax_share = if_else(year <= 2004, 1, NA_real_)
  )

ggsave(paste0(ts_output, "race_count.jpeg"))

ggplot(data_collapsed, aes(x = year)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax_count), 
              fill = "gray80", alpha = 0.5, data = subset(data_collapsed, shade == 1)) +
  geom_line(aes(y = White, color = "White"), linewidth = 1.2) +
  geom_line(aes(y = Black, color = "Black"), linewidth = 1.2) +
  geom_line(aes(y = Hispanic, color = "Hispanic"), linewidth = 1.2) +
  geom_line(aes(y = Asian, color = "Asian"), linewidth = 1.2) +
  scale_color_manual(values = c("White" = "blue", "Black" = "red", 
                                "Hispanic" = "green", "Asian" = "orange")) +
  labs(title = "Population Trends by Race/Ethnicity",
       subtitle = "1999–2023",
       x = "Year",
       y = "Population Count",
       color = "Group") +
  theme_minimal()

ggplot(data_collapsed, aes(x = year)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax_share), 
              fill = "gray80", alpha = 0.5, data = subset(data_collapsed, shade == 1)) +
  geom_line(aes(y = White_share, color = "White"), linewidth = 1.2) +
  geom_line(aes(y = Black_share, color = "Black"), linewidth = 1.2) +
  geom_line(aes(y = Hispanic_share, color = "Hispanic"), linewidth = 1.2) +
  geom_line(aes(y = Asian_share, color = "Asian"), linewidth = 1.2) +
  scale_color_manual(values = c("White" = "blue", "Black" = "red", 
                                "Hispanic" = "green", "Asian" = "orange")) +
  labs(title = "Population Trends by Race/Ethnicity",
       subtitle = "1999–2023",
       x = "Year",
       y = "Population Share",
       color = "Group") +
  theme_minimal()

ggsave(paste0(ts_output, "race_shares.jpeg"))

dep_data_long <- data_collapsed %>%
  pivot_longer(cols = c(dep_ratio, young_dep_ratio, old_dep_ratio),
               names_to = "type",
               values_to = "value")

ggplot(dep_data_long, aes(x = year, y = value, color = type)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c(
      "dep_ratio" = "black",
      "young_dep_ratio" = "dodgerblue",
      "old_dep_ratio" = "firebrick"
    ),
    labels = c(
      "dep_ratio" = "Total Dependency Ratio",
      "young_dep_ratio" = "Youth Dependency Ratio",
      "old_dep_ratio" = "Old-Age Dependency Ratio"
    )
  ) +
  labs(
    title = "Dependency Ratios Over Time",
    x = "Year",
    y = "Ratio",
    color = "Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(paste0(ts_output, "dep_ratio.jpeg"))
