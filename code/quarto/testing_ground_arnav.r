file_path <- '/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/ageracesex_rda/ageracesex_year_long.rda'
load(file_path)
data_all <- ageracesex_year_long

data_all <- data_all %>%
  rowwise() %>%
  mutate(total = sum(c(M, F, NA_sex, na_sex), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    black_share = Black / total,
    white_share = White / total,
    hispanic_share = Hispanic / total,
    asian_share = Asian / total,
    male_share = M / total,
    female_share = F / total,
    under_18_share = under_18 / total,
    bet_19_65_share = bet_19_65 / total,
    over_65_share = over_65 / total,
  )

data_collapsed <- data_all %>%
  group_by(year) %>%
  summarise(across(
    c(AIAN, Asian, Black, Hispanic, NA_race, White, other_race, under_18, bet_19_65, over_65, total),
    ~ sum(.x, na.rm = TRUE)
  )) %>%
  mutate(
    under_18_share = under_18 / total,
    bet_19_65_share = bet_19_65 / total,
    over_65_share = over_65 / total
  ) %>%
  filter(year != 1999) %>%
  ungroup()

# Collapse to counts by year and age group
age_counts_long <- data_collapsed %>%
  select(year, under_18, bet_19_65, over_65) %>%
  pivot_longer(cols = c(under_18, bet_19_65, over_65),
               names_to = "type",
               values_to = "value")

# Plot counts by age group over time
ggplot(age_counts_long, aes(x = year, y = value, color = type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, alpha = 0.9) +
  scale_color_manual(
    values = c(
      "under_18" = "#A6CEE3",
      "bet_19_65" = "#601215",
      "over_65" = "#003F5C"
    ),
    labels = c(
      "under_18" = "Under 18",
      "bet_19_65" = "Ages 19-65",
      "over_65" = "Over 65"
    )
  ) +
  labs(
    title = "Population by Age Group Over Time",
    x = "Year",
    y = "Population Count"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    expand = c(0.02, 0),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2020, by = 5),
    minor_breaks = NULL,
    expand = c(0.02, 0)
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    # Axes
    axis.title = element_text(size = 12, color = "gray20"),
    axis.text = element_text(size = 10, color = "gray30"),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "gray20"),
    legend.key.width = unit(1.5, "cm"),
    legend.margin = margin(b = 15),
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),   # Remove vertical minor grid lines
    # Add solid x-axis line
    axis.line.x = element_line(color = "black", size = 0.5)
  )

