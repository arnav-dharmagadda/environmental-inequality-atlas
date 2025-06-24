################################################################################
# FILE: 04_CvilleAC_VA_US_RaceShares2023.R
# PURPOSE: Run after 03_SummaryStatsCvilleACRaceShares2023.R to plot 2023 racial
# population shares for Cville/AC, VA, and US in a bar chart.
# AUTHOR: Elizabeth Shiker
# CREATED: June 16th, 2025
################################################################################
# INPUTS: combined_totals_display from 03_SummaryStatsCvilleACRaceShares2023.R
# OUTPUTS: CvilleAC_VA_US_RaceShares_2023.png
################################################################################

# Reshape data to long format for plotting
plot_data <- combined_totals_display %>%
  select(race, US_share, VA_share, CvilleAC_share) %>%
  pivot_longer(
    cols = c(US_share, VA_share, CvilleAC_share),
    names_to = "Region",
    values_to = "Share"
  ) %>%
  mutate(Region = recode(Region,
                         "US_share" = "United States",
                         "VA_share" = "Virginia",
                         "CvilleAC_share" = "Charlottesville/Albemarle"))

# Bar plot
ggsave(
  "output/raceincome/race/CvilleAC_VA_US_RaceShares_2023.png",
  plot = ggplot(plot_data, aes(x = race, y = Share, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Racial Composition: Charlottesville/Albemarle County vs Virginia vs U.S. (2023)",
    x = "Race/Ethnicity",
    y = "Population Share",
    fill = "Region"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  width = 8, height = 6, dpi = 300
)
