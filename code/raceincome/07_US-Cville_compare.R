################################################################################
# FILE: 07_US_Cville_compare.R
# PURPOSE:generate figure that compare US with Cville
# AUTHOR: Josie Fischman
# CREATED: June 17th, 2025
################################################################################
# INPUTS: nat_raceincome_2023.rda, race_summary_filtered
# OUTPUTS: 
################################################################################

raceincome_2023 <- load("/Users/jfischman/Library/CloudStorage/OneDrive-BowdoinCollege/Documents/GitHub/environmental-inequality-atlas/data/processed/raceincome_rda/nat_raceincome_2023.rda")
raceincome2023 <- get(raceincome_2023)
View(raceincome2023)
# separate into race income deciles

race_summary2023 <- raceincome2023 %>%
  filter(
    !is.na(race_ethnicity),
    !is.na(income_decile)
  ) %>%
  group_by(race_ethnicity, income_decile) %>%
  summarise(
    total_count = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )
#US Summary
us_summary <- raceincome2023 %>%
  filter(!is.na(race_ethnicity), !is.na(income_decile), !race_ethnicity %in% c(NA, "NA", "Unknown", "Other/Unknown"), income_decile != 0) %>%
  group_by(region = "United States", race = race_ethnicity, decile = income_decile) %>%
  summarise(
    total_count = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )
# CVILLE Summary
cville_summary <- raceincome2023 %>%
  filter(
    STATEFP == "51",
    COUNTYFP %in% c("540", "003"),
    !is.na(race_ethnicity),
    !is.na(income_decile),
    !race_ethnicity %in% c(NA, "NA", "Unknown", "Other/Unknown"),
    income_decile != 0
  ) %>%
  group_by(region = "Charlottesville/Albemarle", race = race_ethnicity, decile = income_decile) %>%
  summarise(
    total_count = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )
#combine the two
combined_summary <- bind_rows(us_summary, cville_summary)

#Graph shares

combined_prop <- combined_summary %>%
  group_by(region, race) %>%
  mutate(share = total_count / sum(total_count)) %>%
  ungroup()

tk <- ggplot(combined_prop, aes(x = factor(decile), y = share, fill = region)) +
  geom_col(position = position_dodge(width = 1), width = 1) +
  facet_wrap(~ race, scales = "free_y") +
  scale_fill_manual(values = c("United States" = "#1f78b4", "Charlottesville/Albemarle" = "#FFA500")) +
  labs(
    title = "Income Decile Share by Race: U.S. vs. Charlottesville/Albemarle (2023)",
    x = "Income Decile",
    y = "Share of Race Group",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("output/raceincome/Income Decile Share by Race: U.S. vs. CharlottesvilleAlbemarle orange blue.png", plot = tk, width = 7, height = 4)


us_data <- combined_prop %>% filter(region == "United States") %>% mutate(alpha = 0.5)
cville_data <- combined_prop %>% filter(region == "Charlottesville/Albemarle") %>% mutate(alpha = 1)

tj <- ggplot() +
  # First: Cville bars (in the back)
  geom_col(data = cville_data, aes(x = factor(decile), y = share, fill = region, alpha = alpha), position = "identity", color = NA) +
  # Then: US bars (on top)
  geom_col(data = us_data, aes(x = factor(decile), y = share, fill = region, alpha = alpha), position = "identity", color = NA) +
  facet_wrap(~ race, scales = "free_y") +
  scale_fill_manual(
    values = c("United States" = "#1f78b4", "Charlottesville/Albemarle" = "#33a02c")
  ) +
  scale_alpha_identity() +
  labs(
    title = "Income Decile Share by Race: U.S. vs. Charlottesville/Albemarle",
    x = "Income Decile",
    y = "Share of Race Group",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("output/raceincome/Income Decile Share by Race: U.S. vs. CharlottesvilleAlbemarle.png", plot = tj, width = 6, height = 4)


