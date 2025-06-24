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
combined_prop$race <- factor(combined_prop$race, levels = c("White", "Black", "Hispanic", "Asian", "AIAN"))
tk <- ggplot(combined_prop, aes(x = factor(decile), y = share, fill = region)) +
  geom_col(position = position_dodge(width = 1), width = 1) +
  facet_wrap(~ race, scales = "fixed") +
  scale_fill_manual(values = c("United States" = "orange", "Charlottesville/Albemarle" = "#232d4b")) +
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

#average income by race

avg_decile_by_race <- combined_summary %>%
  group_by(region, race) %>%
  summarise(
    avg_decile = weighted.mean(decile, total_count, na.rm = TRUE),
    .groups = "drop"
  )

ty <- ggplot(avg_decile_by_race, aes(x = race, y = avg_decile, fill = region)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("United States" = "#1f78b4", "Charlottesville/Albemarle" = "#FFA500")) +
  labs(
    title = "Average Income Decile by Race: U.S. vs. Charlottesville/Albemarle (2023)",
    x = "Race",
    y = "Average Income Decile",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
ggsave("output/raceincome/Average Income Decile by Race: U.S. vs. CharlottesvilleAlbemarle.png", plot = ty, width = 6, height = 4)

#average decile
avg_decile <- combined_summary %>%
  group_by(region) %>%
  summarise(
    avg_decile = weighted.mean(decile, total_count, na.rm = TRUE),
    .groups = "drop"
  )

tw <- ggplot(avg_decile, aes(x = region, y = avg_decile, fill = region)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("United States" = "#1f78b4", "Charlottesville/Albemarle" = "#FFA500")) +
  labs(
    title = "Average Income Decile: U.S. vs. Charlottesville/Albemarle (2023)",
    x = "Region",
    y = "Average Income Decile",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
ggsave("output/raceincome/Average Income Decile: U.S. vs. CharlottesvilleAlbemarle.png", plot = tw, width = 6, height = 4)

#graph just deciles

decile_summary <- raceincome2023 %>%
  filter(
    !is.na(income_decile),
    income_decile != 0,
    !is.na(n_noise_postprocessed)
  ) %>%
  mutate(region = case_when(
    STATEFP == "51" & COUNTYFP %in% c("540", "003") ~ "Charlottesville/Albemarle",
    TRUE ~ "United States"
  )) %>%
  group_by(region, decile = income_decile) %>%
  summarise(
    total_count = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

decile_prop <- decile_summary %>%
  group_by(region) %>%
  mutate(share = total_count / sum(total_count)) %>%
  ungroup()

qq <- ggplot(decile_prop, aes(x = factor(decile), y = share, fill = region)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9) +
  scale_fill_manual(values = c("United States" = "#FFA500", "Charlottesville/Albemarle" = "#232d4b")) +
  labs(
    title = "Income Decile Distribution: U.S. vs. Charlottesville/Albemarle",
    x = "Income Decile",
    y = "Share of Total Population",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("output/raceincome/Income Decile Distribution: U.S. vs. CharlottesvilleAlbemarle.png", plot = qq, width = 6, height = 4)

