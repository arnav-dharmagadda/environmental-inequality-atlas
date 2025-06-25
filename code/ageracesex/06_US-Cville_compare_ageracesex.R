################################################################################
# FILE: 06_US-Cville_compare_ageracesex.R
# PURPOSE:generate figure that compare US with Cville
# AUTHOR: Josie Fischman
# CREATED: June 20th, 2025
################################################################################
# INPUTS: nat_ageracesex_2023.rda, race_summary_filtered
# OUTPUTS: 
################################################################################
ageracesex_2023 <- load("/Users/jfischman/Library/CloudStorage/OneDrive-BowdoinCollege/Documents/GitHub/environmental-inequality-atlas/data/processed/ageracesex_rda/nat_ageracesex_2023.rda")
ageracesex2023 <- get(ageracesex_2023)
View(ageracesex2023)
# separate into race income deciles

age_summary2023 <- ageracesex2023 %>%
  filter(
    !is.na(age_group)
    #!is.na(income_decile)
  ) %>%
  group_by(age_group) %>%
  summarise(
    total_count = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )
#US Summary
us_arssummary <- ageracesex2023 %>%
  filter(!is.na(age_group), !age_group %in% c(NA, "NA", "Unknown", "Other/Unknown"), age_group != 0) %>%
  group_by(region = "United States", age = age_group) %>%
  summarise(
    total_count = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )
# CVILLE Summary
cville_arssummary <- ageracesex2023 %>%
  filter(
    STATEFP == "51",
    COUNTYFP %in% c("540", "003"),
    !is.na(age_group),
    !age_group %in% c(NA, "NA", "Unknown", "Other/Unknown"),
    age_group != 0
  ) %>%
  group_by(region = "Charlottesville/Albemarle", age = age_group) %>%
  summarise(
    total_count = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )
#combine the two
combined_summary <- bind_rows(us_summary, cville_summary)

#graph
age_summary <- ageracesex2023 %>%
  filter(
    !is.na(age_group),
    age_group != "Missing Age",
    !is.na(n_noise_postprocessed)
  ) %>%
  mutate(region = case_when(
    STATEFP == "51" & COUNTYFP %in% c("540", "003") ~ "Charlottesville/Albemarle",
    TRUE ~ "United States"
  )) %>%
  group_by(region, age = age_group) %>%
  summarise(
    total_count = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

age_prop <- age_summary %>%
  group_by(region) %>%
  mutate(share = total_count / sum(total_count)) %>%
  ungroup()
age_prop$age <- factor(age_prop$age, levels = c("Under 18", "19-65", "Over 65"))
qw <- ggplot(age_prop, aes(x = factor(age), y = share, fill = region)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9) +
  scale_fill_manual(values = c("United States" = "#FFA500", "Charlottesville/Albemarle" = "#232d4b")) +
  labs(
    title = "Age Distribution: U.S. vs. Charlottesville/Albemarle",
    x = "Age",
    y = "Share of Total Population",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("output/ageracesex/03_charts/Age Distribution: U.S. vs. CharlottesvilleAlbemarle.png", plot = qw, width = 6, height = 4)

View(age_prop)
