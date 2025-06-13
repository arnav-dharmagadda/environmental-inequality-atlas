################################################################################
# FILE: 06_dissimilarity.R
# PURPOSE: This script generates a dissimilarity index and map.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 12th, 2025
################################################################################
# INPUTS: 
# OUTPUTS: 
################################################################################

calculate_dissimilarity <- function(group_df, group1, group2) {
  A <- sum(group_df[[group1]], na.rm = TRUE)
  B <- sum(group_df[[group2]], na.rm = TRUE)
  
  if (A == 0 | B == 0) return(NA)  # Avoid divide-by-zero
  
  D <- 0.5 * sum(
    abs((group_df[[group1]] / A) - (group_df[[group2]] / B)),
    na.rm = TRUE
  )
  
  return(D)
}

data_all <- data_all %>%
  mutate(non_white = if_else(
    rowSums(!is.na(across(c(Black, AIAN, Asian, Hispanic)))) == 0,
    NA_real_,
    rowSums(across(c(Black, AIAN, Asian, Hispanic)), na.rm = TRUE)
  ))

dissimilarity_by_county_year <- data_all %>%
  group_by(NAME, year) %>%
  summarise(
    di_white_non_white = calculate_dissimilarity(pick(everything()), "White", "non_white"),
    di_white_black     = calculate_dissimilarity(pick(everything()), "White", "Black"),
    di_white_hispanic  = calculate_dissimilarity(pick(everything()), "White", "Hispanic"),
    di_white_asian     = calculate_dissimilarity(pick(everything()), "White", "Asian"),
    di_white_AIAN      = calculate_dissimilarity(pick(everything()), "White", "AIAN"),
    .groups = "drop"
  ) %>%
  st_drop_geometry()

total_dissimilarity_by_year <- data_all %>%
  group_by(year) %>%
  summarise(
    di_white_non_white = calculate_dissimilarity(pick(everything()), "White", "non_white"),
    di_white_black     = calculate_dissimilarity(pick(everything()), "White", "Black"),
    di_white_hispanic  = calculate_dissimilarity(pick(everything()), "White", "Hispanic"),
    di_white_asian     = calculate_dissimilarity(pick(everything()), "White", "Asian"),
    di_white_AIAN      = calculate_dissimilarity(pick(everything()), "White", "AIAN"),
    .groups = "drop"
  ) %>%
  mutate(NAME = "Total") %>%
  select(NAME, year, everything())

dissimilarity_combined <- bind_rows(dissimilarity_by_county_year, total_dissimilarity_by_year)

dissimilarity_long <- dissimilarity_combined %>%
  filter(NAME == "Total") %>%
  pivot_longer(
    cols = starts_with("di_white_"),
    names_to = "group_pair",
    values_to = "dissimilarity_index"
  )

library(ggplot2)

ggplot(dissimilarity_long %>% filter(NAME == "Total"), 
       aes(x = year, y = dissimilarity_index, color = group_pair)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c(
      "di_white_black" = "#1b9e77",
      "di_white_hispanic" = "#7570b3",
      "di_white_asian" = "#d95f02",
      "di_white_AIAN" = "#e7298a",
      "di_white_non_white" = "#66a61e"
    ),
    labels = c(
      "di_white_black" = "White vs Black",
      "di_white_hispanic" = "White vs Hispanic",
      "di_white_asian" = "White vs Asian",
      "di_white_AIAN" = "White vs AIAN",
      "di_white_non_white" = "White vs Non-White"
    )
  ) +
  labs(
    title = "Trends in Racial Residential Segregation (1999–2023)",
    subtitle = "Dissimilarity Index between White and Other Groups, U.S. Total",
    x = "Year",
    y = "Dissimilarity Index",
    color = "Group Pair"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.title = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )
