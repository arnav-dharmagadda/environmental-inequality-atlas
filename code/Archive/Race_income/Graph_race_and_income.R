###################################################################
# Purpose: initial visualizations of race and income gridded eif data
# Author: Josie Fischman
# Date created: 6/4/2025
###################################################################
# Inputs: raceincome_wide
# Outputs: Income Decile Distribution by Race.png, Income Distribution by Race Facet.png, Income Decile Composition by Race.png, Income Distribution by Race in Charlottesville.png, Income Distribution by Race (proportional) in Charlottesville.png, Share of Race Group in 2nd, 5th, and 10th Income Deciles.png 
###################################################################
library(ggplot2)
library(RColorBrewer)
library(prettymapr)
library(sf)
library(stringr)
#change into spatial data
raceincome_sf <- st_as_sf(
  raceincome_wide,
  coords = c("grid_lon", "grid_lat"),
  crs = 4326  # WGS 84 (standard GPS lat/lon)
)
#filter year 2023 and convert to long
raceincome_long <- raceincome_sf %>%
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
#make it a grid
# Step 1: Convert to sf POINT object
raceincome_2023 <- st_as_sf(raceincome_2023, coords = c("grid_lon", "grid_lat"), crs = 4326)
# Step 2: Get expanded bounding box to fully contain all grid cells
bbox <- st_bbox(raceincome_2023)
bbox_expanded <- bbox
bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - 0.005
bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + 0.005
# Step 3: Create grid polygons at 0.01° resolution covering the area
grid_polygons <- st_make_grid(
  cellsize = c(0.01, 0.01),
  offset = c(bbox_expanded["xmin"], bbox_expanded["ymin"]),
  n = c(ceiling((bbox_expanded["xmax"] - bbox_expanded["xmin"]) / 0.01),
        ceiling((bbox_expanded["ymax"] - bbox_expanded["ymin"]) / 0.01)),
  crs = 4326,
  what = "polygons"
)
# Step 4: Convert grid to sf and join with your original data (spatial match)
grid_sf <- st_sf(geometry = grid_polygons)
# Step 5: Spatial join: match points to polygons (assumes 1 point per cell)
raceincome_2023 <- st_join(grid_sf, raceincome_2023, join = st_contains)

#filter for decile and race
# Separate into race and decile
raceincome_long <- raceincome_long %>%
  mutate(
    race = gsub("_.*", "", variable),
    decile = gsub(".*_(\\d+)_.*", "\\1", variable),
    decile = as.integer(decile)
  ) %>%
  filter(race != "other" & race != "Other" & !is.na(decile))
#summarize by counts
race_summary <- raceincome_long %>%
  group_by(race, decile) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")

#plot income deciles by race

p <- ggplot(race_summary, aes(x = factor(decile), y = total_count, fill = race)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = paste("Income Decile Distribution by Race in", year),
    x = "Income Decile (0 = lowest, 9 = highest)",
    y = "Number of People"
  ) +
  theme_minimal()

ggsave("output/raceincome/Income Decile Distribution by Race.png", plot = p, width = 6, height = 4)

#facet
q <- ggplot(race_summary, aes(x = factor(decile), y = total_count)) +
  geom_col(fill = "#1f78b4") +
  facet_wrap(~ race, scales = "free_y") +
  labs(
    title = paste("Income Distribution by Race in", year),
    x = "Income Decile",
    y = "Number of People"
  ) +
  theme_minimal()
ggsave("output/raceincome/Income Distribution by Race Facet.png", plot = q, width = 6, height = 4)



#race distribution by income decile ungrouped
r <- ggplot(race_summary, aes(x = race, y = total_count, fill = factor(decile))) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "YlGnBu", name = "Income Decile") +
  labs(
    title = "Income Decile Composition by Race",
    x = "Race",
    y = "Number of People"
  ) +
  theme_minimal()
ggsave("output/raceincome/Income Decile Composition by Race.png", plot = r, width = 6, height = 4)

#race distribution by income decile grouped

race_summary_grouped <- race_summary %>%
  mutate(
    decile_group = case_when(
      decile %in% 0:3 ~ "Low (0–3)",
      decile %in% 4:7 ~ "Mid (4–7)",
      decile %in% 8:9 ~ "High (8–9)"
    )
  ) %>%
  group_by(race, decile_group) %>%
  summarise(total_count = sum(total_count, na.rm = TRUE), .groups = "drop")

race_summary_grouped$decile_group <- factor(
  race_summary_grouped$decile_group,
  levels = c("Low (0–3)", "Mid (4–7)", "High (8–9)")
)

s <- ggplot(race_summary_grouped, aes(x = race, y = total_count, fill = decile_group)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2", name = "Income Group") +
  labs(
    title = "Income Distribution by Race in Charlottesville/Albemarle",
    x = "Race",
    y = "Number of People"
  ) +
  theme_minimal()
ggsave("output/raceincome/Income Distribution by Race in Charlottesville.png", plot = s, width = 6, height = 4)


#Income dist by Race (share of each race)
race_summary_grouped_share <- race_summary_grouped %>%
  group_by(race) %>%
  mutate(share = total_count / sum(total_count, na.rm = TRUE)) %>%
  ungroup()

race_summary_grouped_share$decile_group <- factor(
  race_summary_grouped_share$decile_group,
  levels = c("Low (0–3)", "Mid (4–7)", "High (8–9)")
)

t <- ggplot(race_summary_grouped_share, aes(x = race, y = share, fill = decile_group)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2", name = "Income Group") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Income Distribution by Race (Proportional)",
    x = "Race",
    y = "Share of Group"
  ) +
  theme_minimal()

ggsave("output/raceincome/Income Distribution by Race (proportional) in Charlottesville.png", plot = t, width = 6, height = 4)

#income dist in 1st, 5, and 10th decile
selected_deciles <- c(1, 4, 9)

race_summary_selected <- race_summary %>%
  filter(decile %in% selected_deciles)

#calculate share by race

race_summary_selected_share <- race_summary_selected %>%
  group_by(race) %>%
  mutate(share = total_count / sum(total_count, na.rm = TRUE)) %>%
  ungroup()

#rename 
race_summary_selected_share$decile <- factor(
  race_summary_selected_share$decile,
  levels = c(1, 4, 9),
  labels = c("1st Decile", "5th Decile", "10th Decile")
)
#plot
u <- ggplot(race_summary_selected_share, aes(x = race, y = share, fill = decile)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2", name = "Household Income Decile") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Share of Race Group in 2nd, 5th, and 10th Income Deciles",
    x = "Race",
    y = "Share of Group"
  ) +
  theme_minimal()
ggsave("output/raceincome/Share of Race Group in 2nd, 5th, and 10th Income Deciles.png", plot = u, width = 6, height = 4)

#map?

#merged3_sf <- merged3_sf %>%
# mutate(
#   white_low = rowSums(across(c(n_noise_White_0, n_noise_White_1, n_noise_White_2, n_noise_White_3), as.numeric), na.rm = TRUE),
#  white_mid = rowSums(across(c(n_noise_White_4, n_noise_White_5, n_noise_White_6, n_noise_White_7), as.numeric), na.rm = TRUE),
#  white_high = rowSums(across(c(n_noise_White_8, n_noise_White_9), as.numeric), na.rm = TRUE)
# )

#merged3_sf <- merged3_sf %>%
# mutate(
#  white_income_group = case_when(
#    white_low + white_mid + white_high == 0 ~ NA_character_,  # no White residents → NA
#   white_low >= white_mid & white_low >= white_high ~ "Low (0–3)",
#   white_mid >= white_low & white_mid >= white_high ~ "Mid (4–7)",
#   white_high >= white_low & white_high >= white_mid ~ "High (8–9)",
#  TRUE ~ NA_character_
#)
# )
#table(merged3_sf$white_income_group, useNA = "always")
#merged3_sf$white_income_group <- factor(
#  merged3_sf$white_income_group,
# levels = c("Low (0–3)", "Mid (4–7)", "High (8–9)")
#)
#ggplot(merged3_sf) +
# geom_sf(aes(fill = white_income_group), color="white", size = 2) +
#scale_fill_manual(
#  values = c(
#    "Low (0–3)" = "#d73027",
#   "Mid (4–7)" = "#fee08b",
#   "High (8–9)" = "#1a9850"
# ),
# na.value = "white",
# name = "White Income Group"
#) +
# labs(
#  title = "Dominant Income Group of White Residents by Location",
# fill = "Income Group"
# ) +
# theme_minimal()
#map for black people
#merged3_sf <- merged3_sf %>%
#  mutate(
#   black_low = rowSums(across(c(n_noise_Black_0, n_noise_Black_1, n_noise_Black_2, n_noise_Black_3), as.numeric), na.rm = TRUE),
#  black_mid = rowSums(across(c(n_noise_Black_4, n_noise_Black_5, n_noise_Black_6, n_noise_Black_7), as.numeric), na.rm = TRUE),
#  black_high = rowSums(across(c(n_noise_Black_8, n_noise_Black_9), as.numeric), na.rm = TRUE)
# )

#merged3_sf <- merged3_sf %>%
# mutate(
#  black_income_group = case_when(
#    black_low + black_mid + black_high == 0 ~ NA_character_,  # no White residents → NA
#   black_low >= black_mid & black_low >= black_high ~ "Low (0–3)",
#   black_mid >= black_low & black_mid >= black_high ~ "Mid (4–7)",
#   black_high >= black_low & black_high >= black_mid ~ "High (8–9)",
#   TRUE ~ NA_character_
# )
# )
#table(merged3_sf$black_income_group, useNA = "always")
#merged3_sf$black_income_group <- factor(
#  merged3_sf$black_income_group,
# levels = c("Low (0–3)", "Mid (4–7)", "High (8–9)")
#)


#ggplot(merged3_sf) +
#  annotation_map_tile(type = "cartolight", zoom = NULL) +
#  geom_sf(aes(fill = black_income_group), color="white", size = 2) +
# scale_fill_manual(
#   values = c(
#     "Low (0–3)" = "#d73027",
#    "Mid (4–7)" = "#fee08b",
#    "High (8–9)" = "#1a9850"
#  ),
#  na.value = NA,
#  name = "Black Income Group"
# ) +
# labs(
#  title = "Dominant Income Group of Black Residents by Location",
#  fill = "Income Group"
#) +
# theme_minimal()

#plot each individual race

