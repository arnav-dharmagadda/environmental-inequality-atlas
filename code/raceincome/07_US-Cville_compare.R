################################################################################
# FILE: 07_US_Cville_compare.R
# PURPOSE:generate figure that compare US with Cville
# AUTHOR: Josie Fischman
# CREATED: June 17th, 2025
################################################################################
# INPUTS: nat_raceincome_2023.rda, race_summary_filtered
# OUTPUTS: 
################################################################################
install.packages("reactable")
install.packages("sparkline")
install.packages("htmltools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("purrr")
install.packages("scales")
install.packages("arrow")
install.packages("sf")
install.packages("webshot2")
library(dplyr)
library(tidyr)
library(purrr)
library(reactable)
library(sparkline)
library(htmltools)
library(scales)
library(arrow)
library(sf)
library(webshot2)
raceincome_2023 <- load("/Users/jfischman/Library/CloudStorage/Dropbox-TheLab/Josie Fischman/Github/environmental-inequality-atlas/data/processed/raceincome_rda/nat_raceincome_2024.rda")
raceincome2023 <- get(raceincome_2023)
View(raceincome2023)
# separate into race income deciles
raceincome2023 <- raceincome2023 %>%
  filter(
    !is.na(race_ethnicity),
    income_decile %in% 1:10,
    !tolower(trimws(race_ethnicity)) %in% c("na", "unknown", "other/unknown")
  )


race_summary2023 <- raceincome2023 %>%
  group_by(race_ethnicity, income_decile) %>%
  summarise(total_count = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop")




#US Summary
us_summary <- raceincome2023 %>%
  filter(
    !race_ethnicity %in% c("NA", "Unknown", "Other/Unknown")
  ) %>%
  group_by(region = "United States", race = race_ethnicity, decile = income_decile) %>%
  summarise(total_count = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop")

# CVILLE Summary
cville_summary <- raceincome2023 %>%
  filter(
    STATEFP == "51",
    COUNTYFP %in% c("540", "003"),
    !race_ethnicity %in% c("NA", "Unknown", "Other/Unknown")
  ) %>%
  group_by(region = "Charlottesville/Albemarle", race = race_ethnicity, decile = income_decile) %>%
  summarise(total_count = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop")

#combine the two
combined_summary <- raceincome2023 %>%
  filter(
    race_ethnicity %in% c("White", "Black", "Hispanic", "Asian", "AIAN"),
    income_decile %in% 1:10
  ) %>%
  mutate(
    region = if_else(STATEFP == "51" & COUNTYFP %in% c("540", "003"),
                     "Charlottesville/Albemarle", "United States"),
    race = race_ethnicity,
    decile = income_decile
  ) %>%
  group_by(region, race, decile) %>%
  summarise(total_count = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop")


#Graph shares

combined_prop <- combined_summary %>%
  group_by(region, race) %>%
  mutate(
    share = total_count / sum(total_count),
    decile = factor(decile, levels = 1:10)
  ) %>%
  ungroup()
combined_prop$race <- factor(combined_prop$race, levels = c("White", "Black", "Hispanic", "Asian", "AIAN"))
# Bar plot
tk <- ggplot(combined_prop, aes(x = decile, y = share, fill = region)) +
  geom_col(position = position_dodge(width = 1), width = 1) +
  facet_wrap(~ race, scales = "fixed") +
  scale_fill_manual(values = c("United States" = "orange", "Charlottesville/Albemarle" = "#232d4b")) +
  labs(
    title = "Income Decile Share by Race: U.S. vs. Charlottesville/Albemarle (2023)",
    x = "Income Decile",
    y = "Share of Race Group",
    fill = "Region"
  ) +
  theme_minimal()
ggsave("output/raceincome/Income Decile Share by Race: U.S. vs. CharlottesvilleAlbemarle orange blue.png", plot = tk, width = 7, height = 4)

# Layered bar plot
us_data <- combined_prop %>% filter(region == "United States") %>% mutate(alpha = 0.5)
cville_data <- combined_prop %>% filter(region == "Charlottesville/Albemarle") %>% mutate(alpha = 1)

tj <- ggplot() +
  geom_col(data = cville_data, aes(x = decile, y = share, fill = region, alpha = alpha), position = "identity") +
  geom_col(data = us_data, aes(x = decile, y = share, fill = region, alpha = alpha), position = "identity") +
  facet_wrap(~ race, scales = "free_y") +
  scale_fill_manual(values = c("United States" = "#1f78b4", "Charlottesville/Albemarle" = "#33a02c")) +
  scale_alpha_identity() +
  labs(
    title = "Income Decile Share by Race: U.S. vs. Charlottesville/Albemarle",
    x = "Income Decile", y = "Share of Race Group", fill = "Region"
  ) +
  theme_minimal()
ggsave("output/raceincome/Income Decile Share by Race: U.S. vs. CharlottesvilleAlbemarle.png", plot = tj, width = 6, height = 4)

# Avg decile by race
avg_decile_by_race <- combined_summary %>%
  group_by(region, race) %>%
  summarise(avg_decile = weighted.mean(decile, total_count, na.rm = TRUE), .groups = "drop")

ty <- ggplot(avg_decile_by_race, aes(x = race, y = avg_decile, fill = region)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("United States" = "#1f78b4", "Charlottesville/Albemarle" = "#FFA500")) +
  labs(title = "Average Income Decile by Race: U.S. vs. Charlottesville/Albemarle (2023)",
       x = "Race", y = "Average Income Decile", fill = "Region") +
  theme_minimal()
ggsave("output/raceincome/Average Income Decile by Race: U.S. vs. CharlottesvilleAlbemarle.png", plot = ty, width = 6, height = 4)

# Avg decile overall
avg_decile <- combined_summary %>%
  group_by(region) %>%
  summarise(avg_decile = weighted.mean(decile, total_count, na.rm = TRUE), .groups = "drop")

tw <- ggplot(avg_decile, aes(x = region, y = avg_decile, fill = region)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("United States" = "#1f78b4", "Charlottesville/Albemarle" = "#FFA500")) +
  labs(title = "Average Income Decile: U.S. vs. Charlottesville/Albemarle (2023)",
       x = "Region", y = "Average Income Decile", fill = "Region") +
  theme_minimal()
ggsave("output/raceincome/Average Income Decile: U.S. vs. CharlottesvilleAlbemarle.png", plot = tw, width = 6, height = 4)

# Decile-only graph
decile_summary <- raceincome2023 %>%
  filter(!is.na(income_decile), income_decile != 0, !is.na(n_noise_postprocessed)) %>%
  mutate(region = if_else(STATEFP == "51" & COUNTYFP %in% c("540", "003"),
                          "Charlottesville/Albemarle", "United States")) %>%
  group_by(region, decile = income_decile) %>%
  summarise(total_count = sum(n_noise_postprocessed, na.rm = TRUE), .groups = "drop")

decile_prop <- decile_summary %>%
  group_by(region) %>%
  mutate(share = total_count / sum(total_count)) %>%
  ungroup() %>%
  mutate(decile = factor(decile, levels = 1:10))

qq <- ggplot(decile_prop, aes(x = decile)) +
  geom_col(data = filter(decile_prop, region == "Charlottesville/Albemarle"),
           aes(y = share, fill = region), width = 0.9) +
  geom_hline(data = filter(decile_prop, region == "United States"),
             aes(yintercept = share, color = region), linewidth = 1) +
  scale_fill_manual(values = c("Charlottesville/Albemarle" = "#232d4b")) +
  scale_color_manual(values = c("United States" = "#FFA500")) +
  labs(title = "Income Decile Distribution: U.S. vs. Charlottesville/Albemarle",
       x = "Income Decile", y = "Share of Total Population") +
  theme_minimal()
ggsave("output/raceincome/Income Decile Distribution: U.S. vs. CharlottesvilleAlbemarle.png", plot = qq, width = 6, height = 4)

#AVG Income
avg_income_by_race <- raceincome_combined %>%
  mutate(race_ethnicity_clean = tolower(trimws(race_ethnicity))) %>%
  filter(
    !is.na(race_ethnicity_clean),
    !race_ethnicity_clean %in% c("na", "unknown", "other", "other/unknown"),
    !is.na(income_dollar_2024),
    !is.na(n_noise_postprocessed)
  ) %>%
  mutate(
    income_dollar_2024 = ifelse(income_dollar_2024 < 0, 0, income_dollar_2024)
  ) %>%
  group_by(year, race_ethnicity_clean) %>%
  summarise(
    avg_income = weighted.mean(income_dollar_2024, n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )
race_map <- c(
  "white" = "White",
  "black" = "Black",
  "hispanic" = "Hispanic",
  "asian" = "Asian",
  "aian" = "AIAN"
)

avg_income_by_race <- avg_income_by_race %>%
  mutate(race_ethnicity = race_map[race_ethnicity_clean]) %>%
  filter(!is.na(race_ethnicity)) %>%
  mutate(race_ethnicity = factor(race_ethnicity,
                                 levels = c("White", "Black", "Hispanic", "Asian", "AIAN")))
ggplot(avg_income_by_race, aes(x = year, y = avg_income, color = race_ethnicity)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "White" = "#a6cee3",
    "Black" = "#003f5c",
    "Hispanic" = "#601215",
    "Asian" = "#e7d8c9",
    "AIAN" = "#2a9d8f"
  )) +
  labs(
    title = "Average Adjusted Gross Income by Race (1999–2024)",
    x = "Year",
    y = "Average AGI (Inflation-Adjusted to 2024 $)",
    color = "Race/Ethnicity"
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 8),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    legend.position = "top"
  ) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, NA))


#Sparkline

# Clean race_ethnicity, remove unwanted categories, and handle negative incomes
avg_income_by_race <- raceincome_combined %>%
  mutate(race_ethnicity_clean = tolower(trimws(race_ethnicity))) %>%
  filter(
    !is.na(race_ethnicity_clean),
    !race_ethnicity_clean %in% c("na", "unknown", "other", "other/unknown"),
    !is.na(income_dollar_2024),
    !income_decile %in% c("na", "unknown", "other", "other/unknown", "0"),
    income_decile != 0
    
  ) %>%
  mutate(
    income_dollar_2024 = ifelse(income_dollar_2024 < 0, 0, income_dollar_2024)
  ) %>%
  group_by(year, race_ethnicity_clean) %>%
  summarise(
    avg_income = weighted.mean(income_dollar_2024, n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

# Map back cleaned names to proper case and factor
race_map <- c(
  "white" = "White",
  "black" = "Black",
  "hispanic" = "Hispanic",
  "asian" = "Asian",
  "aian" = "AIAN"
)

avg_income_by_race <- avg_income_by_race %>%
  mutate(race_ethnicity = race_map[race_ethnicity_clean]) %>%
  filter(!is.na(race_ethnicity)) %>%
  mutate(race_ethnicity = factor(race_ethnicity, levels = c("White", "Black", "Hispanic", "Asian", "AIAN")))

# Make trend table for race
race_income_trend <- avg_income_by_race %>%
  arrange(year) %>%
  group_by(race_ethnicity) %>%
  summarise(
    Trend = list(avg_income),
    Income_2024 = avg_income[year == 2024],
    .groups = "drop"
  ) %>%
  rename(`Race/Ethnicity` = race_ethnicity)

# For income decile, no string cleaning needed, but handle negative incomes similarly
avg_income_by_decile <- raceincome_combined %>%
  filter(!is.na(income_decile), !is.na(income_dollar_2024)) %>%
  mutate(
    income_dollar_2024 = ifelse(income_dollar_2024 < 0, 0, income_dollar_2024)
  ) %>%
  group_by(year, income_decile) %>%
  summarise(
    avg_income = weighted.mean(income_dollar_2024, n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

# Make trend table for decile
decile_income_trend <- avg_income_by_decile %>%
  arrange(year) %>%
  group_by(income_decile) %>%
  summarise(
    Trend = list(avg_income),
    Income_2024 = avg_income[year == 2024],
    .groups = "drop"
  ) %>%
  rename(`Income Decile` = income_decile)
decile_income_trend <- decile_income_trend %>%
  filter(`Income Decile` != 0)

reactable(
  race_income_trend,
  columns = list(
    `Race/Ethnicity` = colDef(name = "Race/Ethnicity"),
    Income_2024 = colDef(
      name = "2024 AGI",
      format = colFormat(prefix = "$", separators = TRUE, digits = 0),
      align = "center"
    ),
    Trend = colDef(
      name = "AGI Trend",
      cell = function(values) {
        left_label <- dollar(values[1])
        right_label <- dollar(values[length(values)])
        spark <- sparkline(values,
                           type = "line",
                           chartRangeMin = 0,
                           lineColor = "#003f5c",
                           fillColor = "rgba(0, 63, 92, 0.2)",
                           width = 100,
                           height = 40
        )
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          tags$span(left_label, style = "font-size: 10px; color: black;"),
          tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
          tags$span(right_label, style = "font-size: 10px; color: black;")
        )
      },
      html = TRUE,
      align = "center"
    )
  ),
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  compact = TRUE
)

jjp<- reactable(
  decile_income_trend,
  columns = list(
    `Income Decile` = colDef(
      name = "Income Decile",
      width = 120  # thinner column (pixels)
    ),
    Income_2024 = colDef(
      name = "2024 Average Income",
      format = colFormat(prefix = "$", separators = TRUE, digits = 0),
      width = 100
    ),
    Trend = colDef(
      name = "Income Trend",
      width = 250,   # wider column for sparkline
      cell = function(values) {
        left_label <- scales::dollar(values[1])
        right_label <- scales::dollar(values[length(values)])
        spark <- sparkline(values,
                           type = "line",
                           chartRangeMin = 0,
                           lineColor = "#003f5c",
                           fillColor = "rgba(0, 63, 92, 0.2)",
                           width = 140,  # widen the sparkline itself for better fit
                           height = 40
        )
        div(
          style = "display: flex;",
          tags$span(left_label, style = "font-size: 10px; color: black;"),
          tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
          tags$span(right_label, style = "font-size: 10px; color: black;")
        )
      },
      html = TRUE
      
    )
  ),
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  compact = TRUE
)
saveWidget(jjp, "income_trend_table.html", selfcontained = TRUE)

# Save as PNG
webshot("income_trend_table.html", "income_trend_table.png", zoom = 2)

#into THIRDS
income_trend_thirds <- raceincome_combined %>%
  filter(
    !is.na(income_decile),
    income_decile %in% 1:10,
    !is.na(income_dollar_2024),
    !is.na(n_noise_postprocessed)
  ) %>%
  mutate(
    income_dollar_2024 = ifelse(income_dollar_2024 < 0, 0, income_dollar_2024),
    Group = case_when(
      income_decile %in% 1:3 ~ "Bottom Third",
      income_decile %in% 4:7 ~ "Middle Third",
      income_decile %in% 8:10 ~ "Top Third"
    )
  ) %>%
  group_by(year, Group) %>%
  summarise(
    avg_income = weighted.mean(income_dollar_2024, n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )


# Prepare sparkline format
trend_table <- income_trend_thirds %>%
  arrange(year) %>%
  group_by(Group) %>%
  summarise(
    Trend = list(avg_income),
    Income_2024 = avg_income[year == 2024],
    Start_Income = first(avg_income),
    Years = 2024 - first(year),
    .groups = "drop"
  ) %>%
  mutate(
    Annual_Growth = (Income_2024 / Start_Income)^(1 / Years) - 1
  )
trend_table_clean <- trend_table %>%
  select(Group, Income_2024, Trend, Annual_Growth)


# Create the reactable
jjj <- reactable(
  trend_table_clean,
  columns = list(
    Group = colDef(name = "Income Tier", width = 120),
    Income_2024 = colDef(
      name = "2024 Average Income",
      format = colFormat(prefix = "$", separators = TRUE, digits = 0),
      width = 150
    ),
    Annual_Growth = colDef(
      name = "Avg Annual Growth",
      format = colFormat(percent = TRUE, digits = 1),
      width = 130
    ),
    Trend = colDef(
      name = "Income Trend",
      width = 250,
      cell = function(values) {
        left_label <- scales::dollar(values[1])
        right_label <- scales::dollar(values[length(values)])
        spark <- sparkline(values,
                           type = "line",
                           chartRangeMin = 0,
                           lineColor = "#003f5c",
                           fillColor = "rgba(0, 63, 92, 0.2)",
                           width = 140,
                           height = 40)
        div(
          style = "display: flex; align-items: center;",
          tags$span(left_label, style = "font-size: 10px; color: black;"),
          tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
          tags$span(right_label, style = "font-size: 10px; color: black;")
        )
      },
      html = TRUE
    )
  ),
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  compact = TRUE
)

saveWidget(jjj, "income_trend_thirds_table.html", selfcontained = TRUE)

# Save as PNG
webshot("income_trend_thirds_table.html", "output/raceincome/income_trend_thirds_table.png", zoom = 2)


#GGPLOT
ggplot(income_trend_thirds, aes(x = year, y = avg_income, color = Group)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Bottom Third" = "#a6cee3",
    "Middle Third" = "#1f78b4",
    "Top Third" = "#003f5c"
  )) +
  scale_y_continuous(labels = dollar_format(), limits = c(0, NA)) +
  labs(
    title = "Average Income Over Time by Income Tier",
    x = "Year",
    y = "Average Income (2024-adjusted $)",
    color = "Income Group"
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Filter for Charlottesville/Albemarle 1999–2024 data
cville_deciles <- raceincome_combined %>%
  filter(
    !is.na(income_decile),
    income_decile %in% 1:10,
    !is.na(income_dollar_2024),
    !is.na(n_noise_postprocessed),
    STATEFP == "51", COUNTYFP %in% c("540", "003")
  ) %>%
  mutate(
    income_dollar_2024 = ifelse(income_dollar_2024 < 0, 0, income_dollar_2024)
  )

# Summarize: avg income per decile per year
cville_decile_yearly <- cville_deciles %>%
  group_by(year, income_decile) %>%
  summarise(
    avg_income = weighted.mean(income_dollar_2024, n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

# Sparkline prep: Trend + income in 2024
cville_trend_table <- cville_decile_yearly %>%
  arrange(year) %>%
  group_by(income_decile) %>%
  summarise(
    Trend = list(avg_income),
    Income_2024 = avg_income[year == 2024],
    Start_Income = first(avg_income),
    Years = 2024 - first(year),
    .groups = "drop"
  ) %>%
  mutate(
    Annual_Growth = (Income_2024 / Start_Income)^(1 / Years) - 1
  )

# Add population count and share from 2024
decile_pop_2024 <- cville_deciles %>%
  filter(year == 2024) %>%
  group_by(income_decile) %>%
  summarise(
    Population = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share = Population / sum(Population)
  )

# Merge together

cville_decile_table_simple <- cville_trend_table %>%
  select(income_decile, Income_2024, Annual_Growth) %>%
  left_join(decile_pop_2024, by = "income_decile") %>%
  rename(
    `Income Decile` = income_decile,
    `Avg Income (2024 $)` = Income_2024,
    `Annual Growth` = Annual_Growth,
    `Population (2024)` = Population,
    `Share of Pop.` = Share
  )

# Render table with reactable
tup <- reactable(
  cville_decile_table_simple,
  columns = list(
    `Income Decile` = colDef(name = "Decile", width = 70),
    `Population (2024)` = colDef(
      format = colFormat(separators = TRUE, digits = 0),
      width = 120
    ),
    `Share of Pop.` = colDef(
      format = colFormat(percent = TRUE, digits = 1),
      width = 100
    ),
    `Avg Income (2024 $)` = colDef(
      format = colFormat(prefix = "$", separators = TRUE, digits = 0),
      width = 160
    ),
    `Annual Growth` = colDef(
      format = colFormat(percent = TRUE, digits = 1),
      width = 120
    )
  ),
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  compact = TRUE
)

saveWidget(tup, "income_table.html", selfcontained = TRUE)

# Save as PNG
webshot(
  "income_table.html",
  "output/raceincome/income_table.png",
  vwidth = 650,    # viewport width in pixels
  vheight = 800,    # viewport height in pixels
  zoom = 3
)

