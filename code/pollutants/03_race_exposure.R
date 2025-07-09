################################################################################
# FILE: 03_race_exposure.R
# PURPOSE: Create table of exposure by race category.
# AUTHOR: Arnav Dharmagadda
# CREATED: July 7th, 2025
################################################################################
# INPUTS: pollutants_long.rda, ageracesex_long.rda, raceincome_long.rda
# OUTPUTS: None.
################################################################################

#### COLLAPSE DATA ####

raceincome_pollute_avg <- raceincome_pollute %>%
    mutate(pm25_gt_9 = pm25_ACAG_gwr > 9) %>%
    group_by(year, race_ethnicity) %>%
    summarize(
      pm25_avg = weighted.mean(pm25_ACAG_gwr, n_noise_postprocessed, na.rm = TRUE),
      exceed_standard = weighted.mean(pm25_gt_9, n_noise_postprocessed, na.rm = TRUE),
      .groups = "drop"
    )

national_avg <- national %>%
    mutate(pm25_gt_9 = pm25_ACAG_gwr > 9) %>%
    group_by(year, race_ethnicity) %>%
    summarize(
      nat_pm25_avg = weighted.mean(pm25_ACAG_gwr, n_noise_postprocessed, na.rm = TRUE),
      nat_exceed_standard = weighted.mean(pm25_gt_9, n_noise_postprocessed, na.rm = TRUE),
      .groups = "drop"
    )


# Merge national averages to raceincome_pollute_avg
raceincome_pollute_avg <- raceincome_pollute_avg %>%
  left_join(
    national_avg
  )

# Calculate total (all races combined) statistics
total_stats <- raceincome_pollute %>%
  mutate(pm25_gt_9 = pm25_ACAG_gwr > 9) %>%
  filter(year != 1999) %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  group_by(year) %>%
  summarize(
    pm25_avg = weighted.mean(pm25_ACAG_gwr, n_noise_postprocessed, na.rm = TRUE),
    exceed_standard = weighted.mean(pm25_gt_9, n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate national totals (all races combined)
national_total_stats <- national %>%
  mutate(pm25_gt_9 = pm25_ACAG_gwr > 9) %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  select(pm25_ACAG_gwr, n_noise_postprocessed, pm25_gt_9) %>%
  summarize(
    nat_pm25_avg = weighted.mean(pm25_ACAG_gwr, n_noise_postprocessed, na.rm = TRUE),
    nat_exceed_standard = weighted.mean(pm25_gt_9, n_noise_postprocessed, na.rm = TRUE),
    year = 2023,
    .groups = "drop"
  )

# Combine total stats with national totals
total_stats <- total_stats %>%
  left_join(national_total_stats, by = "year") %>%
  summarise(
    race_ethnicity = "Total",
    PM25_2023 = pm25_avg[year == 2023],
    Trend = list(pm25_avg),
    NatTrend = nat_pm25_avg[year == 2023],
    Exceed_2023 = exceed_standard[year == 2023],
    ExceedTrend = list(exceed_standard),
    NatExceedTrend = nat_exceed_standard[year == 2023],
    .groups = "drop"
  )

# Prepare sparkline-friendly format with nat avg
pm25_spark_data <- raceincome_pollute_avg %>%
  filter(!is.na(pm25_avg)) %>%
  filter(year != 1999) %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  arrange(race_ethnicity, year) %>%
  group_by(race_ethnicity) %>%
  summarise(
    PM25_2023 = pm25_avg[year == 2023],
    Trend = list(pm25_avg),
    NatTrend = nat_pm25_avg[year == 2023],
    Exceed_2023 = exceed_standard[year == 2023],
    ExceedTrend = list(exceed_standard),
    NatExceedTrend = nat_exceed_standard[year == 2023],
    .groups = "drop"
  ) %>%
  # Add the total row
  bind_rows(total_stats) %>%
  # Reorder rows to match desired race order with Total at top
  mutate(race_ethnicity = factor(race_ethnicity, levels = c("White", "Black", "Hispanic", "Asian", "AIAN", "Total"))) %>%
  arrange(race_ethnicity) 

htmltools::save_html(
  reactable(
    pm25_spark_data,
    columns = list(
      race_ethnicity = colDef(
        name = "Race or Ethnicity", 
        align = "center",
        style = function(value) {
          if (value == "Total") {
            list(fontWeight = "bold", backgroundColor = "#f5f5f5")
          }
        }
      ),
      PM25_2023 = colDef(
        name = "PM2.5<br><span style='font-size: 16px; font-weight: normal;'>(2023)</span>",
        format = colFormat(digits = 2),
        align = "center",
        html = TRUE,
        style = function(value, index) {
          if (pm25_spark_data$race_ethnicity[index] == "Total") {
            list(fontWeight = "bold", backgroundColor = "#f5f5f5")
          }
        }
      ),
      Trend = colDef(
        name = "PM2.5 <br><span style='font-size: 16px; font-weight: normal;'>(2000–2023)</span>",
        cell = function(values, index) {
          n <- length(values)
          left_label <- formatC(values[1], format = "f", digits = 1)
          right_label <- formatC(values[n], format = "f", digits = 1)
          spark <- sparkline(
            values,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = max(values, na.rm = TRUE),
            lineColor = "black",
            lineWidth = 1,
            fillColor = "rgba(0, 0, 0, 0.15)",
            spotRadius = 1.5,
            highlightSpotColor = NULL,
            highlightLineColor = NULL,
            minSpotColor = FALSE,
            maxSpotColor = FALSE,
            lastSpotColor = FALSE,
            width = 100,
            height = 40
          )
          div_style <- "display: flex; align-items: center; justify-content: center;"
          div(
            style = div_style,
            tags$span(left_label, style = "font-size: 10px; color: black;"),
            tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
            tags$span(right_label, style = "font-size: 10px; color: black;")
          )
        },
        html = TRUE,
        align = "center",
        style = function(value, index) {
          if (pm25_spark_data$race_ethnicity[index] == "Total") {
            list(fontWeight = "bold", backgroundColor = "#f5f5f5")
          }
        }
      ),
      NatTrend = colDef(
        name = "National PM2.5<br><span style='font-size: 16px; font-weight: normal;'>(2023)</span>",
        format = colFormat(digits = 2),
        align = "center",
        html = TRUE,
        style = function(value, index) {
          if (pm25_spark_data$race_ethnicity[index] == "Total") {
            list(fontWeight = "bold", backgroundColor = "#f5f5f5")
          }
        }
      ),
      Exceed_2023 = colDef(
        name = "% Exceeding Standard<br><span style='font-size: 16px; font-weight: normal;'>(2023)</span>",
        format = colFormat(percent = TRUE, digits = 1),
        align = "center",
        html = TRUE,
        style = function(value, index) {
          if (pm25_spark_data$race_ethnicity[index] == "Total") {
            list(fontWeight = "bold", backgroundColor = "#f5f5f5")
          }
        }
      ),
      ExceedTrend = colDef(
        name = "% Exceeding Standard<br><span style='font-size: 16px; font-weight: normal;'>(2000–2023)</span>",
        cell = function(values, index) {
          n <- length(values)
          left_label <- paste0(round(100 * values[1], 1), "%")
          right_label <- paste0(round(100 * values[n], 1), "%")
          spark <- sparkline(
            values,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = 1,
            lineColor = "black",
            lineWidth = 1,
            fillColor = "rgba(0, 0, 0, 0.15)",
            spotRadius = 1.5,
            highlightSpotColor = NULL,
            highlightLineColor = NULL,
            minSpotColor = FALSE,
            maxSpotColor = FALSE,
            lastSpotColor = FALSE,
            width = 100,
            height = 40
          )
          div_style <- "display: flex; align-items: center; justify-content: center;"
          div(
            style = div_style,
            tags$span(left_label, style = "font-size: 10px; color: black;"),
            tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
            tags$span(right_label, style = "font-size: 10px; color: black;")
          )
        },
        html = TRUE,
        align = "center",
        style = function(value, index) {
          if (pm25_spark_data$race_ethnicity[index] == "Total") {
            list(fontWeight = "bold", backgroundColor = "#f5f5f5")
          }
        }
      ),
      NatExceedTrend = colDef(
        name = "National % Exceeding Standard<br><span style='font-size: 16px; font-weight: normal;'>(2023)</span>",
        format = colFormat(percent = TRUE, digits = 1),
        align = "center",
        html = TRUE,
        style = function(value, index) {
          if (pm25_spark_data$race_ethnicity[index] == "Total") {
            list(fontWeight = "bold", backgroundColor = "#f5f5f5")
          }
        }
      )
    ),
    theme = reactableTheme(
      headerStyle = list(
        fontSize = "25px",
        fontWeight = "bold",
        color = "black",
        fontFamily = "Lato"
      ),
      cellStyle = list(
        fontFamily = "Lato"
      )
    ),
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE
  ),
  file = "GitHub/environmental-inequality-atlas/output/pollutants/race_exposure.html"
)

webshot2::webshot(
  "GitHub/environmental-inequality-atlas/output/pollutants/race_exposure.html",
  "GitHub/environmental-inequality-atlas/code/quarto/images/race_exposure.png",
  vwidth = 1200,
  vheight = 800,
  zoom = 3,
  delay = 2
)

#### Changes in Exposure Over Time ####

growth_rates <- raceincome_pollute %>%
  filter(race_ethnicity != "Other/Unknown") %>%
  filter(year >= 2000) %>%
  filter(income_decile >= 1) %>%
  group_by(race_ethnicity, income_decile) %>%
  summarize(
    first_year = min(year),
    last_year = max(year),
    first_value = pm25_ACAG_gwr[year == first_year][1],
    last_value = pm25_ACAG_gwr[year == last_year][1],
    n_years = last_year - first_year,
    avg_growth_rate = (last_value / first_value)^(1 / n_years) - 1
  )

ggplot(growth_rates, aes(x = income_decile, y = avg_growth_rate, color = race_ethnicity)) +
  #geom_point(size = 2, shape = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  labs(
    title = "Average Decline in PM2.5 by Income Decile and Race (2023)",
    x = "Income Decile",
    y = "Average PM2.5 Decline (% Decrease)",
    color = "Race/Ethnicity"
      ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    legend.position = "bottom",
    axis.ticks.y = element_line(color = "black")
  ) 
