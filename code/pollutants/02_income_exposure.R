################################################################################
# FILE: 02_income_exposure.R
# PURPOSE: Create table of exposure by income category.
# AUTHOR: Arnav Dharmagadda
# CREATED: July 7th, 2025
################################################################################
# INPUTS: pollutants_long.rda, ageracesex_long.rda, raceincome_long.rda
# OUTPUTS: None.
################################################################################

#### COLLAPSE DATA ####

raceincome_pollute_avg <- raceincome_pollute %>%
    mutate(pm25_gt_9 = pm25_ACAG_gwr > 9) %>%
    group_by(year, income_decile) %>%
    summarize(
      pm25_avg = weighted.mean(pm25_ACAG_gwr, n_noise_postprocessed, na.rm = TRUE),
      exceed_standard = weighted.mean(pm25_gt_9, n_noise_postprocessed, na.rm = TRUE),
      .groups = "drop"
    )

national_avg <- national %>%
    mutate(pm25_gt_9 = pm25_ACAG_gwr > 9) %>%
    group_by(year, income_decile) %>%
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

# Prepare sparkline-friendly format with nat avg
pm25_spark_data <- raceincome_pollute_avg %>%
  filter(!is.na(pm25_avg)) %>%
  filter(year != 1999) %>%
  filter(income_decile != 0) %>%
  arrange(income_decile, year) %>%
  group_by(income_decile) %>%
  summarise(
    PM25_2023 = pm25_avg[year == 2023],
    Trend = list(pm25_avg),
    NatTrend = nat_pm25_avg[year == 2023],
    Exceed_2023 = exceed_standard[year == 2023],
    ExceedTrend = list(exceed_standard),
    NatExceedTrend = nat_exceed_standard[year == 2023],
    .groups = "drop"
  ) %>%
  mutate(income_decile = as.character(income_decile))  # So it prints cleanly

htmltools::save_html(
  reactable(
    pm25_spark_data,
    columns = list(
      income_decile = colDef(name = "Income Decile", align = "center"),
      PM25_2023 = colDef(
        name = "PM2.5<br><span style='font-size: 16px; font-weight: normal;'>(2023)</span>",
        format = colFormat(digits = 2),
        align = "center",
        html = TRUE
      ),
      Exceed_2023 = colDef(
        name = "% Exceeding Standard<br><span style='font-size: 16px; font-weight: normal;'>(2023)</span>",
        format = colFormat(percent = TRUE, digits = 1),
        align = "center",
        html = TRUE
      ),
      Trend = colDef(
        name = "PM2.5<br><span style='font-size: 16px; font-weight: normal;'>(2000–2023)</span>",
        cell = function(values) {
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
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            tags$span(left_label, style = "font-size: 10px; color: black;"),
            tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
            tags$span(right_label, style = "font-size: 10px; color: black;")
          )
        },
        html = TRUE,
        align = "center"
      ),
      ExceedTrend = colDef(
        name = "% Exceeding Standard<br><span style='font-size: 16px; font-weight: normal;'>(2000–2023)</span>",
        cell = function(values) {
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
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            tags$span(left_label, style = "font-size: 10px; color: black;"),
            tags$div(spark, style = "padding-left: 5px; padding-right: 5px;"),
            tags$span(right_label, style = "font-size: 10px; color: black;")
          )
        },
        html = TRUE,
        align = "center"
      ),
      NatTrend = colDef(
        name = "National PM2.5<br><span style='font-size: 16px; font-weight: normal;'>(2023)</span>",
        format = colFormat(digits = 2),
        align = "center",
        html = TRUE
      ),
      NatExceedTrend = colDef(
        name = "National % Exceeding Standard<br><span style='font-size: 16px; font-weight: normal;'>(2023)</span>",
        format = colFormat(percent = TRUE, digits = 1),
        align = "center",
        html = TRUE
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
  file = "GitHub/environmental-inequality-atlas/output/pollutants/income_exposure.html"
)

webshot2::webshot(
  "GitHub/environmental-inequality-atlas/output/pollutants/income_exposure.html",
  "GitHub/environmental-inequality-atlas/code/quarto/images/income_exposure.png",
  vwidth = 1200,
  vheight = 800,
  zoom = 3,
  delay = 2
)
#### CREATING LINE PLOT ####

line_plot <- raceincome_pollute %>%
    mutate(pm25_gt_9 = pm25_ACAG_gwr > 9) %>%
    filter(year == 2023) %>%
    filter(income_decile != 0) %>%
    group_by(income_decile) %>%
    summarize(
      pm25_avg = weighted.mean(pm25_ACAG_gwr, n_noise_postprocessed, na.rm = TRUE),
      exceed_standard = weighted.mean(pm25_gt_9, n_noise_postprocessed, na.rm = TRUE),
      .groups = "drop"
    )

ggplot(line_plot, aes(x = income_decile, y = pm25_avg)) +
  geom_point(color = "steelblue", size = 2, shape = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linetype = "dashed") +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "PM2.5 Average by Income Decile (2023)",
    x = "Income Decile",
    y = "Average PM2.5 (µg/m³)"
      ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black")
  )

#### CREATING LINE PLOT BY RACE ####

line_plot_race <- raceincome_pollute %>%
    mutate(pm25_gt_9 = pm25_ACAG_gwr > 9) %>%
    filter(year == 2023) %>%
    filter(income_decile != 0) %>%
    group_by(race_ethnicity, income_decile) %>%
    summarize(
      pm25_avg = weighted.mean(pm25_ACAG_gwr, n_noise_postprocessed, na.rm = TRUE),
      exceed_standard = weighted.mean(pm25_gt_9, n_noise_postprocessed, na.rm = TRUE),
      .groups = "drop"
    )

ggplot(line_plot_race, aes(x = income_decile, y = pm25_avg, color = race_ethnicity)) +
  #geom_point(size = 2, shape = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  labs(
    title = "PM2.5 Average by Income Decile and Race (2023)",
    x = "Income Decile",
    y = "Average PM2.5 (µg/m³)",
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
