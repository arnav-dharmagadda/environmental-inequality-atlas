################################################################################
# FILE: 08_IncomeDistribution_CvilleAC1999to2023.R
# PURPOSE: Use gridded EIF data for race/income from 1999 to 2023 to generate 
# an area plot on share for each income decile
# AUTHOR: Elizabeth Shiker
# CREATED: June 23rd, 2025
################################################################################
# INPUTS: raceincome_year.rda
# OUTPUTS: IncomeDistribution_CvilleAC_AreaChart.png
################################################################################

# Define range of years
years <- 1999:2023
data_path <- "data/processed/raceincome_rda"

# Loop through years and summarize by income decile
income_share_list <- lapply(years, function(yr) {
  file <- file.path(data_path, paste0("raceincome_", yr, ".rda"))
  obj_name <- load(file)
  data <- get(obj_name)
  
  # Filter for Cville + Albemarle County
  data %>%
    filter(
      STATEFP == "51",
      COUNTYFP %in% c("003", "540"),
      !is.na(income_decile),
      !is.na(n_noise_postprocessed)
    ) %>%
    # Group by income decile and sum population counts
    group_by(income_decile) %>%
    summarise(pop = sum(n_noise_postprocessed), .groups = "drop") %>%
    # Add year column and calculate shares
    mutate(
      year = yr,
      share = pop / sum(pop)
    )
})

# Combine into one data frame
income_share_cville <- bind_rows(income_share_list) %>%
  mutate(income_decile = factor(income_decile, levels = 1:10))

# Create and save area plot showing the share of each income decile over time
ggsave(
  "output/raceincome/IncomeDistribution_CvilleAC_AreaChart.png",
  plot = ggplot(income_share_cville, aes(x = year, y = share, fill = income_decile)) +
  geom_area(color = "white", linewidth = 0.2) + # white boundary between layers
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_viridis_d(name = "Income Decile", direction = -1) +
  labs(
    title = "Income Distribution in Charlottesville/Albemarle (1999–2023)",
    x = "Year",
    y = "Share of Population"
  ) +
  theme_minimal(),
  width = 8,
  height = 6
)



