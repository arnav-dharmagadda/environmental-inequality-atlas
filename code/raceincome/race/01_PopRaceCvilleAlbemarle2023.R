################################################################################
# FILE: PopRaceCvilleAlbemarle2023.R
# PURPOSE: Use gridded EIF data to create a bar chart on population share by
# race/ethnicity in Charlottesville and Albemarle County in 2023.
# AUTHOR: Elizabeth Shiker
# CREATED: June 9th, 2025
################################################################################
# INPUTS: raceincome_rda/raceincome_2023.rda
# OUTPUTS: pop_share_by_race_2023.png
################################################################################

#Load in race and income 2023 data

raceincome_2023 <- load("data/processed/raceincome_rda/raceincome_2023.rda")
raceincome2023 <- get(raceincome_2023)

# sum population in each grid by race

racesumbygrid <- raceincome2023 %>%
  filter(!is.na(race_ethnicity)) %>%
  group_by(grid_lon, grid_lat, race_ethnicity) %>%
  summarise(
    n_noise = sum(n_noise, na.rm = TRUE),
    n_noise_postprocessed = sum(n_noise_postprocessed, na.rm = TRUE),
    .groups = "drop"
  )

raceincome_2023_wide <- racesumbygrid %>%
  pivot_wider(
    names_from = race_ethnicity,
    values_from = c(n_noise, n_noise_postprocessed),
    names_sep = "_"
  )

# Sum total post processed population by race/ethnicity and calculate shares

race_totals <- raceincome_2023_wide %>%
  summarise(across(starts_with("n_noise_postprocessed_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    everything(),
    names_to = "race",
    values_to = "total_population"
  ) %>%
  mutate(race = gsub("n_noise_postprocessed_", "", race)) %>%
  filter(race != "Other/Unknown") %>% #exclude before computing share
  mutate(share = total_population / sum(total_population))

# Plot as a histogram (bar chart)
#ggplot(race_totals, aes(x = reorder(race, -total_population), y = total_population)) +
  #geom_bar(stat = "identity", fill = "steelblue") +
  #labs(title = "Total Population by Race/Ethnicity in Charlottesville and Albemarle County (2023)",
       #x = "Race/Ethnicity",
       #y = "Total Postprocessed Population") +
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Saving bar chart of race shares in Cville and AC in 2023
library(scales)  # for percent_format()

ggsave(
  "output/raceincome/race/pop_share_by_race_2023.png",
  plot = ggplot(race_totals, aes(x = reorder(race, -share), y = share)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Population Share by Race/Ethnicity in Charlottesville and Albemarle County (2023)",
      x = "Race/Ethnicity",
      y = "Share of Total Population"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  width = 8,
  height = 6
)