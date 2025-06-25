###################################################################
# Purpose: Charlottesville Locator map
# Author: Josie Fischman
# Date created: 6/12/2025
####################################################################
# Inputs:county data
# Outputs: 
###################################################################

gridpoints_path <- paste0(data_path, "/raceincome/gridpoints_with_county_2020 (1).rda")

#### Load Data ####

load(gridpoints_path)
gridpoints <- df
rm(df)

library(tigris)
library(sf)
library(ggplot2)

# Get Virginia counties as polygons
va_counties <- counties(state = "51", cb = TRUE, class = "sf")

# Filter for Albemarle (540) and Charlottesville (003)
highlighted <- va_counties %>% 
  filter(COUNTYFP %in% c("003", "540"))

# Plot
ggplot() +
  geom_sf(data = va_counties, fill = "grey90", color = "black") +
  geom_sf(data = highlighted, fill = "#232d4b", color = "black", size = 0.6) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),        # Remove coordinate text
    axis.ticks = element_blank(),       # Remove ticks
    panel.grid = element_blank(),       # Remove gridlines
    #panel.background = element_blank()  # Optional: remove panel background
  )
  