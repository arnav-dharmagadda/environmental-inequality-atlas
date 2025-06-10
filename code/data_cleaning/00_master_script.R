################################################################################
# FILE: 00_master_script.R
# PURPOSE: Run all cleaning files for Gridded EIF population data.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 9th, 2025
################################################################################
# INPUTS: None.
# OUTPUTS: None.
################################################################################

#### Clear Environment (optional) ####
rm(list = ls())

#### Set Working Directory (optional) ####

setwd("/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas")

data_path <- "/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/gridded_eif_data/"

#### Source Individual Scripts ####

source("code/data_cleaning/01_setup.R")

source("code/data_cleaning/02_clean_ageracesex.R")

source("code/data_cleaning/03_clean_raceincome.R")

source("code/data_cleaning/04_merge_ageracesex_raceincome.R")

source("code/data_cleaning/05_polygonal_adjustment.R")