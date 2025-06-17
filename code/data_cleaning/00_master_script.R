################################################################################
# FILE: 00_master_script.R
# PURPOSE: Run all cleaning files for Gridded EIF population data.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 9th, 2025
################################################################################
# INPUTS: None.
# OUTPUTS: None.
################################################################################

#### Source Individual Scripts ####

source(paste0(git_path, "code/data_cleaning/01_setup.R"))

source(paste0(git_path, "code/data_cleaning/02_clean_ageracesex.R"))

source(paste0(git_path, "code/data_cleaning/03_clean_raceincome.R"))

source(paste0(git_path, "code/data_cleaning/04_merge_ageracesex_raceincome.R"))

source(paste0(git_path, "code/data_cleaning/05_point_adjustment.R"))

source(paste0(git_path, "code/data_cleaning/06_hexagon_adjustment.R"))

source(paste0(git_path, "code/data_cleaning/07_grid_adjustment.R"))