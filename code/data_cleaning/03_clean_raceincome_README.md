# Basic Information

Name: 03_clean_raceincome.R
Author: Arnav Dharmagadda
Created: 6/9/2025

# Purpose

This script will read in parquet files containing gridded EIF data on race-income from 1999 through 2023. The files will be merged with the gridpoints data, providing each year with information on county and state geographies. The files will be outputted to an rda and dta file for analysis in R and Stata. 

Then, the files will be appended, creating a year variable. This creates a long data set that is combined and saved.

This data will be reshaped. We create dataframes with variables on race-income combinations, race separately, and income separately. We join these and save this reshaped dataset where each observation is a grid-year observation.

Finally, we reshape such that each observation is a grid only. This is also saved.

# Instructions

1. Run
