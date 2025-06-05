/*******************************************************************************
# FILE: 04_setup_stata.do
# PURPOSE: This script generates the data files for Stata analysis of Gridded 
  EIF data.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 5th, 2025
################################################################################
# INPUTS: 
# OUTPUTS: 
*******************************************************************************/

*** SET DIRECTORY ***

global n=1

// Arnav's Laptop

if $n==1 {
	
	global input "/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/gridded_eif_pop_ageracesex_dta/"
    global output "/Users/arnavdharmagadda/The Lab Dropbox/Arnav Dharmagadda/GitHub/environmental-inequality-atlas/data/processed/gridded_eif_pop_ageracesex_reshaped/"
	
}

cd "${output}"

*** LOOP AND SAVE FILES ***

forvalues year = 1999/2023 {

use "${input}ageracesex_`year'.dta", clear

// Race

preserve

collapse (sum) raw_pop_ = n_noise pp_pop_ = n_noise_postprocessed, by(grid_lon grid_lat STATEFP COUNTYFP COUNTYNS AFFGEOID GEOID NAME LSAD ALAND AWATER race_ethnicity)

replace race_ethnicity = "NA" if race_ethnicity == ""
replace race_ethnicity = "Other" if race_ethnicity == "Other/Unknown"

reshape wide raw_pop_ pp_pop_, i(grid_lon grid_lat) j(race_ethnicity) string

tempfile race
save `race'

restore

// Sex

preserve 

collapse (sum) raw_pop_ = n_noise pp_pop_ = n_noise_postprocessed, by(grid_lon grid_lat STATEFP COUNTYFP COUNTYNS AFFGEOID GEOID NAME LSAD ALAND AWATER sex)

replace sex = "NA" if sex == ""
replace sex = "Missing" if sex == "Missing Gender"

reshape wide raw_pop_ pp_pop_, i(grid_lon grid_lat) j(sex) string

tempfile sex
save `sex'

restore

// Age

preserve

collapse (sum) raw_pop_ = n_noise pp_pop_ = n_noise_postprocessed, by(grid_lon grid_lat STATEFP COUNTYFP COUNTYNS AFFGEOID GEOID NAME LSAD ALAND AWATER age_group)

replace age_group = "NA" if age_group == ""
replace age_group = "missing" if age_group == "Missing Age"
replace age_group = "over_65" if age_group == "Over 65"
replace age_group = "under_18" if age_group == "Under 18"
replace age_group = "bet_19_65" if age_group == "19-65"

reshape wide raw_pop_ pp_pop_, i(grid_lon grid_lat) j(age_group) string

tempfile age
save `age'

restore

use `race', clear

merge 1:1 grid_lon grid_lat using `sex'

drop _merge

merge 1:1 grid_lon grid_lat using `age'

drop _merge

gen year = `year'

tempfile `year'
save ``year''

}

forvalues year = 1999(1)2022 {
    append using ``year''
}

save "ageracesex.dta", replace

