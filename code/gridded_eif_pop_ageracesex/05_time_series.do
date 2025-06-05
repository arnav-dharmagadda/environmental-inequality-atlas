/*******************************************************************************
# FILE: 04_time_series.do
# PURPOSE: This script generates time series figures based on the Gridded EIF 
  age, race, sex files.
# AUTHOR: Arnav Dharmagadda
# CREATED: June 5th, 2025
################################################################################
# INPUTS: 
# OUTPUTS: 
*******************************************************************************/

*** SET DIRECTORY ***

cd "${time_series}"

*** LOAD DATA ***

use "${reshaped}ageracesex.dta", clear

*** 1: RACIAL COMPOSITION OVER TIME ***

preserve

collapse (sum) pp_pop_AIAN pp_pop_Asian pp_pop_Black pp_pop_Hispanic pp_pop_NA_race pp_pop_White pp_pop_other_race total, by(year)

// Raw Counts

twoway ///
  (line pp_pop_White year, lcolor(blue) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_Black year, lcolor(red) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_Hispanic year, lcolor(green) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_Asian year, lcolor(orange) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_other_race year, lcolor(navy) lwidth(medthick) lpattern(solid)), ///
  title("Population Trends by Race/Ethnicity", size(medium)) ///
  subtitle("1999–2023", size(small)) ///
  ytitle("Population Count", size(small)) ///
  xtitle("Year", size(small)) ///
  legend(order(1 "White" 2 "Black" 3 "Hispanic" 4 "Asian" 5 "Other") ring(10) pos(6) col(3)) ///
  graphregion(color(white)) ///
  scheme(538w)
  
// Over Time

local race pp_pop_AIAN pp_pop_Asian pp_pop_Black pp_pop_Hispanic pp_pop_NA_race pp_pop_White pp_pop_other_race

foreach r of local race {
	gen `r'share = `r' / total
}

twoway ///
  (line pp_pop_Whiteshare year, lcolor(blue) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_Blackshare year, lcolor(red) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_Hispanicshare year, lcolor(green) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_Asianshare year, lcolor(orange) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_other_raceshare year, lcolor(navy) lwidth(medthick) lpattern(solid)), ///
  title("Population Trends by Race/Ethnicity", size(medium)) ///
  subtitle("1999–2023", size(small)) ///
  ytitle("Population Share", size(small)) ///
  xtitle("Year", size(small)) ///
  legend(order(1 "White" 2 "Black" 3 "Hispanic" 4 "Asian" 5 "Other") ring(10) pos(6) col(3)) ///
  graphregion(color(white)) ///
  scheme(538w)
  
restore
  
*** 2: AGE COMPOSITION OVER TIME ***

collapse (sum) pp_pop_bet_19_65 pp_pop_missing_age pp_pop_over_65 pp_pop_under_18 total, by(year)

// Raw Counts

twoway ///
  (line pp_pop_under_18 year, lcolor(blue) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_bet_19_65 year, lcolor(red) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_over_65 year, lcolor(green) lwidth(medthick) lpattern(solid)), ///
  title("Population Trends by Age Group", size(medium)) ///
  subtitle("1999–2023", size(small)) ///
  ytitle("Population Count", size(small)) ///
  xtitle("Year", size(small)) ///
  legend(order(1 "Under 18" 2 "Between 19-65" 3 "Over 65") ring(10) pos(6) col(3)) ///
  graphregion(color(white)) ///
  scheme(538w)
  
// Over Time

local age pp_pop_bet_19_65 pp_pop_missing_age pp_pop_over_65 pp_pop_under_18

foreach a of local age {
	gen `a'share = `a' / total
}

twoway ///
  (line pp_pop_under_18share year, lcolor(blue) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_bet_19_65share year, lcolor(red) lwidth(medthick) lpattern(solid)) ///
  (line pp_pop_over_65share year, lcolor(green) lwidth(medthick) lpattern(solid)), ///
  title("Population Trends by Age Group", size(medium)) ///
  subtitle("1999–2023", size(small)) ///
  ytitle("Population Share", size(small)) ///
  xtitle("Year", size(small)) ///
  legend(order(1 "Under 18" 2 "Between 19-65" 3 "Over 65") ring(10) pos(6) col(3)) ///
  graphregion(color(white)) ///
  scheme(538w)
  