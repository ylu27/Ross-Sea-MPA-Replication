****Table 2 DiD regressions
sum
duplicates drop
drop if year==2020
drop if year==2019 & month == 12 
gen postmpa = 1 if date>21153
replace postmpa = 0 if missing(postmpa)
bysort mmsi date: egen dfish = sum(fishing_hours)
bysort mmsi date: egen dcruise = sum(cruise_hours)
gen dop = dfish+dcruise
*keep variable for estimation
keep mmsi  year month modate date treatment postmpa dfish dcruise dop
duplicates drop
drop if dfish==0 & dcruise==0
format %tm modate
sum
*set vessel-day panel
xtset mmsi date
*estimation - clustered standard errors by mmsi
regress dfish treatment##postmpa i.mmsi  i.month, cluster(mmsi)
regress dcruise treatment##postmpa i.mmsi  i.month, cluster(mmsi)
regress dop treatment##postmpa i.mmsi  i.month, cluster(mmsi)

****Table 4 Summary Statistics
sum if treatment==1 & postmpa==0
sum if treatment==1 & postmpa==1
sum if treatment==0 & postmpa==0
sum if treatment==1 & postmpa==1