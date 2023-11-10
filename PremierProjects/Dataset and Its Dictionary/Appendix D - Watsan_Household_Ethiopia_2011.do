***** PRIMARY AND SECONDARY EDUCATION PARTICIPATION BY REGION *****
***** written by Todd G. Smith based on EPDC do files *****
***** modified 24 Nov 2012 *****

clear all
set more off

*********** Define local Variables ***********

local countryname 	"Ethiopia"
local iso			""ETH""
local iso_num		231
local id_0			48
local year 			"2011"
local sourcename 	"DHS"
local webaddress 	"www.measuredhs.com"

local user			"gra"

local filepath		""/Users/`user'/Dropbox/Africa_sub_nat_educ/DHS/Stata_files/`countryname'""
local filename		""ETPR61FL.DTA""

cd `filepath'
use `filename'

capture log close
log using Results/`countryname'_`year'_watsan_household.log, replace

labelbook hv201
labelbook hv205

tab hv024
labelbook hv024

local dhsdefine 	"1 "Tigray" 2 "Afar" 3 "Amhara" 4 "Oromiya" 5 "Somali" 6 "Benshangul-Gumaz" 7 "SNNP" 8 "Gambela" 9 "Harari" 10 "Addis Ababa" 11 "Dire Dawa""
local gadmdefine	"978 "Tigray" 969 "Afar" 970 "Amhara" 975 "Oromiya" 976 "Somali" 971 "Benshangul-Gumaz" 977 "SNNP" 973 "Gambela" 974 "Harari" 968 "Addis Ababa" 972 "Dire Dawa""

local regcount		11
local id_1_start	968

** DHS Region		GADM id_1
local dhs1			978
local dhs2			969
local dhs3			970
local dhs4			975
local dhs5			976
local dhs6			971
local dhs7			977
local dhs8			973
local dhs9			974
local dhs10			968
local dhs11			972

local stratalevels	"hv024 hv025"

*********** svy characterstics are set automatically based on information above ************

capture drop stratavar wt
egen stratavar = group(`stratalevels')
gen wt=hv005/1000000

svyset, clear
svyset hv001 [pw=wt],  strata(stratavar)
*save, replace

*****LOCAL VARIABLES SPECIFIC TO THIS DO FILE***************
local unit %
local sex Both

***** DEFINES NEW REGION VARIABLE BASED ON GADM REGIONS WHEN NECESSARY ****
gen region = hv024

/*****************NUMBER OF REGIONS *********/
**********************************************
* Code corrects for any skipped values in the region variable and 
* temporarily changes the value of hv024 to account for skipped values

levelsof region, local(RegionNoSkips)

local k=1
foreach value of local RegionNoSkips {
	replace region=`k' if region==`value'
	local ++k
	}
lab def dhsdefine `dhsdefine'
lab val region dhsdefine

distinct region 				//count distinct regions

local numreg =r(ndistinct)

* Consider only de jure population (as in DHS report)
keep if hv102==1

* Drop any singleton PSU (stops if they account for more than 5% of observations
svydes, gen(singlestrata)
assert (sum(singlestrata)<(_N/20))
drop if singlestrata == 1
drop singlestrata
********************************************************************************

********************************DEFINITIONS*************************************
* generate country specific improved water and sanitation variables

capture drop impwat
gen impwat = 0
label variable impwat "Access to Improved Water"
replace impwat = . if hv201 == .
replace impwat = . if hv201 == 99
replace impwat = 1 if hv201 == 11 | hv201 == 12 | hv201 == 13
replace impwat = 1 if hv201 == 21 | hv201 == 31 | hv201 == 41 | hv201 == 51
replace impwat = 1 if hv201 == 71 & (hv202 == 11 | hv202 == 12 | hv202 == 13)
replace impwat = 1 if hv201 == 71 & (hv202 == 21 | hv202 == 31 | hv202 == 41 | hv202 == 51)

capture drop impsan
gen impsan = 0
label variable impsan "Access to Improved Sanitation"
replace impsan = . if hv205 == .
replace impsan = . if hv205 == 99
replace impsan = 1 if hv205 == 11 | hv205 == 12 | hv205 == 13
replace impsan = 1 if hv205 == 21 | hv205 == 22
replace impsan = 1 if hv205 == 41

foreach var of varlist hv206 hv207 hv208 {
	replace `var' = . if `var' > 1
	}

capture drop rad_tv
gen rad_tv = 0
replace rad_tv = 1 if hv207 == 1 | hv208 == 1

*******************************************************************************

* 1 - Percentage of population with access to improved water 

svy: mean impwat
estat size

gen indic=1
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean impwat, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San1, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 2 - Percentage of population with access to improved sanitation 

svy: mean impsan
estat size

gen indic=2
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean impsan, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San2, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 3 - Percentage of population with household electricity 

svy: mean hv206
estat size

gen indic=3
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean hv206, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San3, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 4 - Percentage of population with radio in household

svy: mean hv207
estat size

gen indic=4
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean hv207, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San4, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 5 - Percentage of population with television in household

svy: mean hv208
estat size

gen indic=5
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean hv208, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San5, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 6 - Percentage of population with radio and/or television in household

svy: mean rad_tv
estat size

gen indic=6
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean rad_tv, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San6, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

********** COLLAPSE TO HOUSEHOLDS *********************************************

duplicates drop hv001 hv002, force 

*******************************************************************************

* 7 - Percentage of households with access to improved water

svy: mean impwat
estat size

gen indic=7
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean impwat, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San7, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 8 - Percentage of households with access to improved sanitation 

svy: mean impsan
estat size

gen indic=8
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean impsan, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San8, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 9 - Percentage of households with electricity 

svy: mean hv206
estat size

gen indic=9
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean hv206, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San9, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 10 - Percentage of households with radio

svy: mean hv207
estat size

gen indic=10
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean hv207, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San10, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 11 - Percentage of households with television

svy: mean hv208
estat size

gen indic=11
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean hv208, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San11, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*******************************************************************************

* 12 - Percentage of households with radio and/or television

svy: mean rad_tv
estat size

gen indic=12
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean rad_tv, over(region)
estat size
matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
estat effects, deft
matrix dq=e(deft)

forvalues i = 1/`numreg'{
	gen reg`i'=mq[1,`i']
}

forvalues i = 1/`numreg'{
	gen sereg`i'=sq[`i',`i']
}

forvalues i = 1/`numreg'{
	gen sizereg`i'=suq[1,`i']
}

forvalues i = 1/`numreg'{
	gen deftreg`i'=dq[1,`i']
}

savesome indic-deftreg`numreg' if _n==1 using Wat_San12, replace
matrix drop m s su mq sq suq d dq
drop natl senatl sizenatl deftnatl indic


forvalues i = 1/`numreg'{
	drop reg`i'
}

forvalues i = 1/`numreg'{
	drop sereg`i'
}

forvalues i = 1/`numreg'{
	drop sizereg`i'
}

forvalues i = 1/`numreg'{
	drop deftreg`i'
}

*****************************************************************************


*************Append Imp_Water file

use Wat_San1, clear
forval i = 2/12 {
  append using Wat_San`i'.dta
}

label define Indicator 1 "Access to improved water (% of population)"
label define Indicator 2 "Access to improved sanitation (% of population)", add
label define Indicator 3 "Electricity in household (% of population)", add
label define Indicator 4 "Radio in household (% of population)", add
label define Indicator 5 "Television in household (% of population)", add
label define Indicator 6 "Radio and/or Television in household (% of population)", add

label define Indicator 7 "Access to improved water (% of households)", add
label define Indicator 8 "Access to improved sanitation (% of households)", add
label define Indicator 9 "Electricity in household (% of households)", add
label define Indicator 10 "Radio in household (% of households)", add
label define Indicator 11 "Television in household (% of households)", add
label define Indicator 12 "Radio and/or Television in household (% of households)", add
*label define Indicator 5 "Average time to water", add

label values indic Indicator

*****************************************
/* CALCULATE PERCENTAGES AND PROPER STANDARD ERRORS */
replace natl = natl*100
forval i=1/`numreg'{
replace reg`i' = reg`i'*100
}
replace senatl =  (100*sqrt(senatl))  
forval i=1/`numreg'{
replace sereg`i' =  (100*sqrt(sereg`i'))  
}

drop natl senatl deftnatl sizenatl

reshape long reg sereg sizereg deftreg, i(indic) j(subnat)

gen str30 country = "`countryname'"
gen str20 source = "`sourcename'"
gen str30 website = "`webaddress'"
gen str20 unit = "`unit'"
gen str20 year = "`year'"
lab def dhsdefine `dhsdefine'
lab val subnat dhsdefine
rename reg values
rename sereg se
rename sizereg size
rename deftreg deft
 
reshape wide values se size def, i(subnat) j(indic)

rename values1 impwat_pop
rename se1 impwat_pop_se
rename size1 impwat_pop_size
rename deft1 impwat_pop_deft
label var impwat_pop "Access to improved water (% of population)"

rename values2 impsan_pop
rename se2 impsan_pop_se
rename size2 impsan_pop_size
rename deft2 impsan_pop_deft
label var impsan_pop "Access to improved sanitation (% of population)"

rename values3 elec_pop
rename se3 elec_pop_se
rename size3 elec_pop_size
rename deft3 elec_pop_deft
label var elec_pop "Electricity in household (% of population)"

rename values4 radio_pop
rename se4 radio_pop_se
rename size4 radio_pop_size
rename deft4 radio_pop_deft
label var radio_pop "Radio in household (% of population)"

rename values5 tv_pop
rename se5 tv_pop_se
rename size5 tv_pop_size
rename deft5 tv_pop_deft
label var tv_pop "Television in household (% of population)"

rename values6 rad_tv_pop
rename se6 rad_tv_pop_se
rename size6 rad_tv_pop_size
rename deft6 rad_tv_pop_deft
label var rad_tv_pop "Radio and/or Television in household (% of population)"

rename values7 impwat_hh
rename se7 impwat_hh_se
rename size7 impwat_hh_size
rename deft7 impwat_hh_deft
label var impwat_hh "Access to improved water (% of households)"

rename values8 impsan_hh
rename se8 impsan_hh_se
rename size8 impsan_hh_size
rename deft8 impsan_hh_deft
label var impsan_hh "Access to improved sanitation (% of households)"

rename values9 elec_hh
rename se9 elec_hh_se
rename size9 elec_hh_size
rename deft9 elec_hh_deft
label var elec_hh "Electricity in household (% of households)"

rename values10 radio_hh
rename se10 radio_hh_se
rename size10 radio_hh_size
rename deft10 radio_hh_deft
label var radio_hh "Radio in household (% of households)"

rename values11 tv_hh
rename se11 tv_hh_se
rename size11 tv_hh_size
rename deft11 tv_hh_deft
label var tv_hh "Television in household (% of households)"

rename values12 rad_tv_hh
rename se12 rad_tv_hh_se
rename size12 rad_tv_hh_size
rename deft12 rad_tv_hh_deft
label var rad_tv_hh "Radio and/or television in household (% of households)"

drop impwat_pop_deft impsan_pop_deft elec_pop_deft radio_pop_deft tv_pop_deft rad_tv_pop_deft impwat_hh_deft impsan_hh_deft elec_hh_deft radio_hh_deft tv_hh_deft rad_tv_hh_deft

***** THIS CODE IS USED WHEN DHS REGIONS MATCH THE GADM ADM1 REGIONS. *****
***** IT IS THE MOST COMMONLY USED *****

gen id_1 = .
replace id_1 = `dhs1' if subnat == 1
replace id_1 = `dhs2' if subnat == 2
replace id_1 = `dhs3' if subnat == 3
replace id_1 = `dhs4' if subnat == 4
replace id_1 = `dhs5' if subnat == 5
replace id_1 = `dhs6' if subnat == 6
replace id_1 = `dhs7' if subnat == 7
replace id_1 = `dhs8' if subnat == 8
replace id_1 = `dhs9' if subnat == 9
replace id_1 = `dhs10' if subnat == 10
replace id_1 = `dhs11' if subnat == 11
gen name = id_1
lab def name_1 `gadmdefine'
lab val name name_1
decode name, gen(name_1)
drop name
rename country name_0
gen str3 iso = `iso'
gen id_0 = `id_0'
destring year, replace
order id_0 name_0 iso id_1 name_1 year
sort id_1
drop subnat

********************************************************************************

gen iso_num = `iso_num'
gen ccaps_id = id_1 + 230033
lab def ccapsdefine 231001 "Addis Ababa" 231002 "Afar" 231003 "Amhara" 231004 "Benshangul-Gumaz" 231005 "Dire Dawa" 231006 "Gambela Peoples" 231007 "Harari People" 231008 "Oromia" 231009 "Somali" 231010 "Southern Nations, Nationalities and Peoples" 231011 "Tigray"
*lab val ccaps_id ccapsdefine
order id_0 name_0 iso iso_num id_1 ccaps_id name_1 year
drop id_1

********************************************************************************

*save "Results/`countryname'_`year'_watsan_household.dta", replace
*outsheet using "Results/`countryname'_`year'_watsan_household.csv", comma replace

save "/Users/`user'/Dropbox/Africa_sub_nat_educ/DHS/Watsan_Results/`countryname'_`year'_watsan_household.dta", replace

forval i=1/12 {
	erase Wat_San`i'.dta
	}

log close

exit
