***********Regional Level Literacy**********
*********Written by Anustubh Agnihotri based on EPDC do files******
***** modified by Todd G Smith
***** 24 Nov 2012

clear
*set mem 100M
set more off
capture log close
/*
**Allows for user routines (distinct and savesome) to execute on SSC server*****
sysdir set PLUS "U:\My Documents\Stata\ADO"

cd "U:\My Documents\CCAPS\Merge_Male_Female"
log using "Results\EdStats_ET_RegLiteracy.log", replace
use "Ethiopia_Merged_1997.DTA", clear
*/
local user			"gra"

***Local Variables***********
*****CHANGE FOR EACH COUNTRY***************
local countryname	"Ethiopia"
local iso			""ETH""
local iso_num		231
local id_0			48
local year 			2011
local sourcename 	"DHS"
local webaddress 	"www.measuredhs.com"

local unit			%
local gender 		Both

local filepath		"/Users/`user'/Dropbox/Africa_sub_nat_educ/DHS/Stata_files/`countryname'"
local filename		"Ethiopia_Merged_2011.DTA"

cd `filepath'
use `filename'

capture log close
log using Results/`countryname'_`year'_literacy.log, replace

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

local stratalevels	"v024 v025"

/*****************NUMBER OF REGIONS *********/

***** DEFINES NEW REGION VARIABLE BASED ON GADM REGIONS WHEN NECESSARY ****
gen region = v024

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

**********************************************
distinct region 				//count distinct regions

local numreg =r(ndistinct)

* COUNTRY YEAR
* Program calculates education indicators using DHS COUNTRY YEAR

* Consider only de jure population (as in DHS report)
keep if v135==1

* Consider only observations for which literacy or illiteracy can be determined
* program will stop if literacyunknown population exceeds 5% of observations in the sample
capture drop literacyunknown
gen literacyunknown = 1 if (v155==. | v155==3 ) & (v149<3 | v149==.)
assert (sum(literacyunknown)<(_N/20))
drop if literacyunknown==1

* Weight
*gen wt=v005/1000000
*svyset v021 [pw=wt], strata(v022)

*********** svy characterstics are set automatically based on information above ************

capture drop stratavar wt
egen stratavar = group(`stratalevels')
gen wt=v005/1000000

svyset, clear
svyset v001 [pw=wt],  strata(stratavar)

//*****DEFINITIONS***************/

* Literate if
* v155 able to read OR
* v149 educational attainment secondary or higher

capture drop
gen literate=0
replace literate=1 if (v155==1 | v155==2 | v149>=3)


**** AGE GROUPS ***

capture drop age15over
gen age15over=0
replace age15over=1 if (v012>=15 & v012<=999)

capture drop age1524
gen age1524=0
replace age1524=1 if (v012>=15 & v012<=24)

capture drop age1519
gen age1519=0
replace age1519=1 if (v012>=15 & v012<=19)

capture drop age2549
gen age2549=0
replace age2549=1 if (v012>=25 & v012<=49)



//*****CALCULATIONS***************/

***********LITERACY 15 & over***************
svy: mean literate, subpop(age15over)
estat size
estat effect, deft

gen indic=1

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean literate, over(region) subpop(age15over)
estat size
estat effect, deft

matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
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



savesome indic-deftreg`numreg' if _n==1 using result1, replace
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


***********LITERACY 15-24***************
svy: mean literate, subpop(age1524)
estat size
estat effect, deft

gen indic=2

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean literate, over(region) subpop(age1524)
estat size
estat effect, deft

matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
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

savesome indic-deftreg`numreg' if _n==1 using result2, replace
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


***********LITERACY 15-19***************
svy: mean literate, subpop(age1519)
estat size
estat effect, deft

gen indic=3

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean literate, over(region) subpop(age1519)
estat size
estat effect, deft

matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
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

savesome indic-deftreg`numreg' if _n==1 using result3, replace
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

***********LITERACY 25-49***************
svy: mean literate, subpop(age2549)
estat size
estat effect, deft

gen indic=4

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean literate, over(region) subpop(age2549)
estat size
estat effect, deft

matrix mq=e(b)
matrix sq=e(V)
matrix suq=e(_N)
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

savesome indic-deftreg`numreg' if _n==1 using result4, replace
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
*************Append result file*****************

use result1, clear
forval i=2/4{
  append using result`i'.dta
}


label define Indicator 1 "Literacy_15 & over", add
label define Indicator 2 "Literacy_15-24", add
label define Indicator 3 "Literacy_15-19", add
label define Indicator 4 "Literacy_25-49", add
label values indic Indicator

/* CALCULATE PERCENTAGES AND PROPER STANDARD ERRORS */
replace natl = natl*100
forval i=1/`numreg'{
replace reg`i' = reg`i'*100
}
replace senatl =  (100*sqrt(senatl))  
forval i=1/`numreg'{
replace sereg`i' =  (100*sqrt(sereg`i'))  
}

/***CHANGE THIS FOR EACH COUNTRY ***/
/*
rename reg1 Tigray
rename reg2 Affar 
rename reg3 Amhara
rename reg4 Oromiya
rename reg5 Somali
rename reg6 BeniGumuz
rename reg7 SNNP
rename reg8 Gambela
rename reg9 Harari
rename reg10 AddisAbbaba
rename reg11 DireDawa
save "Indicators\EdStats\EdStats_regionliteracy.dta", replace
outsheet using "Indicators\EdStats\EdStats_regionliteracy.csv", replace
*/


/* format for EPDC */
/* Drop National Figures*/
//rename natl reg0
//rename senatl sereg0
//rename sizenatl sizereg0
//rename deftnatl deftreg0
drop natl
drop senatl
drop sizenatl
drop deftnatl

reshape long reg sereg sizereg deftreg, i(indic) j(subnat)



gen str30 country = "`countryname'"
gen str20 source = "`sourcename'"
gen str20 gender = "`gender'"
gen str30 website = "`webaddress'"
gen str20 age = ""
gen str20 unit = "`unit'"
gen str20 year = "`year'"
*label define states `regdefine'
*label values subnat states
lab def dhsdefine `dhsdefine'
lab val subnat dhsdefine
rename reg values
rename sereg se
rename sizereg size
rename deftreg deft


replace age="15 & over" if indic==1
replace age="15-24" if indic==2
replace age="15-19" if indic==3
replace age="25-49" if indic==4

rename values lit
drop deft

reshape wide lit se size age, i(subnat) j(indic)


/*
foreach stub in numlist 1 2 3 4 {
	rename lit`x' = lit_15 & over if `x' == 1	
	}
*/

rename lit1 lit_15_over
rename lit2 lit_15_24
rename lit3 lit_15_19
rename lit4 lit_25_49

rename se1 se_15_over
rename se2 se_15_24
rename se3 se_15_19
rename se4 se_25_49

rename size1 size_15_over
rename size2 size_15_24
rename size3 size_15_19
rename size4 size_25_49

*****From Todd's Participation Do File********
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
order id_0 name_0 iso id_1 name_1 year gender
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

*save "Results/`countryname'_`year'_lit.dta", replace
*outsheet using "Results/`countryname'_`year'_literacy.csv", replace

save "/Users/`user'/Dropbox/Africa_sub_nat_educ/DHS/Lit_Results/`countryname'_`year'_literacy.dta", replace

forval i=1/4{
  erase result`i'.dta
}

log close

exit





