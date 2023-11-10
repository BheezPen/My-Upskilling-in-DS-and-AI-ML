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
log using Results/`countryname'_`year'_reg_educ_part.log, replace

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

********** country specific primary and secondary ages obtained from DHS country reports ******

local p_agel 			7
local lastgradeprim 	8
local lastgradesec 		4

********** these local variables are calculated automatically, but you can change them *****

local s_agel = 		`p_agel' + `lastgradeprim'
local p_ageu = 		`s_agel' - 1
local s_ageu = 		`p_ageu' + `lastgradesec'

*********** svy characterstics are set automatically based on information above ************

capture drop stratavar wt
egen stratavar = group(`stratalevels')
gen wt=hv005/1000000

svyset, clear
svyset hv001 [pw=wt],  strata(stratavar)
save, replace

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

**********************************************
distinct region 				//count distinct regions

local numreg =r(ndistinct)

***************************************************************
* Consider only de jure population (as in DHS report)
keep if hv102==1

* Check that, when missing or unknown values of hv121, hv122, or hv123 are ignored,
* they do not account for more than 5% of observations
gen unknown121 =1 if hv121==. 
gen unknown122 =1 if hv122==. 
gen unknown123 =1 if hv123==. & (hv121==1|hv121==2)
assert (sum(unknown121)<(_N/20))
assert (sum(unknown122)<(_N/20))
assert (sum(unknown123)<(_N/20))
drop unknown121 unknown122 unknown123

* Drop any singleton PSU (stops if they account for more than 5% of observations
svydes, gen(singlestrata)
assert (sum(singlestrata)<(_N/20))
drop if singlestrata == 1
drop singlestrata
******************************************************************


***********************************************DEFINITIONS****************************************************************

capture drop edu
gen edu=0 if hv121 < 8
replace edu=1 if hv121==1|hv121==2

******AGE GROUPS*******

capture drop age611
gen age611=0 if hv105 < 98
replace age611=1 if hv105>5 & hv105<12

capture drop age614
gen age614=0  if hv105<98
replace age614=1 if hv105>5 & hv105<15

capture drop age1214
gen age1214=0  if hv105<98
replace age1214=1 if hv105>11 & hv105<15

capture drop age1517
gen age1517=0  if hv105<98
replace age1517=1 if hv105>14 & hv105<18

capture drop ageprim
gen ageprim=0 if hv105<98
replace ageprim=1 if (hv105>=`p_agel' & hv105 <=`p_ageu')

capture drop agesec
gen agesec=0 if hv105<98
replace agesec=1 if (hv105>=`s_agel' & hv105 <=`s_ageu')

capture drop pagel //Those of the official age of Primary G1
gen pagel=0 if hv105<98
replace pagel=1 if hv105==`p_agel'

* Number of students in grade 1 who are older than official grade 1 age
capture drop overagegrade1
gen overagegrade1=0 if (hv122<8 & hv105<98)
replace overagegrade1=1 if (hv122==1 & hv123==1  & edu==1)  & (hv105>`p_agel' & hv105<.)

******SCHOOLING GROUPS*********

capture drop out
gen out=0 if hv121 < 8
replace out=1 if hv121==0

capture drop prim
gen prim=0 if (hv122<8)
replace prim=1 if (hv123>=1 & hv123<=`lastgradeprim') & edu==1 & hv122==1
replace prim=1 if hv122==1

capture drop sec
gen sec=0 if (hv122<8)
replace sec=1 if (hv123>=1 & hv123<=`lastgradesec') & edu==1 & hv122==2
replace sec=1 if hv122==2

capture drop grade1  // Attending Primry G1
gen grade1=0 if (hv122<8)
replace grade1=1 if hv122==1 & hv123==1  & edu==1


***********************************************CALCULATIONS****************************************************************




**************SCHOOL PARTICIPATION********************** 

* 1- School participation rate, 6-11

* participation rates for age between 6-11, net attendance rate
svy: mean edu, subpop(age611)
* show size of subpopulations
estat size
* store Part_Results in various variables

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

svy: mean edu, over(region)  subpop(age611)
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


savesome indic-deftreg`numreg' if _n==1 using Part_Result1, replace
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


***************************************************************************

* 2- School participation rate, 12-14

svy: mean edu,  subpop(age1214)
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

svy: mean edu, over(region)  subpop(age1214)
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


savesome indic-deftreg`numreg' if _n==1 using Part_Result2, replace
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

****************************************************************************

* 3- School participation rate, 15-17

gen indic=3

svy: mean edu,   subpop(age1517)
estat size
matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
estat effects, deft
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean edu, over(region)  subpop(age1517)
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

savesome indic-deftreg`numreg' if _n==1 using Part_Result3, replace
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

* 4- Net primary attendance rate: Official primary school 

svy: mean prim, subpop(ageprim)
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

svy: mean prim, over(region) subpop(ageprim)
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

savesome indic-deftreg`numreg' if _n==1 using Part_Result4, replace
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

* 5- Gross primary attendance rate: official primary school


***Calculate the ratio of number of students in primary to the total population of the national 
***official primary  school age

svy: total ageprim, subpop(ageprim)			//to get size of sub populations
matrix su=e(_N)

svy: ratio prim/ageprim 				//calc gross attendance
estat effects, deft

gen indic=5
matrix m=e(b)
matrix s=e(V)
matrix d=e(deft)

gen natl=m[1,1]						//store gar, std. error, deft
gen senatl=s[1,1]
gen deftnatl=d[1,1]
gen sizenatl=su[1,1]

svy: total ageprim, over(region) subpop(ageprim)	//to get size of sub populations
matrix suq=e(_N)
svy: ratio prim/ageprim, over(region) 		//calc gross attendance by region
estat effect, deft

matrix mq=e(b)						//store gar, s.e., deft
matrix sq=e(V)
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

savesome indic-deftreg`numreg' if _n==1 using Part_Result5, replace
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

******************************************************************************


* 6- Net Secondary attendance rate: Official secondary school age

svy: mean sec, subpop(agesec)
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

svy: mean sec, over(region) subpop(agesec)
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

savesome indic-deftreg`numreg' if _n==1 using Part_Result6, replace
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

*7- Gross secondary attendance rate: official secondary school

***Calculate number of secondary school aged population
gen indic=7
svy: total agesec, subpop(agesec) 			//to get size of sub populations
matrix su=e(_N)

svy: total agesec, over(region) subpop(agesec)	//to get size of sub populations
matrix suq=e(_N)

***Calculate ratio of the two numbers
svy: ratio sec/agesec
estat effect, deft

matrix m=e(b)
matrix s=e(V)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen deftnatl=d[1,1]
gen sizenatl=su[1,1]

svy: ratio sec/agesec , over(region) 
estat effect, deft

matrix mq=e(b)
matrix sq=e(V)
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

savesome indic-deftreg`numreg' if _n==1 using Part_Result7, replace
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

***************************************************
* 8- Gross grade 1 intake rate



gen indic=8
***Calculate the total pop of official prim school age
svy: total pagel, subpop(pagel)
estat size
matrix su=e(_N)

svy: total pagel, over(region) subpop(pagel)
estat size 
*svy: total grade1, over(region) subpop(grade1) 
matrix suq=e(_N)


***Calculate the ratio of the two numbers
svy: ratio grade1/pagel
estat effect, deft

matrix m=e(b)
matrix s=e(V)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen deftnatl=d[1,1]
gen sizenatl=su[1,1]

svy: ratio grade1/pagel, over(region)
estat effect, deft

matrix mq=e(b)
matrix sq=e(V)
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

savesome indic-deftreg`numreg' if _n==1 using Part_Result8, replace
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

***********************************************************************************

*9- Net grade 1 intake rate


**Determine the indicator
gen indic=9
svy: mean grade1, subpop(pagel) 
estat size
estat effect, deft

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen deftnatl=d[1,1]
gen sizenatl=su[1,1]

svy: mean grade1, over(region) subpop(pagel) 
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

savesome indic-deftreg`numreg' if _n==1 using Part_Result9, replace
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

***********************************************************************************

*10- % of grade 1 students who are older than the official grade 1 age 

* Determine the indicator
gen indic=10
svy: mean overagegrade1, subpop(grade1) 
estat size
estat effect, deft

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean overagegrade1, over(region) subpop(grade1) 
estat effect, deft
estat size

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

savesome indic-deftreg`numreg' if _n==1 using Part_Result10, replace
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

***********************************************
**********Out-of-school children

*11- % of out of primary school children

svy: mean out, subpop(ageprim) 
estat size
estat effects

gen indic=11

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean out, over(region) subpop(ageprim) 
estat size
estat effects

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

savesome indic-deftreg`numreg' if _n==1 using Part_Result11, replace
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

**********************************************

*12- % of out of secondary school children 


svy: mean out, subpop(agesec)
estat size
estat effects

gen indic=12

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean out, over(region) subpop(agesec) 
estat size
estat effects

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

savesome indic-deftreg`numreg' if _n==1 using Part_Result12, replace
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

*********************************************

*13- % of out of school children, 6-11


svy: mean out, subpop(age611) 
estat size
estat effects

gen indic=13

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean out, over(region) subpop(age611) 
estat size
estat effects

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

savesome indic-deftreg`numreg' if _n==1 using Part_Result13, replace
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



**********************************************

*14-  % of out of school children, 6-14

svy: mean out, subpop(age614) 
estat size
estat effects

gen indic=14

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]


svy: mean out, over(region) subpop(age614) 
estat size
estat effects

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

savesome indic-deftreg`numreg' if _n==1 using Part_Result14, replace
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

*********************************************

*15-  % of out of school children, 15-17

svy: mean out , subpop(age1517)
estat size
estat effects

gen indic=15

matrix m=e(b)
matrix s=e(V)
matrix su=e(_N)
matrix d=e(deft)

gen natl=m[1,1]
gen senatl=s[1,1]
gen sizenatl=su[1,1]
gen deftnatl=d[1,1]

svy: mean out, over(region) subpop(age1517) 
estat size
estat effects

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

savesome indic-deftreg`numreg' if _n==1 using Part_Result15, replace
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


*************Append Part_Result file

use Part_Result1, clear
forval i=2/15{
  append using Part_Result`i'.dta
}

label define Indicator 1 "School Partcipation Rate-6-11"
label define Indicator 2 "School Partcipation Rate-12-14", add
label define Indicator 3 "School Partcipation Rate-15-17", add
label define Indicator 4 "Net primary attendance rate (%)", add
label define Indicator 5 "Gross primary attendance rate (%)", add
label define Indicator 6 "Net secondary attendance rate (%)", add
label define Indicator 7 "Gross secondary attendance rate (%)", add
label define Indicator 8 "Gross grade 1 intake rate (%)", add
label define Indicator 9 "Net grade 1 intake rate (%)", add
label define Indicator 10 "Grade 1 students who are older than the official Grade 1 age (%)",add
label define Indicator 11 "% of out-of-primary-school children",add
label define Indicator 12 "% of out-of-secondary-school children",add
label define Indicator 13 "% of out-of-school children, 6-11",add
label define Indicator 14 "% of out-of-school children, 6-14",add
label define Indicator 15 "% of out-of-school children, 15-17",add


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
gen str20 gender = "`sex'"
gen str30 website = "`webaddress'"
gen str20 level = ""
gen str20 age = ""
gen str20 unit = "`unit'"
gen str20 year = "`year'"
lab def dhsdefine `dhsdefine'
lab val subnat dhsdefine
rename reg values
rename sereg se
rename sizereg size
rename deftreg deft
 
replace level="Primary" if indic==4|indic==5|indic==11 
replace level="Secondary" if indic==6|indic==7|indic==12 
replace level="grade 1" if indic==8|indic==9|indic==10 
replace age="6-11" if indic==1|indic==13 
replace age="15-17" if indic==3|indic==15 
replace age="12-14" if indic==2 
replace age="6-14" if indic==14

keep if indic > 3 & indic < 8

drop level
reshape wide values se size def, i(subnat) j(indic)

rename values4 pri_nar
rename se4 pri_nar_se
rename size4 pri_nar_size
rename deft4 pri_nar_deft
label var pri_nar "Net primary attendance rate (%)"

rename values5 pri_gar
rename se5 pri_gar_se
rename size5 pri_gar_size
rename deft5 pri_gar_deft
label var pri_gar "Gross primary attendance rate (%)"

rename values6 sec_nar
rename se6 sec_nar_se
rename size6 sec_nar_size
rename deft6 sec_nar_deft
label var sec_nar "Net secondary attendance rate (%)"

rename values7 sec_gar
rename se7 sec_gar_se
rename size7 sec_gar_size
rename deft7 sec_gar_deft
label var sec_gar "Gross secondary attendance rate (%)"

drop age 
drop pri_nar_deft pri_gar_deft sec_nar_deft sec_gar_deft
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
order id_0 name_0 iso iso_num id_1 ccaps_id name_1 year gender
drop id_1

********************************************************************************

*save "Results/`countryname'_`year'_reg_educ_part.dta", replace
*outsheet using "Results/`countryname'_`year'_reg_educ_part.csv", comma replace

save "/Users/`user'/Dropbox/Africa_sub_nat_educ/DHS/Part_Results/`countryname'_`year'_reg_educ_part.dta", replace

forval i=1/15 {
	erase Part_Result`i'.dta
	}

log close

exit
