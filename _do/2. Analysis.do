/*******************************************************************************
****		Effect of Don Berna's Extradition on Homicides in Medellin		****
****					Do-File for Replicating Results						****
****			Created by: Greg Haugan /// Created on: June 20, 2023		****
****						haugan-gregory@norc.org							****
*******************************************************************************/


clear all 
global path "H:/4. Personal/Violence_Education"
cap mkdir "$path/_out"
cap mkdir "$path/_out/$S_DATE"
cap mkdir "$path/_out/$S_DATE/Graphs"

set more off
set matsize 2000


***** Figure 1: Detailed Medellin Trends Graph
import excel using "$path/_dta/RawData/Medellin_Homicide_Trends.xlsx" , clear firstrow

keep if inrange(year , 2004 , 2013)
twoway (scatteri 0 2008 200 2008 200 2012 0 2012 , recast(area) ///
		color("207 205 201") ylabel( , labsize(medium) glcolor("207 205 201") ///
		angle(0) notick labcolor(black)) yscale(noline) xsize(7.5) legend(off) ///
			graphregion(color(white)))|| ///
	(line homicides_per_100000 year, lcolor("108 100 099") ///
		sort mcolor(black) ///
		xlabel(2004(2)2012) xtick(2004(2)2012) xlabel(2004(2)2012 , valuelabel) ///
		xtitle("Year") ytitle("Homicides per 100,000 inhabitants") ///
		yscale(noline)) || ///
	(scatter homicides_per_100000 year , msymbol(circle) msize(huge) ///
		mlab(homicides_per_100000) 	mlabpos(0) mlabcolor(white) mcolor("108 100 099"))
graph save "$path/_out/$S_DATE/Graphs/Figure1_Medellin_HomicidesPer.gph" , replace
graph export "$path/_out/$S_DATE/Graphs/Medellin_HomicidesPer.pdf" , replace




*****	Figure 3: Berna Homicide Trends 
* Trendline Graph
use "$path/_dta/MonthlyHomicides_ByComuna.dta" , clear
keep if inrange(year , 2004 , 2013)

collapse (mean) homicides homicides_per , by(berna_comuna month)

sort berna_comuna month
by berna_comuna: gen period = _n

reshape wide homicides homicides_per , i(month) j(berna_comuna)


twoway (lpoly homicides0 period, sort scheme(s1mono) mcolor(black) yline(0, lwidth(vvvthin)) ///
		xlabel(1 "2004" 25 "2006" 49 "2008" 73 "2010" 97 "2012" /*121 "2014"*/ , valuelabel)  ///
		xtitle("Year") ytitle("Homicides per Comuna") xline(/*44*/ 53 , lpattern(dash)) ///
		title("Panel A: Homicides in Berna-Controlled and Non-Berna Comunas (Monthly)" , size(small)) ///
		note("Note: Trend lines use a local polynomial smoothing function.", size(vsmall) color(black)) ///
		legend(order(1 "Non-Berna Comunas" 2 "Berna-Controlled Comunas")) legend(region(lwidth(none)))) ///
	   (lpoly homicides1 period, sort lcolor(gs12) lpattern(line)) 
graph save "$path/_out/$S_DATE/Graphs/Homicides_ByBerna.gph" , replace


*	Berna Homicide Map
tempfile 2007_map // Tempfiles for maps
tempfile 2010_map

*Create 2007 homicides file
use "$path/_dta/RawData/Medellin_crimes.dta" , clear
keep if year == 2007 & delito == "Homicidio Comun"

*Format date
replace fecha = substr(fecha,1,length(fecha)-7)
gen date = date(fecha, "DMY")
format date %td
gen Month = substr(fecha,4,.)
gen month = monthly(Month, "MY")
format month %tm
drop Month

*Identify the comuna where each incident took place
geoinpoly y x using "$path/_dta/RawData/Shapefiles/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$path/_dta/RawData/Shapefiles/comunadb.dta" , gen(comuna_merge) keepus(NUMERO_COM)
destring NUMERO_COM , gen(comuna)
drop if comuna == . 

**Generate dummy for Don Berna Comunas
gen berna_comuna = (comuna == 1 | comuna == 13 | comuna == 8 | comuna == 3 | comuna == 5 | comuna == 6)
label var berna_comuna "Berna-Controlled Comuna"

**Generate dummies for post-Berna time periods
gen post_transfer = (date >= 17395) // Berna Prison Transfer on August 17, 2007 (17395 in Stata date)
label var post_transfer "Post-Prison Transfer Period"
gen post_extradition = (date >= 17665) // Berna Extradition on May 13 2008 (17665 in Stata date)
label var post_extradition "Post-Extradition Period"

save "$path/temp_2007.dta" , replace
*Create 2010 homicides file
use "$path/_dta/RawData/Medellin_crimes.dta" , clear
keep if year == 2010 & delito == "Homicidio Comun"

*Format date
replace fecha = substr(fecha,1,length(fecha)-7)
gen date = date(fecha, "DMY")
format date %td
gen Month = substr(fecha,4,.)
gen month = monthly(Month, "MY")
format month %tm
drop Month

*Identify the comuna where each incident took place
geoinpoly y x using "$path/_dta/RawData/Shapefiles/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$path/_dta/RawData/Shapefiles/comunadb.dta" , gen(comuna_merge) keepus(NUMERO_COM)
destring NUMERO_COM , gen(comuna)
drop if comuna == . 

**Generate dummy for Don Berna Comunas
gen berna_comuna = (comuna == 1 | comuna == 13 | comuna == 8 | comuna == 3 | comuna == 5 | comuna == 6)
label var berna_comuna "Berna-Controlled Comuna"

**Generate dummies for post-Berna time periods
gen post_transfer = (date >= 17395) // Berna Prison Transfer on August 17, 2007 (17395 in Stata date)
label var post_transfer "Post-Prison Transfer Period"
gen post_extradition = (date >= 17665) // Berna Extradition on May 13 2008 (17665 in Stata date)
label var post_extradition "Post-Extradition Period"

save "$path/temp_2010.dta" , replace

*2007 Map
use "$path/_dta/RawData/Shapefiles/comunadb.dta" , clear
**Identifying Don Berna Comunas
destring NUMERO_COM , gen(comuna)
gen berna_comuna = (comuna == 1 | comuna == 13 | comuna == 8 | comuna == 3 | comuna == 5 | comuna == 6)
label var berna_comuna "Berna-Controlled Neighborhood"

spmap berna_comuna using "$path/_dta/RawData/Shapefiles/comunacoord.dta" , id(comunaid) ocolor(black) ///
osize(vthin) point(data("$path/temp_2007.dta") xcoord(x) ycoord(y) size(small) fcolor(black) ///
shape(smcircle) ocolor(black) osize(vvvthin) legenda(off)) ///
title("2007", ring(1) position(12)) legenda(off)
graph save "`2007_map'" , replace

*2010 Map
spmap berna_comuna using "$path/_dta/RawData/Shapefiles/comunacoord.dta" , id(comunaid) ocolor(black) ///
osize(vthin) point(data("$path/temp_2010.dta") xcoord(x) ycoord(y) size(small) fcolor(black) ///
shape(smcircle) ocolor(black) osize(vvvthin) legenda(off)) ///
title("2010", ring(1) position(12)) legenda(off)
graph save "`2010_map'" , replace

*Combine maps
graph combine "`2007_map'" "`2010_map'" , ///
graphregion(color(white)) iscale(*.75) col(3) title("Panel B: Geographical Distribution of Homicides" , size(small) color(black)) ///
note("Note: Berna-controlled comunas denoted by dark shading. Black dots represent homicides.", size(vsmall))
graph save "$path/_out/$S_DATE/Graphs/Berna_map.gph" , replace

erase "$path/temp_2007.dta"
erase "$path/temp_2010.dta"


*****	Combine Homicide Trendline and Maps
graph combine "$path/_out/$S_DATE/Graphs/Homicides_ByBerna.gph" ///
	"$path/_out/$S_DATE/Graphs/Berna_map.gph" , rows(2) graphregion(color(white))
graph save "$path/_out/$S_DATE/Graphs/Figure2_Berna_Graphic.gph" , replace

erase "$path/_out/$S_DATE/Graphs/Homicides_ByBerna.gph"
erase "$path/_out/$S_DATE/Graphs/Berna_map.gph"



*********
use "$path/_dta/ViolenceOnEducation.dta" , clear


drop if YEAR == 2014 // only 2004 - 2013
keep if ESTU_EDAD >=16 & ESTU_EDAD <= 19 // only 16-19 year-old students
drop if shift == 5 // don't include weekend students
drop if FAMI_ING_FMILIAR_MENSUAL == 7 // rafa drops these. not sure we need to.

replace DANE_sede = COLE_CODIGO_COLEGIO if mi(DANE_sede) // one school missing DANE_sede

*Gernate control vars
bys DANE_sede YEAR : gen number_students = _N // sum number of students by school
bys comuna YEAR DANE_sede : gen temp = 1 if _n ==1 // sum number of schools by comuna
bys comuna YEAR : egen number_schools = total(temp)
drop temp
bys DANE_sede YEAR: egen pct_female = mean(female) // calculate percent female by school
replace pct_female = pct_female * 100
tab ESTU_EDAD , gen(ESTU_EDAD_) // age dummies
tab shift , gen(shift_) // shift dummies
gen missing_mother = mi(mother_dropout) // dummy for missing mother education
replace mother_dropout = 2 if mi(mother_dropout) // recode for missing mother education
tab mother_dropout , gen(mother_dropout_) // mother ed dummies
gen missing_strata = inlist(ESTU_ESTRATO , 8 , .) // dummy for missing strata
replace ESTU_ESTRATO = 7 if inlist(ESTU_ESTRATO , 8 , .) // recode for missing strata
tab ESTU_ESTRATO , gen(ESTU_ESTRATO_) // gen strata dummies
gen log_number_schools = log(number_schools) , after(number_schools) // take logs
gen log_population = log(population) , after(population)
gen log_public_investment = log(public_investment) , after(public_investment)
gen log_number_students = log(number_students)
gen log_vis_unidades_school = log(vis_unidades_school + 1)
gen adult = inrange(ESTU_EDAD , 18 , 19) if !mi(ESTU_EDAD)
gen high_strata = inrange(ESTU_ESTRATO , 3 , 6) if missing_strata != 1
gen high_income = inrange(FAMI_ING_FMILIAR_MENSUAL , 3, 7) if !mi(FAMI_ING_FMILIAR_MENSUAL)
label var number_schools "Total Number of Secondary Schools in Comuna"
label var pct_female "Percentage of students presenting exam who are female"
label var number_students "Number of students presenting exam"
label var vis_unidades_school "Number of Active Units of Social Interest Housing Within 500m of School"
label var ESTU_EDAD_1 "Age=16"
label var ESTU_EDAD_2 "Age=17"
label var ESTU_EDAD_3 "Age=18"
label var ESTU_EDAD_4 "Age=19"
label var ESTU_ESTRATO_1 "Socioeconomic Strata=1"
label var ESTU_ESTRATO_2 "Socioeconomic Strata=2"
label var ESTU_ESTRATO_3 "Socioeconomic Strata=3"
label var ESTU_ESTRATO_4 "Socioeconomic Strata=4"
label var ESTU_ESTRATO_5 "Socioeconomic Strata=5"
label var ESTU_ESTRATO_6 "Socioeconomic Strata=6"
label var ESTU_ESTRATO_7 "Missing socioeconomic strata data"
local lab : var label mother_dropout
label var mother_dropout_2 "`lab'"
label var mother_dropout_3 "Missing mother education data"
label var adult "Student Age 18+"
label var high_strata "Strata 3+"
label var high_income "Family income 2 monthly minimum wages or higher"

foreach var of varlist shift_*{
	local lab : var label `var'
	local lab = subinstr("`lab'" , "==" , "=" , 1)
	label var `var' "School `lab'"
}

tab comuna , gen(comuna_)	// comuna dummies
tab YEAR , gen(YEAR_) // year dummies
foreach year of numlist 1/10{ // berna-year interactions
	gen berna_comuna_`year' = YEAR_`year' * berna_comuna
}

tab DANE_sede , gen(DANE_sede_) // school dummies

gen log_math = log(MATEMATICAS_PUNT) // log of math scores

gen post2008 = 0 // post extradition dummy
replace post2008=1 if YEAR>2008
gen berna_post_2008= berna_comuna*post2008 // berna-post extradition interaction
label var berna_post_2008 "Berna Comuna X Post-Extradition Period"

cap drop interaction_strength_transfer // wasn't defined correctly
gen interaction_strength_transfer = BernaStrength * post2008
label var interaction_strength_transfer "Berna Strength X Post-Extradition Period"


gen post2005 = 0 // for placebo
replace post2005=1 if YEAR>2005
gen berna_post_2005= berna_comuna*post2005

gen year = YEAR // for parallel trends slope test
replace year = 1 if year <= 2008

*Other crimes
egen Manslaughter_1yr = rowtotal(Manslaughter_m1_250m-Manslaughter_m12_250m)	
label var Manslaughter_1yr "Manslaughter within 250m of school - 12-month period before exam"
egen Drug_Arrests_1yr = rowtotal(Drug_Arrests_m?_250m Drug_Arrests_m10_250m ///
	Drug_Arrests_m11_250m Drug_Arrests_m12_250m)	
label var Drug_Arrests_1yr "Drug Arrests within 250m of school - 12-month period before exam"
	  
egen Street_Muggings_1yr = rowtotal(Street_Muggings_m?_250m Street_Muggings_m10_250m ///
	Street_Muggings_m11_250m Street_Muggings_m12_250m)	
label var Street_Muggings_1yr "Muggings within 250m of school - 12-month period before exam"  
egen Home_Burglaries_1yr = rowtotal(Home_Burglaries_m?_250m Home_Burglaries_m10_250m ///
	Home_Burglaries_m11_250m Home_Burglaries_m12_250m)	
label var Home_Burglaries_1yr "Home Burglaries within 250m of school - 12-month period before exam"
egen Auto_Theft_1yr = rowtotal(Auto_Theft_m?_250m Auto_Theft_m10_250m ///
	Auto_Theft_m11_250m Auto_Theft_m12_250m)	
label var Auto_Theft_1yr "Auto Theft within 250m of school - 12-month period before exam"
egen All_Robberies_1yr = rowtotal(Street_Muggings_1yr Home_Burglaries_1yr Auto_Theft_1yr)	
label var All_Robberies_1yr "All Robberies within 250m of school - 12-month period before exam" 


* Table 1
estpost tabstat MATEMATICAS_PUNT /// Dependent Variable 
	Hom_250_yr1 /// Independent Variables
	public_investment population number_schools /// comuna controls - public investment is different from rafa's. population fractionally off
	pct_female number_students settlement_extent_school vis_unidades_school /// school - pct_female fractionally off (think rafa reset missing to zero)
	female ESTU_EDAD_* shift_* mother_dropout_2 mother_dropout_3 ///  student controls 
	ESTU_ESTRATO_* /// mother dropout is different (think rafa wasn't accounting for missingness)
	if MATEMATICAS_PUNT != 0 , /// 
	statistics(mean sd) columns(statistics)
esttab using "$path/_out/$S_DATE/Table1.tex" , ///
	replace label cells("mean(fmt(3)) sd(fmt(3))") ///
	addnote("Note: Descriptive statistics calculated over all students in the sample sitting for Saber 11 ICFES exam. There are three fewer observations for the student-level female variable due to missing observations.")	
			


* Table 2
preserve

collapse Hom_250_yr1 Hom_500_yr1 Hom_750_yr1 log_math /// DVs
	comuna berna_comuna post2008 post2005 berna_post_2008 berna_post_2005 /// main IVs
	number_schools population public_investment /// comuna-year IVs
	log_number_schools log_population log_public_investment ///	
	female log_number_students log_vis_unidades_school settlement_extent_school /// school-year
	comuna_1 - comuna_16 YEAR_* berna_comuna_* DANE_sede_* , ///
	by(DANE_sede YEAR)

	/// Homicides Panel A
mean Hom_250_yr1 if berna_comuna == 0 & post2008 == 0 ///
	& inlist(YEAR , 2006 , 2007 , 2009 , 2010) , vce(cluster comuna) 
mean Hom_250_yr1 if berna_comuna == 1 & post2008 == 0 ///
	& inlist(YEAR , 2006 , 2007 , 2009 , 2010) , vce(cluster comuna) 
mean Hom_250_yr1 if berna_comuna == 0 & post2008 == 1 ///
	& inlist(YEAR , 2006 , 2007 , 2009 , 2010) , vce(cluster comuna) 
mean Hom_250_yr1 if berna_comuna == 1 & post2008 == 1 ///
	& inlist(YEAR , 2006 , 2007 , 2009 , 2010) , vce(cluster comuna) 
	
reg Hom_250_yr1 berna_comuna post2008 berna_post_2008 ///
	if inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna)
local indep_vars: colnames e(b)
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses', cluster(comuna) nograph seed(123023)

reg Hom_250_yr1 post2008 ///
	if berna_comuna == 0 & inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna) 
reg Hom_250_yr1 post2008 ///
	if berna_comuna == 1 & inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna) 
reg Hom_250_yr1 berna_comuna ///
	if post2008 == 0 & inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna) 
reg Hom_250_yr1 berna_comuna ///
	if post2008 == 1 & inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna) 	

	/// Homicides Panel B
mean Hom_250_yr1 if berna_comuna == 0 & post2005 == 0 ///
	& inlist(YEAR , 2004 , 2005 , 2006 , 2007) , vce(cluster comuna) 
mean Hom_250_yr1 if berna_comuna == 1 & post2005 == 0 ///
	& inlist(YEAR , 2004 , 2005 , 2006 , 2007) , vce(cluster comuna) 
mean Hom_250_yr1 if berna_comuna == 0 & post2005 == 1 ///
	& inlist(YEAR , 2004 , 2005 , 2006 , 2007) , vce(cluster comuna) 
mean Hom_250_yr1 if berna_comuna == 1 & post2005 == 1 ///
	& inlist(YEAR , 2004 , 2005 , 2006 , 2007) , vce(cluster comuna) 
	
reg Hom_250_yr1 berna_comuna post2005 berna_post_2005 /// 
	if inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna)
local indep_vars: colnames e(b)
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses', cluster(comuna) nograph seed(123023)	

reg Hom_250_yr1 post2005 ///
	if berna_comuna == 0 & inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna) 
reg Hom_250_yr1 post2005 ///
	if berna_comuna == 1 & inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna) 
reg Hom_250_yr1 berna_comuna ///
	if post2005 == 0 & inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna) 
reg Hom_250_yr1 berna_comuna ///
	if post2005 == 1 & inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna) 
	
restore	
	
	/// Math Scores Panel A
mean log_math if berna_comuna == 0 & post2008 == 0 ///
	& inlist(YEAR , 2006 , 2007 , 2009 , 2010) , vce(cluster comuna) 
mean log_math if berna_comuna == 1 & post2008 == 0 ///
	& inlist(YEAR , 2006 , 2007 , 2009 , 2010) , vce(cluster comuna) 
mean log_math if berna_comuna == 0 & post2008 == 1 ///
	& inlist(YEAR , 2006 , 2007 , 2009 , 2010) , vce(cluster comuna) 
mean log_math if berna_comuna == 1 & post2008 == 1 ///
	& inlist(YEAR , 2006 , 2007 , 2009 , 2010) , vce(cluster comuna) 
	
reg log_math berna_comuna post2008 berna_post_2008 /// 
	if inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna)
local indep_vars: colnames e(b)
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses', cluster(comuna) nograph seed(123023)	
	
reg log_math post2008 ///
	if berna_comuna == 0 & inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna) 
reg log_math post2008 ///
	if berna_comuna == 1 & inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna) 
reg log_math berna_comuna ///
	if post2008 == 0 & inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna) 
reg log_math berna_comuna ///
	if post2008 == 1 & inlist(YEAR , 2006 , 2007 , 2009 , 2010) , ///
	vce(cluster comuna) 		
	
	/// Math Scores Panel B
mean log_math if berna_comuna == 0 & post2005 == 0 ///
	& inlist(YEAR , 2004 , 2005 , 2006 , 2007) , vce(cluster comuna) 
mean log_math if berna_comuna == 1 & post2005 == 0 ///
	& inlist(YEAR , 2004 , 2005 , 2006 , 2007) , vce(cluster comuna) 
mean log_math if berna_comuna == 0 & post2005 == 1 ///
	& inlist(YEAR , 2004 , 2005 , 2006 , 2007) , vce(cluster comuna) 
mean log_math if berna_comuna == 1 & post2005 == 1 ///
	& inlist(YEAR , 2004 , 2005 , 2006 , 2007) , vce(cluster comuna) 
	
reg log_math berna_comuna post2005 berna_post_2005 /// 
	if inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna)
local indep_vars: colnames e(b)
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses', cluster(comuna) nograph seed(123023)		
	
reg log_math post2005 ///
	if berna_comuna == 0 & inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna) 
reg log_math post2005 ///
	if berna_comuna == 1 & inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna) 
reg log_math berna_comuna ///
	if post2005 == 0 & inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna) 
reg log_math berna_comuna ///
	if post2005 == 1 & inlist(YEAR , 2004 , 2005 , 2006 , 2007) , ///
	vce(cluster comuna) 
	


	
* Table 3	
preserve

collapse Hom_250_yr1 Hom_500_yr1 Hom_750_yr1 /// DVs
	comuna berna_comuna interaction_comuna_transfer /// main IVs
	log_number_schools log_population log_public_investment /// comuna-year IVs
	female log_number_students log_vis_unidades_school settlement_extent_school /// school-year
	comuna_1 - comuna_16 YEAR_* berna_comuna_* DANE_sede_* year , ///
	by(DANE_sede YEAR)
	

	*Col 1 - Comuna FE, no controls
local rega = "rega"
eststo `rega': reghdfe Hom_250_yr1 i.berna_comuna##i.YEAR , /// run model
	noconstant absorb(comuna) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "Y"
estadd local school_fex= "N"
estadd local comuna_c  = "N"
estadd local school_c  = "N"

su Hom_250_yr1 if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `rega'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `rega'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `rega'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `rega'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `rega'
	
	
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }

reghdfe Hom_250_yr1 i.berna_comuna##c.YEAR i.berna_comuna##i.year , ///
	noconstant absorb(comuna) vce(cluster comuna) // parallel trends slope test	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `rega'
	
	
	*Col 2 - School FE, no controls	
local regb = "regb"
eststo `regb': reghdfe Hom_250_yr1 i.berna_comuna##i.YEAR , /// run model
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "N"
estadd local school_c  = "N"

su Hom_250_yr1 if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `regb'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `regb'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `regb'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `regb'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)	
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `regb'
	
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
	
matrix boot_pval = J(1,`n_vars',.)
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }
  
reghdfe Hom_250_yr1 i.berna_comuna##c.YEAR i.berna_comuna##i.year , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) // parallel trends slope test	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `regb'	
	
	
	*Col 3 - School FE, comuna controls		
local regc = "regc"
eststo `regc': reghdfe Hom_250_yr1 i.berna_comuna##i.YEAR /// run model
	log_number_schools log_population log_public_investment , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "Y"
estadd local school_c  = "N"

su Hom_250_yr1 if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `regc'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `regc'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val	
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `regc'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `regc'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)	
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `regc'
		
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
	
matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }
	
reghdfe Hom_250_yr1 i.berna_comuna##i.year i.berna_comuna##c.YEAR ///
	log_number_schools log_population log_public_investment , /// parallel trends slope test
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `regc'	



	*Col 4 - School FE, comuna and school-year controls			
local regd = "regd"
eststo `regd': reghdfe Hom_250_yr1 i.berna_comuna##i.YEAR /// run model
	log_number_schools log_population log_public_investment ///
	female log_number_students log_vis_unidades_school settlement_extent_school , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "Y"
estadd local school_c  = "Y"

su Hom_250_yr1 if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd'  

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `regd'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `regd'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val	
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `regd'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `regd'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `regd'
	
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
	
matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }


reghdfe Hom_250_yr1 i.berna_comuna##i.year i.berna_comuna##c.YEAR ///
	log_number_schools log_population log_public_investment /// parallel trends slope test
	female log_number_students log_vis_unidades_school settlement_extent_school , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `regd'	 


	*Col 5 - 500m homicides with School FE, comuna and school-year controls				
local rege = "rege"
eststo `rege': reghdfe Hom_500_yr1 i.berna_comuna##i.YEAR /// run model
	log_number_schools log_population log_public_investment ///
	female log_number_students log_vis_unidades_school settlement_extent_school , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "Y"
estadd local school_c  = "Y"

su Hom_500_yr1 if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 
	
local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `rege'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `rege'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val	
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `rege'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `rege'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)	
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `rege'
	
boottest `indep_hypotheses', /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023)

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }

reghdfe Hom_500_yr1 i.berna_comuna##i.year i.berna_comuna##c.YEAR ///
	log_number_schools log_population log_public_investment /// parallel trends slope test
	female log_number_students log_vis_unidades_school settlement_extent_school , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `rege'
	

	*Col 6 - 750m homicides with School FE, comuna and school-year controls					
local regf = "regf"
eststo `regf': reghdfe Hom_750_yr1 i.berna_comuna##i.YEAR /// run model
	log_number_schools log_population log_public_investment ///
	female log_number_students log_vis_unidades_school settlement_extent_school , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "Y"
estadd local school_c  = "Y"

su Hom_750_yr1 if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `regf'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `regf'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val	
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `regf'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `regf'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)	
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `regf'
	
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
	
matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }		

reghdfe Hom_750_yr1 i.berna_comuna##i.year i.berna_comuna##c.YEAR ///
	log_number_schools log_population log_public_investment /// parallel trends slope test
	female log_number_students log_vis_unidades_school settlement_extent_school , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `regf'


restore


esttab  `rega' `regb' `regc'  `regd' `rege'  `regf' using ///
	"$path/_out/$S_DATE/Table3.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 500 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 750 Meters \\ Around Schools}")  ///
	keep(1.berna_comuna#2005.YEAR 1.berna_comuna#2006.YEAR ///
		1.berna_comuna#2007.YEAR 1.berna_comuna#2008.YEAR 1.berna_comuna#2009.YEAR ///
		1.berna_comuna#2010.YEAR 1.berna_comuna#2011.YEAR 1.berna_comuna#2012.YEAR ///
		1.berna_comuna#2013.YEAR)  ///
	order(1.berna_comuna#2005.YEAR 1.berna_comuna#2006.YEAR ///
		1.berna_comuna#2007.YEAR 1.berna_comuna#2008.YEAR 1.berna_comuna#2009.YEAR ///
		1.berna_comuna#2010.YEAR 1.berna_comuna#2011.YEAR 1.berna_comuna#2012.YEAR ///
		1.berna_comuna#2013.YEAR) ///
	stats(obs joint_p partrend_p average_pre pre2008_p average_post post2008_p ///
		mny sdy comuna_fex school_fex comuna_c school_c ,  ///
		labels("Observations" "Joint significance of 2005-2008 interactions p-value" ///
		"Parallel trends slope test p-value" "Average 2005-2008 effect" ///
		"Average 2005-2008 effect p-value" "Average post-2008 treatment effect" ///
		"Average post-2008 treatment effect p-value" "D.V. Mean" ///
		"D.V. Standard Deviation" "Comuna FE" "School FE" "Comuna Controls" ///
		"School Controls"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include year fixed effects. Controls at the comuna level are public investment, population, and number of schools. Controls at the school level are percentage of students who are female, number of students, human built settlements within 500 meters of the school, and number of public housing units within 500 meters of the school. Don Berna was extradited May 2008.")	
			
		
	
	
*Table 4	

	*Col 1 - Comuna FE, no controls
local rega = "rega"
eststo `rega': reghdfe log_math i.berna_comuna##i.YEAR , /// run model
	noconstant absorb(comuna) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "Y"
estadd local school_fex= "N"
estadd local comuna_c  = "N"
estadd local school_c  = "N"
estadd local ind_c = "N" 
estadd local socio_c = "N"

su MATEMATICAS_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `rega'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `rega'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `rega'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `rega'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `rega'
	
	
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }


reghdfe log_math i.berna_comuna##c.YEAR i.berna_comuna##i.year , ///
	noconstant absorb(comuna) vce(cluster comuna) // parallel trends slope test	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `rega'
	
	
	*Col 2 - School FE, no controls	
local regb = "regb"
eststo `regb': reghdfe log_math i.berna_comuna##i.YEAR , /// run model
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "N"
estadd local school_c  = "N"
estadd local ind_c = "N" 
estadd local socio_c = "N"

su MATEMATICAS_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `regb'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `regb'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `regb'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `regb'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)	
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `regb'
	
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
	
matrix boot_pval = J(1,`n_vars',.)
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }
  
reghdfe log_math i.berna_comuna##c.YEAR i.berna_comuna##i.year , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) // parallel trends slope test	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `regb'	
	
	
	*Col 3 - School FE, comuna controls		
local regc = "regc"
eststo `regc': reghdfe log_math i.berna_comuna##i.YEAR /// run model
	log_number_schools log_population log_public_investment , ///
	 noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "Y"
estadd local school_c  = "N"
estadd local ind_c = "N" 
estadd local socio_c = "N"

su MATEMATICAS_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `regc'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `regc'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val	
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `regc'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `regc'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)	
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `regc'
		
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
	
matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }
	
reghdfe log_math i.berna_comuna##i.year i.berna_comuna##c.YEAR ///
	log_number_schools log_population log_public_investment , /// parallel trends slope test
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `regc'	


	*Col 4 - School FE, comuna and school-year controls			
local regd = "regd"
eststo `regd': reghdfe log_math i.berna_comuna##i.YEAR /// run model
	log_number_schools log_population log_public_investment ///
	pct_female log_number_students log_vis_unidades_school settlement_extent_school , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "Y"
estadd local school_c  = "Y"
estadd local ind_c = "N" 
estadd local socio_c = "N"

su MATEMATICAS_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd'  

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `regd'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `regd'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val	
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `regd'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `regd'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `regd'
	
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
	
matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }


reghdfe log_math i.berna_comuna##i.year i.berna_comuna##c.YEAR ///
	log_number_schools log_population log_public_investment /// parallel trends slope test
	pct_female log_number_students log_vis_unidades_school settlement_extent_school , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `regd'	 


	*Col 5 - School FE, and comuna, school-year, and individual controls (except strata)				
local rege = "rege"
eststo `rege': reghdfe log_math i.berna_comuna##i.YEAR /// run model
	log_number_schools log_population log_public_investment ///
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother, ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "Y"
estadd local school_c  = "Y"
estadd local ind_c = "Y" 
estadd local socio_c = "N"

su MATEMATICAS_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 
	
local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `rege'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `rege'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val	
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `rege'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `rege'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)	
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `rege'
	
boottest `indep_hypotheses', /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023)

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }

reghdfe log_math i.berna_comuna##i.year i.berna_comuna##c.YEAR ///
	log_number_schools log_population log_public_investment /// parallel trends slope test
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `rege'
	

	*Col 6 - School FE, and comuna, school-year, and all individual controls 
local regf = "regf"
eststo `regf': reghdfe log_math i.berna_comuna##i.YEAR /// run model
	log_number_schools log_population log_public_investment ///
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
estadd local obs=e(N)    // store additional info for table
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local comuna_c  = "Y"
estadd local school_c  = "Y"
estadd local ind_c = "Y" 
estadd local socio_c = "Y"

su MATEMATICAS_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean'
estadd local sdy = `sd' 

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest (1.berna_comuna#2005.YEAR = 0) (1.berna_comuna#2006.YEAR = 0) /// pre-2008 
	(1.berna_comuna#2007.YEAR = 0) (1.berna_comuna#2008.YEAR = 0) , /// joint sig test
	cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'	
estadd local joint_p = `temp' : `regf'
local temp : di %9.3f ((_b[1.berna_comuna#2005.YEAR] + /// average pre-period effect
	_b[1.berna_comuna#2006.YEAR] + _b[1.berna_comuna#2007.YEAR] + ///
	_b[1.berna_comuna#2008.YEAR]) / 4)
estadd local average_pre = `temp' : `regf'
boottest (((_b[1.berna_comuna#2005.YEAR] + _b[1.berna_comuna#2006.YEAR] + /// 
	_b[1.berna_comuna#2007.YEAR] + _b[1.berna_comuna#2008.YEAR]) / 4) = 0) , ///
	cluster(comuna) nograph seed(123023) // pre-2008 av treat effect p-val	
local temp : di %9.3f `r(p)'	
estadd local pre2008_p = `temp' : `regf'
local temp : di %9.3f ((_b[1.berna_comuna#2009.YEAR] + /// average post-period effect
	_b[1.berna_comuna#2010.YEAR] + _b[1.berna_comuna#2011.YEAR] + ///
	_b[1.berna_comuna#2012.YEAR] + _b[1.berna_comuna#2013.YEAR]) / 5)
estadd local average_post = `temp' : `regf'
boottest (((_b[1.berna_comuna#2009.YEAR] + _b[1.berna_comuna#2010.YEAR] + /// 
	_b[1.berna_comuna#2011.YEAR] + _b[1.berna_comuna#2012.YEAR] ///
	+ _b[1.berna_comuna#2013.YEAR]) / 5) = 0) , /// post-2008 av treat effect p-val
	cluster(comuna) nograph seed(123023)	
local temp : di %9.3f `r(p)'		
estadd local post2008_p = `temp' : `regf'
	
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
	
matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}		

foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }		

reghdfe log_math i.berna_comuna##i.year i.berna_comuna##c.YEAR ///
	log_number_schools log_population log_public_investment /// parallel trends slope test
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 	
boottest 1.berna_comuna#c.YEAR , cluster(comuna) nograph seed(123023)
local temp : di %9.3f `r(p)'		
estadd local partrend_p  = `temp' : `regf'


esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/Table4.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}") ///
	keep(1.berna_comuna#2005.YEAR 1.berna_comuna#2006.YEAR ///
		1.berna_comuna#2007.YEAR 1.berna_comuna#2008.YEAR 1.berna_comuna#2009.YEAR ///
		1.berna_comuna#2010.YEAR 1.berna_comuna#2011.YEAR 1.berna_comuna#2012.YEAR ///
		1.berna_comuna#2013.YEAR 1.mother_dropout)  ///
	order(1.berna_comuna#2005.YEAR 1.berna_comuna#2006.YEAR ///
		1.berna_comuna#2007.YEAR 1.berna_comuna#2008.YEAR 1.berna_comuna#2009.YEAR ///
		1.berna_comuna#2010.YEAR 1.berna_comuna#2011.YEAR 1.berna_comuna#2012.YEAR ///
		1.berna_comuna#2013.YEAR 1.mother_dropout) ///
	stats(obs joint_p partrend_p average_pre pre2008_p average_post post2008_p ///
		mny sdy comuna_fex school_fex comuna_c school_c ind_c socio_c,  ///
		labels("Observations" "Joint significance of pre-2008 interactions p-value" ///
		"Parallel trends slope test p-value" "Average pre-2008 effect" ///
		"Average pre-2008 effect p-value" "Average post-2008 treatment effect" ///
		"Average post-2008 treatment effect p-value" "Math Score Mean" ///
		"Math Score Standard Deviation" "Comuna FE" "School FE" "Comuna Controls" ///
		"School Controls" "Individual Controls Without Socioecnomic Stratum" ///
		"Socioeconomic Stratum"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include year fixed effects. Controls defined in Table 1. Missing values of mother dropout and socioeconomic strata are replaced with zero and indicator variables of missingness are included in the regression.")	




	
*Table 5 - Panel A	

	*Col 1 - Comuna FE, no controls
local rega = "rega"
eststo `rega': reghdfe log_math i.berna_comuna berna_post_2008 post2008 , /// run model
	noconstant absorb(comuna) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "Y"
estadd local school_fex= "N"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }


	*Col 2 - School FE, no controls
local regb = "regb"
eststo `regb': reghdfe log_math i.berna_comuna berna_post_2008 post2008 , /// run model
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }


 	*Col 3 - School FE, all controls
local regc = "regc"
eststo `regc': reghdfe log_math i.berna_comuna berna_post_2008 post2008 /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  } 
  
  
  
  	*Col 4 - School FE, all controls and comuna time trend
local regd = "regd"
eststo `regd': reghdfe log_math i.berna_comuna berna_post_2008 post2008 /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  } 
 
 
 
  	*Col 5 - School FE, all controls, comuna time trend, and 2004 levels X Year indicator
local rege = "rege"
eststo `rege': reghdfe log_math i.berna_comuna berna_post_2008 post2008 /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	i.YEAR##c.HDI2004 , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "Y"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }  


esttab  `rega' `regb' `regc' `regd' `rege' using ///
	"$path/_out/$S_DATE/Table5_PanelA.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}") ///
	keep(berna_post_2008 1.mother_dropout)  ///
	order(berna_post_2008 1.mother_dropout) ///
	stats(obs comuna_fex school_fex controls time_trend level_year_interact ,  ///
		labels("Observations" "Comuna FE" "School FE" "All Controls" ///
		"Comuna Time Trend" "Comuna Variables in 2004 X Year Indicators"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. Observations correspond to panel A. All regressions include year fixed effects. Controls defined in Table 1. Missing values of mother dropout and socioeconomic strata are replaced with zero and indicator variables of missingness are included in the regression.")

  
  
  
  
  
  
 *Table 5 - Panel B	

	*Col 1 - Comuna FE, no controls
local rega = "rega"
eststo `rega': reghdfe log_math i.berna_comuna berna_post_2005 post2005 /// run model
	if inrange(YEAR , 2004 , 2008) , ///
	noconstant absorb(comuna) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "Y"
estadd local school_fex= "N"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }


	*Col 2 - School FE, no controls
local regb = "regb"
eststo `regb': reghdfe log_math i.berna_comuna berna_post_2005 post2005 /// run model
		if inrange(YEAR , 2004 , 2008) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }


 	*Col 3 - School FE, all controls
local regc = "regc"
eststo `regc': reghdfe log_math i.berna_comuna berna_post_2005 post2005 /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if inrange(YEAR , 2004 , 2008) , ///	
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  } 
  
  
  
  	*Col 4 - School FE, all controls and comuna time trend
local regd = "regd"
eststo `regd': reghdfe log_math i.berna_comuna berna_post_2005 post2005 /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	if inrange(YEAR , 2004 , 2008) , ///	
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  } 
 
 
 
  	*Col 5 - School FE, all controls, comuna time trend, and 2004 levels X Year indicator
local rege = "rege"
eststo `rege': reghdfe log_math i.berna_comuna berna_post_2005 post2005 /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	i.YEAR##c.HDI2004 ///
	if inrange(YEAR , 2004 , 2008) , ///	
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "Y"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }  


esttab  `rega' `regb' `regc' `regd' `rege' using ///
	"$path/_out/$S_DATE/Table5_PanelB.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}") ///
	keep(berna_post_2005 1.mother_dropout)  ///
	order(berna_post_2005 1.mother_dropout) ///
	stats(obs comuna_fex school_fex controls time_trend level_year_interact ,  ///
		labels("Observations" "Comuna FE" "School FE" "All Controls" ///
		"Comuna Time Trend" "Comuna Variables in 2004 X Year Indicators"))	///
	
  
  
  

	
*Table 6 - Panel A

	*Col 1 - Comuna FE, no controls
local rega = "rega"
eststo `rega': reghdfe Hom_250_yr1 berna_post_2008 i.YEAR ///
	if !mi(log_math) , /// run model
	noconstant absorb(comuna) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "Y"
estadd local school_fex= "N"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }

ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	if !mi(log_math) , /// get first-stage F
	noconstant absorb(comuna) cluster(comuna) first
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'
  

	*Col 2 - School FE, no controls
local regb = "regb"
eststo `regb': reghdfe Hom_250_yr1 berna_post_2008 i.YEAR ///
	if !mi(log_math) , /// run model
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }

ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	if !mi(log_math) , /// get first-stage F
	noconstant absorb(DANE_sede) cluster(comuna) first
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

 	*Col 3 - School FE, all controls
local regc = "regc"
eststo `regc': reghdfe Hom_250_yr1 berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table
*estadd local F = e(F)
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  } 
  
ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , /// get first-stage F
	noconstant absorb(DANE_sede) cluster(comuna) first
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'
  
  	*Col 4 - School FE, all controls and comuna time trend
local regd = "regd"
eststo `regd': reghdfe Hom_250_yr1 berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table
*estadd local F = e(F)
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  } 
 
ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	if !mi(log_math) , /// get first-stage F
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(i.comuna##c.YEAR)
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'
 
 
  	*Col 5 - School FE, all controls, comuna time trend, and 2004 levels X Year indicator
local rege = "rege"
eststo `rege': reghdfe Hom_250_yr1 berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	i.YEAR##c.HDI2004 ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "Y"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }  

ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	i.YEAR##c.HDI2004 ///
	if !mi(log_math) , /// get first-stage F
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(i.comuna##c.YEAR i.YEAR##c.HDI2004)
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

esttab  `rega' `regb' `regc' `regd' `rege' using ///
	"$path/_out/$S_DATE/Table6_PanelA.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}") ///
	keep(berna_post_2008 1.mother_dropout)  ///
	order(berna_post_2008 1.mother_dropout) ///
	stats(obs F1 comuna_fex school_fex controls time_trend level_year_interact ,  ///
		labels("Observations" "First-Stage F" "Comuna FE" "School FE" "All Controls" ///
		"Comuna Time Trend" "Comuna Variables in 2004 X Year Indicators"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include year fixed effects. Controls defined in Table 1. Missing values of mother dropout and socioeconomic strata are replaced with zero and indicator variables of missingness are included in the regression.")  
  
  

	
*Table 6 - Panel B

	*Col 1 - Comuna FE, no controls
local rega = "rega"
eststo `rega': reghdfe Hom_250_yr1 berna_post_2005 i.YEAR ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , /// run model
	noconstant absorb(comuna) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "Y"
estadd local school_fex= "N"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }

ivreghdfe log_math (Hom_250_yr1 = berna_post_2005 ) i.YEAR ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , /// get first-stage F
	noconstant absorb(comuna) cluster(comuna) first
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'
  

	*Col 2 - School FE, no controls
local regb = "regb"
eststo `regb': reghdfe Hom_250_yr1 berna_post_2005 i.YEAR ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , /// run model
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }

ivreghdfe log_math (Hom_250_yr1 = berna_post_2005 ) i.YEAR ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , /// get first-stage F
	noconstant absorb(DANE_sede) cluster(comuna) first
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

 	*Col 3 - School FE, all controls
local regc = "regc"
eststo `regc': reghdfe Hom_250_yr1 berna_post_2005 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table
*estadd local F = e(F)
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  } 
  
ivreghdfe log_math (Hom_250_yr1 = berna_post_2005 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , /// get first-stage F
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'
  
  	*Col 4 - School FE, all controls and comuna time trend
local regd = "regd"
eststo `regd': reghdfe Hom_250_yr1 berna_post_2005 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table
*estadd local F = e(F)
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  } 
 
ivreghdfe log_math (Hom_250_yr1 = berna_post_2005 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , /// get first-stage F
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR)
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'
 
 
  	*Col 5 - School FE, all controls, comuna time trend, and 2004 levels X Year indicator
local rege = "rege"
eststo `rege': reghdfe Hom_250_yr1 berna_post_2005 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	i.YEAR##c.HDI2004 ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "Y"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }  

ivreghdfe log_math (Hom_250_yr1 = berna_post_2005 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	i.YEAR##c.HDI2004 ///
	if !mi(log_math) & inrange(YEAR , 2004 , 2008) , /// get first-stage F
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR i.YEAR##c.HDI2004)
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

esttab  `rega' `regb' `regc' `regd' `rege' using ///
	"$path/_out/$S_DATE/Table6_PanelB.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}" ///
		"\specialcell{Homicides\\ 250 Meters \\ Around Schools}") ///
	keep(berna_post_2005 1.mother_dropout)  ///
	order(berna_post_2005 1.mother_dropout) ///
	stats(obs F1 comuna_fex school_fex controls time_trend level_year_interact ,  ///
		labels("Observations" "First-Stage F" "Comuna FE" "School FE" "All Controls" ///
		"Comuna Time Trend" "Comuna Variables in 2004 X Year Indicators"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include year fixed effects. Controls defined in Table 1. Missing values of mother dropout and socioeconomic strata are replaced with zero and indicator variables of missingness are included in the regression.")  
  
  
  
  
 
*Table 7 - Panel A	

	*Col 1 - Comuna FE, no controls
local rega = "rega"
eststo `rega': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR , ///
	noconstant absorb(comuna) cluster(comuna) first

estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "Y"
estadd local school_fex= "N"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }


	*Col 2 - School FE, no controls
local regb = "regb"
eststo `regb': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR , /// run model
	noconstant absorb(DANE_sede) cluster(comuna) first

estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }


 	*Col 3 - School FE, all controls
local regc = "regc"
eststo `regc': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  } 
  
  
  
  	*Col 4 - School FE, all controls and comuna time trend
local regd = "regd"
eststo `regd': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR , /// 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(i.comuna##c.YEAR)

estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "N"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  } 
 
 
 
  	*Col 5 - School FE, all controls, comuna time trend, and 2004 levels X Year indicator
local rege = "rege"
eststo `rege': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	i.YEAR##c.HDI2004 , /// 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR i.YEAR##c.HDI2004)

estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "Y"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }  


esttab  `rega' `regb' `regc' `regd' `rege' using ///
	"$path/_out/$S_DATE/Table7_PanelA.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}") ///
	keep(Hom_250_yr1 1.mother_dropout)  ///
	order(Hom_250_yr1 1.mother_dropout) ///
	stats(obs F1 comuna_fex school_fex controls time_trend level_year_interact ,  ///
		labels("Observations" "First-Stage F" "Comuna FE" "School FE" "All Controls" ///
		"Comuna Time Trend" "Comuna Variables in 2004 X Year Indicators"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include year fixed effects. Controls defined in Table 1. Missing values of mother dropout and socioeconomic strata are replaced with zero and indicator variables of missingness are included in the regression.")
 
  
  
  
*Table 7 - Panel B	

	*Col 1 - Comuna FE, no controls
local rega = "rega"
eststo `rega': reghdfe log_math Hom_250_yr1 i.YEAR , /// run model
	noconstant absorb(comuna) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "Y"
estadd local school_fex= "N"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }


	*Col 2 - School FE, no controls
local regb = "regb"
eststo `regb': reghdfe log_math Hom_250_yr1 i.YEAR , /// run model
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "N"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }


 	*Col 3 - School FE, all controls
local regc = "regc"
eststo `regc': reghdfe log_math Hom_250_yr1 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "N" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  } 
  
  
  
  	*Col 4 - School FE, all controls and comuna time trend
local regd = "regd"
eststo `regd': reghdfe log_math Hom_250_yr1 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "N"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  } 
 
 
 
  	*Col 5 - School FE, all controls, comuna time trend, and 2004 levels X Year indicator
local rege = "rege"
eststo `rege': reghdfe log_math Hom_250_yr1 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.comuna##c.YEAR ///
	i.YEAR##c.HDI2004 , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	
estadd local comuna_fex  = "N"
estadd local school_fex= "Y"
estadd local controls  = "Y"
estadd local time_trend = "Y" 
estadd local level_year_interact = "Y"

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }  


esttab  `rega' `regb' `regc' `regd' `rege' using ///
	"$path/_out/$S_DATE/Table7_PanelB.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores}") ///
	keep(Hom_250_yr1 1.mother_dropout)  ///
	order(Hom_250_yr1 1.mother_dropout) ///
	stats(obs comuna_fex school_fex controls time_trend level_year_interact ,  ///
		labels("Observations" "Comuna FE" "School FE" "All Controls" ///
		"Comuna Time Trend" "Comuna Variables in 2004 X Year Indicators"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include year fixed effects. Controls defined in Table 1. Missing values of mother dropout and socioeconomic strata are replaced with zero and indicator variables of missingness are included in the regression.")

  
    
  
*Table 8  

 	*Col 1 - Manslaughter

local rega = "rega"
eststo `rega': reghdfe Manslaughter_1yr berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table	

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }  	
  
su Manslaughter_1yr if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `rega'
estadd local sdy = `sd' 	: `rega' 
	
 	*Col 2 - Drug Arrests

local regb = "regb"
eststo `regb': reghdfe Drug_Arrests_1yr berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)	
estadd local obs=e(N)    // store additional info for table	

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }  

su Drug_Arrests_1yr if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regb'
estadd local sdy = `sd' 	: `regb' 
 
 
 	*Col 3 - Muggings
	
local regc = "regc"
eststo `regc': reghdfe Street_Muggings_1yr berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)	  
estadd local obs=e(N)    // store additional info for table	

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }  

su Street_Muggings_1yr if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regc'
estadd local sdy = `sd' 	: `regc'   
  
 	*Col 4 - Muggings

local regd = "regd"
eststo `regd': reghdfe Home_Burglaries_1yr berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)	  
estadd local obs=e(N)    // store additional info for table	

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }  

su Home_Burglaries_1yr if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regd'
estadd local sdy = `sd' 	: `regd'    


 	*Col 5 - Auto Theft

local rege = "rege"
eststo `rege': reghdfe Auto_Theft_1yr berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)	  
estadd local obs=e(N)    // store additional info for table	

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }  

su Auto_Theft_1yr if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `rege'
estadd local sdy = `sd' 	: `rege'   

  
 	*Col 6 - All robberies
  	
local regf = "regf"
eststo `regf': reghdfe All_Robberies_1yr berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)  
estadd local obs=e(N)    // store additional info for table

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }  
	
su All_Robberies_1yr if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regf'
estadd local sdy = `sd' 	: `regf'   

  
esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/Table8.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Manslaughter}" ///
		"\specialcell{Drug Arrests}" ///
		"\specialcell{Street Muggings}" ///
		"\specialcell{Home Burglaries}" ///
		"\specialcell{Auto Theft}" ///
		"\specialcell{All Robberies}") ///
	keep(berna_post_2008)  ///
	order(berna_post_2008) ///
	stats(obs mny sdy ,  ///
		labels("Observations" "D.V. Mean" "D.V. Standard Deviation"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include year and school fixed effects, and all controls defined in Table 1. Missing values of mother dropout and socioeconomic strata are replaced with zero and indicator variables of missingness are included in the regression. Regressions estimated with OLS.")  
  
  
  
  
  
*Table 9    

 	*Col 1 - female
  	
local rega = "rega"
eststo `rega': reghdfe female berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)  
estadd local obs=e(N)    // store additional info for table

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }  
	
su female if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `rega'
estadd local sdy = `sd' 	: `rega'   


 	*Col 2 - Number of students
  	
local regb = "regb"
eststo `regb': reghdfe number_students berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)   
estadd local obs=e(N)    // store additional info for table

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }  
	
su number_students if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regb'
estadd local sdy = `sd' 	: `regb'   
  
 	
 	*Col 3 - Adult
  	
local regc = "regc"
eststo `regc': reghdfe adult berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)   
estadd local obs=e(N)    // store additional info for table

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }  
	
su adult if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regc'
estadd local sdy = `sd' 	: `regc'   

	
 	*Col 4 - Mother dropout
  	
local regd = "regd"
eststo `regd': reghdfe mother_dropout_2 berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) & mother_dropout_3 != 1 , ///
	noconstant absorb(DANE_sede) vce(cluster comuna)   
estadd local obs=e(N)    // store additional info for table

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }  
	
su mother_dropout_2 if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regd'
estadd local sdy = `sd' 	: `regd'   
  
	
 	*Col 5 - Higher strata
  	
local rege = "rege"
eststo `rege': reghdfe high_strata berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	if !mi(log_math) & missing_strata != 1, ///
	noconstant absorb(DANE_sede) vce(cluster comuna)  
estadd local obs=e(N)    // store additional info for table

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }  
	
su high_strata if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `rege'
estadd local sdy = `sd' 	: `rege'   
  
	
 	*Col 6 - Income
  	
local regf = "regf"
eststo `regf': reghdfe high_income berna_post_2008 i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !mi(log_math) & inrange(YEAR , 2008 , 2013) , /// income only available after 2007
	noconstant absorb(DANE_sede) vce(cluster comuna) 
estadd local obs=e(N)    // store additional info for table

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }  
	
su high_income if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regf'
estadd local sdy = `sd' 	: `regf'   
  
  
esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/Table9.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Female \\ Indicator}" ///
		"\specialcell{Number of \\ Students}" ///
		"\specialcell{Adult \\ Indicator}" ///
		"\specialcell{Mother has \\ Elementary Ed. \\ or Less}" ///
		"\specialcell{Strata 3+}" ///
		"\specialcell{Family Income \geq 2 \\ Min. Wages \\ (2008-2013)}") ///
	keep(berna_post_2008)  ///
	order(berna_post_2008) ///
	stats(obs mny sdy ,  ///
		labels("Observations" "D.V. Mean" "D.V. Standard Deviation"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include year and school fixed effects, and all controls defined in Table 1, excluding the dependent variable. Missing values of mother dropout and socioeconomic strata are replaced with zero and indicator variables of missingness are included in the regression, except in Columns (4) and (5), which use only non-missing values of mother's education and strata, respectively. Regressions estimated with OLS.")    
  
  
*Table 10 
  
  	*Col 1 - School FE, all controls with school time trends, 2004-2013
cap egen school = group(DANE_sede) // just running with i.DANE_sede throws error, so regenerate
local rega = "rega"
eststo `rega': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.school##c.YEAR , /// 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(i.school##c.YEAR)

estadd local obs=e(N)    // store additional info for table	
estadd local period = "2004-2013"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }   
   
 
  	*Col 2 - School FE, all controls with other crimes, 2004-2013
local regb = "regb"
eststo `regb': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	Manslaughter_1yr Drug_Arrests_1yr Street_Muggings_1yr Home_Burglaries_1yr ///
	Auto_Theft_1yr , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
estadd local period = "2004-2013"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }  
  
 	*Col 3 - Berna Strength as Instrument
local regc = "regc"
eststo `regc': ivreghdfe log_math (Hom_250_yr1 = interaction_strength_transfer ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }   
  
  
 	*Col 4 - Only comunas that share a border 
local regd = "regd"
eststo `regd': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata ///
	if !inlist(comuna , 14 , 15 , 16) , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }       
  
 	*Col 5 - Math scores (in levels) 
local rege = "rege"
eststo `rege': ivreghdfe MATEMATICAS_PUNT (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }       
  
  
 	*Col 6 - Homcides 1 week before exam 
replace Homicide_wk1_250m = 0 if Homicide_wk1_250m == .	// missings here are really zeros
	
local regf = "regf"
eststo `regf': reghdfe log_math Homicide_wk1_250m i.YEAR /// run model
	log_number_schools log_population log_public_investment /// 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) vce(cluster DANE_sede) 	

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regf'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(DANE_sede) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }           
  
  	*Col 7 - School FE, all controls, Comuna 7 as treatment, 2004-2013
preserve
replace berna_post_2008 = 1 if comuna == 7 & post_extradition == 1
local regg = "regg"
eststo `regg': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
estadd local period = "2004-2013"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regg'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regg'
  }      
 
esttab  `rega' `regb' `regc' `regd' `rege' `regf' `regg' using ///
	"$path/_out/$S_DATE/Table10.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math\\ Test Scores \\ School Time \\ Trends}" ///
	"\specialcell{Log of Math\\ Test Scores \\ Controlling for \\ Other Crimes}" ///	
	"\specialcell{Log of Math\\ Test Scores \\ Berna Strength \\ Instrument}" ///
	"\specialcell{Log of Math\\ Test Scores \\ Dropping Non- \\ Neighboring Comunas}" ///
	"\specialcell{Math Test \\ Scores (in levels)}" ///
	"\specialcell{Log of Math\\ Test Scores \\ OLS \\ Homicides 1 week \\ Before Exam}" ///
	"\specialcell{Log of Math\\ Test Scores \\ Comuna 7 as \\ Berna-Controlled}") ///
	keep(Hom_250_yr1 Homicide_wk1_250m 1.mother_dropout)  ///
	order(Hom_250_yr1 Homicide_wk1_250m 1.mother_dropout) ///
	stats(obs F1 ,  ///
		labels("Observations" "First-Stage F"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses, except for Column (6) which reports the p-value from standard errors clustered at the school level. All regressions include controls defined in Table 1. Column (3) is estimated with 2SLS using an interaction of the post-extradition period indicator with the level of Don Berna's strength in each comuna as an instrument for homicides. Column (6) is estimated with OLS. All other regressions estimated with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. The post-extradition period is 2009-2013.")     
restore   
  
  
*Table 11  
  
  	*Col 1 - School FE, all controls, 2008-2013
local rega = "rega"
eststo `rega': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata if inrange(YEAR , 2008 , 2013), ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
estadd local period = "2008-2013"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  } 
   
  
  	*Col 2 - School FE, all controls with family income, 2008-2013
local regb = "regb"
eststo `regb': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata i.FAMI_ING_FMILIAR_MENSUAL ///
	if inrange(YEAR , 2008 , 2013), ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
estadd local period = "2008-2013"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }   
  
  
 	*Col 3 - Randomly selected treatment
cap drop placebo placebo_post2008
set seed 123023
cap bys comuna : gen placebo = runiform(0,1) if _n == 1
sort placebo
replace placebo = (_n <= 6) if !mi(placebo) //  select 6 random comunas
bys comuna : egen temp = mean(placebo)
replace placebo = temp
drop temp
gen placebo_post2008 = placebo * inrange(YEAR , 2009 , 2013)

	
local regc = "regc"
eststo `regc': ivreghdfe log_math (Hom_250_yr1 = placebo_post2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table
estadd local period = "2004-2013"	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }
   
  
  	*Col 4 - School-shift FE, all controls, 2004-2013
cap egen schoolshift = group(DANE_sede shift)
local regd = "regd"
eststo `regd': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(schoolshift) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
estadd local period = "2004-2013"
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }    
  
 
esttab  `rega' `regb' `regc' `regd' using ///
	"$path/_out/$S_DATE/Table11.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math\\ Test Scores}" ///
		"\specialcell{Log of Math\\ Test Scores \\ Controlling for \\ Family Income}" ///
		"\specialcell{Log of Math\\ Test Scores \\ Randomly Selected \\ Berna-Controlled \\ Comunas}" ///
		"\specialcell{Log of Math\\ Test Scores \\ School-Shift FE}") ///
	keep(Hom_250_yr1 1.mother_dropout)  ///
	order(Hom_250_yr1 1.mother_dropout) ///
	stats(obs period F1 ,  ///
		labels("Observations" "Time Period" "First-Stage F"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include all controls defined in Table 1, as well as year and school fixed effects, except for Column (4) which uses school-shift fixed effects instead of school-level. All regressions estimated with 2SLS, where the interaction of Berna-controlled comuna and the post-extradition period is an instrument for homicides. The post-extradition period is 2009-2013. Data on family income is only available for 2008-2013.")    

   
  
 
*Figure 7 - robustness of results to dropping individual comunas from berna_controlled group
cap mat drop b // drop matrices for storing results, if they exist
cap mat drop CI
levelsof comuna if berna_comuna == 1 , local(comunas)  // local with list of Berna comunas
foreach comuna in `comunas'{

preserve

replace berna_post_2008 = 0 if comuna == `comuna' // drop comuna from berna-controlled group
ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// re-run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

matrix b = nullmat(b) , _b[Hom_250_yr1] // store coefficient
	
boottest Hom_250_yr1 , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 
matrix CI = nullmat(CI) \ r(CI)  // store confidence interval

restore

}

mat colnames b = 1 3 5 6 8 13
matrix CI = CI'
coefplot mat(b), ci(CI) vertical yline(0 , lstyle(solid)) ///
	xtitle("Excluded Comuna" , color(black) size(medium)) ///
	ytitle("Effect of Homicides on Log Math Scores" , color(black) size(medium)) ///
	graphregion(color(white)) ///
	ylabel( , labsize(medium) glcolor(gs15) glstyle(solid) angle(0) notick /// 
		labcolor(black)) ///
	mcolor(black) ciopts(recast(rcap) lcolor(black))
graph save "$path/_out/$S_DATE/Graphs/Figure7_ComunaExclude.gph" , replace
graph export "$path/_out/$S_DATE/Graphs/Figure7_ComunaExclude.pdf" , replace
   
  

*Table 12 
	
	*Columns 1 and 2 - by sex
foreach i of numlist 0 1{
		
local female_`i' = "female_`i'"
eststo `female_`i'': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata if female == `i', ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `female_`i''
estadd local period = "2004-2013" : `female_`i''

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `female_`i''
  }           
}  

	*Columns 3 and 4 - by mother's education
foreach i of numlist 0 1{
		
local mother_dropout_`i' = "mother_dropout_`i'"
eststo `mother_dropout_`i'': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata if mother_dropout == `i', ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata i.mother_dropout i.missing_mother)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `mother_dropout_`i''
estadd local period = "2004-2013" : `mother_dropout_`i''

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `mother_dropout_`i''
  }           
}  


	*Columns 5 and 6 - by strata
foreach i of numlist 0 1{
		
local high_strata_`i' = "high_strata_`i'"
eststo `high_strata_`i'': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata if high_strata == `i', ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata i.mother_dropout i.missing_mother)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `high_strata_`i''
estadd local period = "2004-2013" : `high_strata_`i''

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `high_strata_`i''
  }           
}  


	*Columns 7 and 8 - by income
foreach i of numlist 0 1{
		
local high_income_`i' = "high_income_`i'"
eststo `high_income_`i'': ivreghdfe log_math (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata if high_income == `i' & ///
		inrange(YEAR , 2008 , 2013), ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata i.mother_dropout i.missing_mother)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `high_income_`i''
estadd local period = "2008-2013" : `high_income_`i''

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `high_income_`i''
  }           
}  

esttab  `female_0' `female_1' `mother_dropout_0' `mother_dropout_1' ///
	`high_strata_0' `high_strata_1' `high_income_0' `high_income_1' using ///
	"$path/_out/$S_DATE/Table12.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Math \\ Test Scores \\ Males}" ///
		"\specialcell{Log of Math \\ Test Scores \\ Females}" ///
		"\specialcell{Log of Math \\ Test Scores \\ Mother Secondary Ed. \\ or Higher}" ///
		"\specialcell{Log of Math \\ Test Scores \\ Mother Elem. Ed. \\ or Less}" ///
		"\specialcell{Log of Math \\ Test Scores \\ Low Strata}" ///
		"\specialcell{Log of Math \\ Test Scores \\ High Strata}" ///
		"\specialcell{Log of Math \\ Test Scores \\ Family Income < \\ 2 Min. Wages}" ///
		"\specialcell{Log of Math \\ Test Scores \\ Family Income \geq \\ 2 Min. Wages}") ///
	keep(Hom_250_yr1)  ///
	order(Hom_250_yr1) ///
	stats(obs F1 period,  ///
		labels("Observations" "First-Stage F" "Period"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include school and year fixed effects, and controls defined in Table 1. Missing values for mother's education and strata are replaced with indicators to capture missingness, except in Columns (3)-(4) and (5)-(6), respectively, where observations with missing values are dropped. Regressions estimated with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. Post-Extradition period is 2009-2013.")    
  
  
*Figure 8, Panel A - Effect by time band
matrix drop _all
preserve

foreach i of numlist 1/12{

replace Homicide_m`i'_250m = 0 if Homicide_m`i'_250m == .	// missings are zeros

if `i' > 1{
	local n = `i' - 1 // we want to widen the time window with each loop
	replace Homicide_m`i'_250m = Homicide_m`i'_250m + Homicide_m`n'_250m
}
	
ivreghdfe log_math (Homicide_m`i'_250m = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)
	
*coef matrix saves coefficient estimates
matrix coef = nullmat(coef) \ _b[Homicide_m`i'_250m]
*ci matrices save upper and lower bounds of 95 % confidence interval
boottest Homicide_m`i'_250m , /// wild bootstrapped p-vals w/ confidence interval
	cluster(comuna) nograph seed(123023) 
matrix CI_lower = nullmat(CI_lower) \ r(CI)[1,1]  // store confidence interval
matrix CI_upper = nullmat(CI_upper) \ r(CI)[1,2] 	
}

matrix full = coef , CI_upper , CI_lower

clear
svmat full

gen lag = (_n)

twoway (connect full1 lag, sort scheme(s1mono) mcolor(black) ///
		ylabel( , labsize(medium) glcolor(gs15) glstyle(solid) angle(0) notick /// 
			labcolor(black)) ///
		yline(0, lwidth(vvvthin)) ///
		title("Panel A: Effect of Homicides as Time Window Increases" , ///
			size(medium)) ///
		xlabel(1(1)12) ///
		xtitle("Months Before Exam") ///
		ytitle("Estimated Effect on Log Math Scores") ///		
		legend(off)) ///
	   (line full2 lag, sort lcolor(gs6) lpattern(dash)) ///
	   (line full3 lag, sort lcolor(gs6) lpattern(dash)) 
graph save "temp1" , replace

restore   

  
*Figure 8, Panel B - Effect by month of exposure
matrix drop _all
preserve

foreach i of numlist 1/12{

replace Homicide_m`i'_250m = 0 if Homicide_m`i'_250m == .	// missings are zeros

ivreghdfe log_math (Homicide_m`i'_250m = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)
	
*coef matrix saves coefficient estimates
matrix coef = nullmat(coef) , _b[Homicide_m`i'_250m]

*ci matrices save upper and lower bounds of 95 % confidence interval
boottest Homicide_m`i'_250m , /// wild bootstrapped p-vals w/ confidence interval
	cluster(comuna) nograph seed(123023) 
matrix CI = nullmat(CI) \ r(CI)  // store confidence interval
	
}
matrix CI[6,1] = .
mat colnames coef = 1 2 3 4 5 6 7 8 9 10 11 12
matrix CI = CI'
coefplot mat(coef), ci(CI) vertical yline(0 , lstyle(solid)) ///
	title("Panel B: Effect of Homicides Ocurring in Different Months Before Exam" , ///
			size(medium)) ///	
	xtitle("Month Before Exam When Homicide Ocurred" , color(black) size(medium)) ///
	ytitle("Effect of Homicides on Log Math Scores" , color(black) size(medium)) ///
	graphregion(color(white)) ///
	ylabel( , labsize(medium) glcolor(gs15) glstyle(solid) angle(0) notick /// 
		labcolor(black)) ///
	mcolor(black) ciopts(recast(rcap) lcolor(black))  
graph save "temp2" , replace

restore   
 
graph combine "temp1" "temp2" , graphregion(color(white)) iscale(*.75) 
graph save "$path/_out/$S_DATE/Graphs/Figure8.gph" , replace  
  
erase "temp1.gph" 
erase "temp2.gph"  

  
*Table A.1 - other subject areas
cap gen log_spanish = log(LENGUAJE_PUNT) 
cap gen log_social_science = log(CIENCIAS_SOCIALES_PUNT) 
cap gen log_philosophy = log(FILOSOFIA_PUNT) 
cap gen log_biology = log(BIOLOGIA_PUNT) 
cap gen log_chemistry = log(QUIMICA_PUNT) 
cap gen log_physics = log(FISICA_PUNT)
  
 	*Col 1 - Spanish
local rega = "rega"
eststo `rega': ivreghdfe log_spanish (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }   
  
su LENGUAJE_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `rega'
estadd local sdy = `sd'   : `rega' 
  
  
 	*Col 2 - Social Science
local regb = "regb"
eststo `regb': ivreghdfe log_social_science (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }     
  
su CIENCIAS_SOCIALES_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regb'
estadd local sdy = `sd'   : `regb'   
  
  
 	*Col 3 - Philosophy
local regc = "regc"
eststo `regc': ivreghdfe log_philosophy (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }       
  
su FILOSOFIA_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regc'
estadd local sdy = `sd'   : `regc'     
  
  
 	*Col 4 - Biology
local regd = "regd"
eststo `regd': ivreghdfe log_biology (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }        
  
su BIOLOGIA_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regd'
estadd local sdy = `sd'   : `regd'      
  
  
 	*Col 5 - Chemistry
local rege = "rege"
eststo `rege': ivreghdfe log_chemistry (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }        
    
su QUIMICA_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `rege'
estadd local sdy = `sd'   : `rege'  	
	
  
 	*Col 6 - Physics
local regf = "regf"
eststo `regf': ivreghdfe log_physics (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regf'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }         
  
su FISICA_PUNT if e(sample) // store DV mean and SD
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local mny = `mean' : `regf'
estadd local sdy = `sd'   : `regf'    
  
  
esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/TableA1.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Log of Language \\ Test Scores}" ///
		"\specialcell{Log of Social \\ Science Test Scores}" ///
		"\specialcell{Log of Philosophy \\ Test Scores}" ///
		"\specialcell{Log of Biology \\ Test Scores}" ///
		"\specialcell{Log of Chemistry \\ Test Scores}" ///
		"\specialcell{Log of Physics\\ Test Scores}") ///
	keep(Hom_250_yr1 1.mother_dropout)  ///
	order(Hom_250_yr1 1.mother_dropout) ///
	stats(obs F1 mny sdy ,  ///
		labels("Observations" "First-Stage F" "D.V. Mean (in levels)" ///
			"D.V. Standard Deviation (in levels)"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include controls defined in Table 1. Regressions estimated with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. Post-Extradition period is 2009-2013.")    
  
  
  
*Table A.2 - Indicator of Test Score Percentile
cap drop temp
cap drop math_pctile*
_pctile MATEMATICAS_PUNT, p(10, 25, 50, 75, 90 , 99.999) // calculate percentiles
egen temp = cut(MATEMATICAS_PUNT) , at(0 , `r(r1)' , `r(r2)' , `r(r3)' , ///
	`r(r4)' , `r(r5)' , `r(r6)') // group into percentile cuts
sum temp , detail
replace temp = `r(max)' if mi(temp) & !mi(MATEMATICAS_PUNT) // missings are highest cut
egen math_pctile = group(temp) // appropriately coded and labelled cuts
label define math_pctile 1 "Below 10th percentile" ///
	2 "Between 10th and 25th percentile" 3 "Between 25th and 50th percentile" ///
	4 "Between 50th and 75th percentile" 5 "Between 75th and 90th percentile" ///
	6 "Between 90th and 100th percentile" , replace
label val math_pctile math_pctile
tab math_pctile , gen(math_pctile_)
  
	*Loop through the six columns
foreach i of numlist 1/6{

local lab : var label math_pctile_`i'
local lab = subinstr("`lab'" , "math_pctile==" , "" , .)
label var math_pctile_`i' "`lab'"

local math_pctile_`i' = "math_pctile_`i'"
eststo `math_pctile_`i'': ivreghdfe math_pctile_`i' (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)

estadd local obs=e(N)    // store additional info for table	

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `math_pctile_`i''
  }           
}  

esttab  `math_pctile_1' `math_pctile_2' `math_pctile_3' `math_pctile_4' ///
	`math_pctile_5' `math_pctile_6' using ///
	"$path/_out/$S_DATE/TableA2.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Below \\ 10th Percentile}" ///
		"\specialcell{Between 10th and \\ 25th Percentiles}" ///
		"\specialcell{Between 25th and \\ 50th Percentiles}" ///
		"\specialcell{Between 50th and \\ 75th Percentiles}" ///
		"\specialcell{Between 75th and \\ 90th Percentiles}" ///
		"\specialcell{Above \\ 90th Percentile}") ///
	keep(Hom_250_yr1 1.mother_dropout)  ///
	order(Hom_250_yr1 1.mother_dropout) ///
	stats(obs ,  ///
		labels("Observations"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include controls defined in Table 1. Regressions estimated with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. Post-Extradition period is 2009-2013.")    
  
  
  
  
*Figure A.1 - Panel A, by distance thresholds
matrix drop _all
foreach dist of numlist 100 250 500 750 1000 1250 1500 1750 2000 2250 2500 2750{
ivreghdfe log_math (Hom_`dist'_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)
		
*coef matrix saves coefficient estimates
matrix coef = nullmat(coef) \ _b[Hom_`dist'_yr1]
*ci matrices save upper and lower bounds of 95 % confidence interval
boottest Hom_`dist'_yr1 , /// wild bootstrapped p-vals w/ confidence interval
	cluster(comuna) nograph seed(123023) 
matrix CI_lower = nullmat(CI_lower) \ r(CI)[1,1]  // store confidence interval
matrix CI_upper = nullmat(CI_upper) \ r(CI)[1,2] 	
}

matrix full = coef , CI_upper , CI_lower

preserve
clear
svmat full

gen dist = 250 * (_n - 1)
replace dist = 100 if dist == 0

twoway (connect full1 dist, sort scheme(s1mono) mcolor(black) ///
	ylabel( , labsize(medium) angle(0) /// 
	labcolor(black)) ///	
	yline(0, lwidth(vvvthin)) xlabel(#8) xtick(#8) xlabel(, valuelabel)  ///
		title("Panel A: Estimated Coefficient for Different Distance Thresholds" , ///
			size(meduim)) xtitle("Distance Threshold (meters)") ///
		ytitle("Estimated Effect on Log Math Scores") ///
		legend(off)) ///
	   (line full2 dist, sort lcolor(gs6) lpattern(dash)) ///
	   (line full3 dist, sort lcolor(gs6) lpattern(dash)) 
graph save "temp1" , replace
restore
 

*Figure A.1 - Panel B, by distance threshold rings
matrix drop _all
preserve
	*Get rings
replace Hom_2000_yr1 = Hom_2000_yr1 - Hom_1750_yr1	
replace Hom_1750_yr1 = Hom_1750_yr1 - Hom_1500_yr1		
replace Hom_1500_yr1 = Hom_1500_yr1 - Hom_1250_yr1	
replace Hom_1250_yr1 = Hom_1250_yr1 - Hom_1000_yr1
replace Hom_1000_yr1 = Hom_1000_yr1 - Hom_750_yr1
replace Hom_750_yr1 = Hom_750_yr1 - Hom_500_yr1
replace Hom_500_yr1 = Hom_500_yr1 - Hom_250_yr1

foreach dist of numlist 250 500 750 1000 1250 /*1500 1750 2000*/{
ivreghdfe log_math (Hom_`dist'_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)
		
*coef matrix saves coefficient estimates
matrix coef = nullmat(coef) \ _b[Hom_`dist'_yr1]
*ci matrices save upper and lower bounds of 95 % confidence interval
boottest Hom_`dist'_yr1 , /// wild bootstrapped p-vals w/ confidence interval
	cluster(comuna) nograph seed(123023) 
matrix CI_lower = nullmat(CI_lower) \ r(CI)[1,1]  // store confidence interval
matrix CI_upper = nullmat(CI_upper) \ r(CI)[1,2] 	
}

matrix full = coef , CI_upper , CI_lower

clear
svmat full

gen dist = 250 * (_n)
replace dist = 100 if dist == 0

twoway (connect full1 dist, sort scheme(s1mono) mcolor(black) ///
	ylabel(0(0.01)-0.03 , labsize(medium) angle(0) /// 
	labcolor(black))  ///
	yline(0, lwidth(vvvthin)) ///
	xlabel(250 "100-250" 500 "250-500" 750 "500-750" ///
	1000 "750-1000" 1250 "1000-1250")   ///
	title("Panel B: Estimated Coefficient for Different Distance Rings" , ///
		size(medium)) ///
	xtitle("Distance Ring (meters)") ///
	ytitle("Estimated Effect on Log Math Scores") ///
	legend(off)) ///
	(line full2 dist, sort lcolor(gs6) lpattern(dash)) ///
	(line full3 dist, sort lcolor(gs6) lpattern(dash)) 
graph save "temp2" , replace

restore  

graph combine "temp1" "temp2" , graphregion(color(white)) iscale(*.75) 
graph save "$path/_out/$S_DATE/Graphs/FigureA1.gph" , replace  
  
erase "temp1.gph" 
erase "temp2.gph"  
  
  
*Figure A.2, Panel A - by level of exposure within 250
matrix drop _all
preserve

foreach i of numlist 3/13{
	
gen Homicides_`i' = Hom_250_yr1 >= `i'

	
ivreghdfe log_math (Homicides_`i' = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)
		
		
*coef matrix saves coefficient estimates
matrix coef = nullmat(coef) \ _b[Homicides_`i']
*ci matrices save upper and lower bounds of 95 % confidence interval
boottest Homicides_`i' , /// wild bootstrapped p-vals w/ confidence interval
	cluster(comuna) nograph seed(123023) 
matrix CI_lower = nullmat(CI_lower) \ r(CI)[1,1]  // store confidence interval
matrix CI_upper = nullmat(CI_upper) \ r(CI)[1,2] 	
}

matrix full = coef , CI_upper , CI_lower

clear
svmat full

gen lag = (_n - 1) + 3


twoway (line full1 lag, sort scheme(s1mono) mcolor(black) ///
	ylabel( , labsize(medium) angle(0) /// 
	labcolor(black)) ///	
	yline(0, lwidth(vvvthin))     ///
	xlabel(3(2)13 , labsize(medium)) ///
		title("Panel A: Homicide Exposure Within 250m of School" , ///
			size(medium)) ///
		xtitle("Homicides in 12-Month Period Before Exam") ///
		ytitle("Estimated Effect on Log Math Scores") ///
		legend(off)) ///
	   (line full2 lag, sort lcolor(gs6) lpattern(dash)) ///
	   (line full3 lag, sort lcolor(gs6) lpattern(dash)) 
graph save "temp1" , replace

restore   

  
*Figure A.2, Panel B - by level of exposure within 500m
matrix drop _all
preserve

foreach i of numlist 13/36{
	
gen Homicides_`i' = Hom_500_yr1 >= `i'

	
ivreghdfe log_math (Homicides_`i' = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment /// run model 
	pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
	female i.ESTU_EDAD i.shift i.mother_dropout i.missing_mother ///
	i.ESTU_ESTRATO i.missing_strata , ///
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		pct_female log_number_students log_vis_unidades_school settlement_extent_school ///
		female i.ESTU_EDAD i.shift i.missing_mother ///
		i.ESTU_ESTRATO i.missing_strata)
		
		
*coef matrix saves coefficient estimates
matrix coef = nullmat(coef) \ _b[Homicides_`i']
*ci matrices save upper and lower bounds of 95 % confidence interval
boottest Homicides_`i' , /// wild bootstrapped p-vals w/ confidence interval
	cluster(comuna) nograph seed(123023) 
matrix CI_lower = nullmat(CI_lower) \ r(CI)[1,1]  // store confidence interval
matrix CI_upper = nullmat(CI_upper) \ r(CI)[1,2] 	
}

matrix full = coef , CI_upper , CI_lower

clear
svmat full

gen lag = (_n - 1) + 13

twoway (line full1 lag, sort scheme(s1mono) mcolor(black) ///
	ylabel( , labsize(medium) angle(0) /// 
	labcolor(black)) ///	
	yline(0, lwidth(vvvthin))     ///
		title("Panel B: Homicide Exposure Within 500m of School" , ///
			size(medium)) ///
		xtitle("Homicides in 12-Month Period Before Exam") ///
		ytitle("Estimated Effect on Log Math Scores") ///
		legend(off)) ///
	   (line full2 lag, sort lcolor(gs6) lpattern(dash)) ///
	   (line full3 lag, sort lcolor(gs6) lpattern(dash)) 
graph save "temp2" , replace

restore   


graph combine "temp1" "temp2" , graphregion(color(white)) iscale(*.75) 
graph save "$path/_out/$S_DATE/Graphs/FigureA2.gph" , replace  
  
erase "temp1.gph"
erase "temp2.gph"  
  

*Table 13 
use "$path/_dta/Dropout.dta" , clear

drop turnover transfer // we'll recalculate to ensure it's defined correctly

*first drop problematic obs. these have multiple obs per year, and not clear what order they attended the schools, so makes it impossible to accurately calculate transfer and dropout. It's about 2% of students or around 3% of all observations.
bys idglobal20150903 YEAR : gen temp = _N // count observations per student per year
bys idglobal20150903 : egen problem = max(temp) // get the max observations per student per year for each student
drop if problem > 1 // we want to drop all observations of these students
drop temp prob

*generate dropout variable for students who stop attending Medellin public schools	
bys idglobal20150903 (YEAR): gen dropout = 100 if _n == _N & YEAR != 2014 & ///
	grado != 11	 
bys idglobal20150903 (YEAR): replace dropout = 0 if _n != _N & YEAR != 2014	& ///
	grado != 11
 
*generate transfer variable
bys idglobal20150903 (YEAR): gen transfer = 100 * (DANE_sede[_n] != DANE_sede[_n+1]) ///
	if (idglobal20150903[_n] == idglobal20150903[_n+1]) & YEAR != 2014 & grado != 11
bys idglobal20150903 (YEAR): replace transfer = 0 if _n == _N & YEAR != 2014 & ///
	grado != 11

*generate a turnover variable for students who either dropped out or transferred
/*bys idglobal20150903 DANE_sede (YEAR): /// 
	gen turnover = 100 if _n == _N & YEAR != 2014 & grado != 11	
bys idglobal20150903 DANE_sede (YEAR): ///
	replace turnover = 0 if _n != _N & YEAR != 2014 & grado != 11*/
gen turnover = 100 * (dropout == 100 | transfer == 100) if YEAR != 2014 & grado != 11	

*Repeater	
bys idglobal20150903 (YEAR): gen repeater = 100 * (grado[_n] == grado[_n+1]) if ///
	(idglobal20150903[_n] == idglobal20150903[_n+1]) & YEAR != 2014 & ///
	grado != 11
bys idglobal20150903 (YEAR): replace repeater = 0 if _n == _N & YEAR != 2014 & ///
	grado != 11
	
keep if inrange(edad2 , 14 , 18) & inlist(grado , 9 , 10) // main analysis for grade 11 test scores uses only 16-19 year olds, so for dropout analysis with 9th and 10th graders we'll look at 14-18 year olds (i.e., two years younger on the bottom end and one year younger on the top end)


*small number of observations where strata is 9
label define estrato 9 "Invalid" , modify

gen berna_post_2008 = (berna_comuna == 1 & YEAR >= 2009)
label var berna_post_2008 "Berna Comuna X Post-Extradition Period"
gen log_public_investment = log(public_investment) , after(public_investment)
gen log_population = log(population) , after(population)
bys comuna YEAR DANE_sede : gen temp = 1 if _n ==1 // sum number of schools by comuna
bys comuna YEAR : egen number_schools = total(temp)
drop temp
gen log_number_schools = log(number_schools) , after(number_schools) // take logs
gen log_vis_unidades_school = log(vis_unidades_school + 1)  
bys DANE_sede YEAR grado: egen pct_female = mean(genero==0) // calculate percent female students by school
replace pct_female = pct_female * 100
bys DANE_sede YEAR grado : gen number_students = _N // sum number of students by school-grade
gen log_number_students = log(number_students)
gen high_strata = inrange(estrato , 3 , 6) if !mi(estrato)

*students missing female
replace female = 2 if female == .
label define female 0 "Male" 1 "Female" 2 "Missing"
label val female female

*students missing shift
replace tipo_jorn = 4 if tipo_jorn == .
label define  etiqjornada 4 "Missing" , modify

tab edad2 , gen(age_) 
tab tipo_jorn , gen(shift_) 
tab estrato , gen(strata_)

	* Column 1 - all students
preserve

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata_* , by(DANE_sede YEAR grado)
	
local rega = "rega"
eststo `rega': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rega'
estadd local hom_sd = `sd' : `rega'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `rega'
estadd local dropout_sd = `sd' : `rega'  
  
restore 


	* Column 2 - female students
preserve

keep if genero == 0

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regb = "regb"
eststo `regb': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regb'
estadd local hom_sd = `sd' : `regb'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regb'
estadd local dropout_sd = `sd' : `regb'  
  
restore  


	* Column 3 - male students
preserve

keep if genero == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regc = "regc"
eststo `regc': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regc'
estadd local hom_sd = `sd' : `regc'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regc'
estadd local dropout_sd = `sd' : `regc'  
  
restore  


	* Column 4 - low strata
preserve

keep if high_strata == 0

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regd = "regd"
eststo `regd': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regd'
estadd local hom_sd = `sd' : `regd'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regd'
estadd local dropout_sd = `sd' : `regd'  
  
restore 


	* Column 5 - high strata
preserve

keep if high_strata == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local rege = "rege"
eststo `rege': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rege'
estadd local hom_sd = `sd' : `rege'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `rege'
estadd local dropout_sd = `sd' : `rege'  
  
restore 
 
 
	* Column 6 - low strata, males
preserve

keep if high_strata == 0 & genero2 == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regf = "regf"
eststo `regf': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regf'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regf'
estadd local hom_sd = `sd' : `regf'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regf'
estadd local dropout_sd = `sd' : `regf'  
  
restore  
 
 
esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/Table13_turnover.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Turnover Rate \\ All Students}" ///
		"\specialcell{Turnover Rate \\ Female Students}" ///
		"\specialcell{Turnover Rate \\ Male Students}" ///
		"\specialcell{Turnover Rate \\ Low Strata}" ///
		"\specialcell{Turnover Rate \\ High Strata}" ///
		"\specialcell{Turnover Rate \\ Low Strata and Male}") ///
	keep(Hom_250_yr1)  ///
	order(Hom_250_yr1) ///
	stats(obs F1 dropout_mean dropout_sd hom_mean hom_sd ,  ///
		labels("Observations" "First-Stage F" "D.V. Mean" "D.V. Standard Deviation" ///
		"Homicides Mean" "Homicides Standard Deviation"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include school and year fixed effects. All regressions control for comuna-level variables from Table ~\ref{tab:Table1}. Other controls vary at the school-year level: number of students, percentage of students who are female, housing units of social interest and human build settlements. Indicator variables for the grade are also included. All regressions estimated at the school-grade level with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. Post-Extradition period is 2009-2013. Dependent variable is measured in percentages. Low strata defined as students from socioeconomic strata 1 and 2; High strata as students from strata 3 and higher. All regressions correspond to 2005-2013 period of observation (2014 is censored and not included because we do not have data for 2015 and later to calculate rates for 2014).")    
   
 

*Same, but for dropout	 
	* Column 1 - all students
preserve

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata_* , by(DANE_sede YEAR grado)
	
local rega = "rega"
eststo `rega': ivreghdfe dropout (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rega'
estadd local hom_sd = `sd' : `rega'
sum dropout if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `rega'
estadd local dropout_sd = `sd' : `rega'  
  
restore 


	* Column 2 - female students
preserve

keep if genero == 0

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regb = "regb"
eststo `regb': ivreghdfe dropout (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regb'
estadd local hom_sd = `sd' : `regb'
sum dropout if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regb'
estadd local dropout_sd = `sd' : `regb'  
  
restore  


	* Column 3 - male students
preserve

keep if genero == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regc = "regc"
eststo `regc': ivreghdfe dropout (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regc'
estadd local hom_sd = `sd' : `regc'
sum dropout if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regc'
estadd local dropout_sd = `sd' : `regc'  
  
restore  


	* Column 4 - low strata
preserve

keep if high_strata == 0

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regd = "regd"
eststo `regd': ivreghdfe dropout (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regd'
estadd local hom_sd = `sd' : `regd'
sum dropout if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regd'
estadd local dropout_sd = `sd' : `regd'  
  
restore 


	* Column 5 - high strata
preserve

keep if high_strata == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local rege = "rege"
eststo `rege': ivreghdfe dropout (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rege'
estadd local hom_sd = `sd' : `rege'
sum dropout if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `rege'
estadd local dropout_sd = `sd' : `rege'  
  
restore 
 
 
	* Column 6 - low strata, males
preserve

keep if high_strata == 0 & genero2 == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regf = "regf"
eststo `regf': ivreghdfe dropout (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regf'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regf'
estadd local hom_sd = `sd' : `regf'
sum dropout if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regf'
estadd local dropout_sd = `sd' : `regf'  
  
restore  
 
 
esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/Table13_Dropout.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Dropout Rate \\ All Students}" ///
		"\specialcell{Dropout Rate \\ Female Students}" ///
		"\specialcell{Dropout Rate \\ Male Students}" ///
		"\specialcell{Dropout Rate \\ Low Strata}" ///
		"\specialcell{Dropout Rate \\ High Strata}" ///
		"\specialcell{Dropout Rate \\ Low Strata and Male}") ///		
	keep(Hom_250_yr1)  ///
	order(Hom_250_yr1) ///
	stats(obs F1 dropout_mean dropout_sd hom_mean hom_sd ,  ///
		labels("Observations" "First-Stage F" "D.V. Mean" "D.V. Standard Deviation" ///
		"Homicides Mean" "Homicides Standard Deviation"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include school and year fixed effects. All regressions control for comuna-level variables from Table ~\ref{tab:Table1}. Other controls vary at the school-year level: number of students, percentage of students who are female, housing units of social interest and human build settlements. Indicator variables for the grade are also included. All regressions estimated at the school-grade level with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. Post-Extradition period is 2009-2013. Dependent variable is measured in percentages. Low strata defined as students from socioeconomic strata 1 and 2; High strata as students from strata 3 and higher. All regressions correspond to 2005-2013 period of observation (2014 is censored and not included because we do not have data for 2015 and later to calculate rates for 2014). Dropout in year $t$ defined as not enrolling in any Medellin public school in year $t+1$.") 
	
	
	

*Same, but for transfer	
	* Column 1 - all students
preserve

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata_* , by(DANE_sede YEAR grado)
	
local rega = "rega"
eststo `rega': ivreghdfe transfer (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rega'
estadd local hom_sd = `sd' : `rega'
sum transfer if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `rega'
estadd local dropout_sd = `sd' : `rega'  
  
restore 


	* Column 2 - female students
preserve

keep if genero == 0

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regb = "regb"
eststo `regb': ivreghdfe transfer (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regb'
estadd local hom_sd = `sd' : `regb'
sum transfer if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regb'
estadd local dropout_sd = `sd' : `regb'  
  
restore  


	* Column 3 - male students
preserve

keep if genero == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regc = "regc"
eststo `regc': ivreghdfe transfer (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regc'
estadd local hom_sd = `sd' : `regc'
sum transfer if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regc'
estadd local dropout_sd = `sd' : `regc'  
  
restore  


	* Column 4 - low strata
preserve

keep if high_strata == 0

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regd = "regd"
eststo `regd': ivreghdfe transfer (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regd'
estadd local hom_sd = `sd' : `regd'
sum transfer if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regd'
estadd local dropout_sd = `sd' : `regd'  
  
restore 


	* Column 5 - high strata
preserve

keep if high_strata == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local rege = "rege"
eststo `rege': ivreghdfe transfer (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rege'
estadd local hom_sd = `sd' : `rege'
sum transfer if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `rege'
estadd local dropout_sd = `sd' : `rege'  
  
restore 
 
 
	* Column 6 - low strata, males
preserve

keep if high_strata == 0 & genero2 == 1

collapse turnover dropout transfer comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regf = "regf"
eststo `regf': ivreghdfe transfer (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regf'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regf'
estadd local hom_sd = `sd' : `regf'
sum transfer if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regf'
estadd local dropout_sd = `sd' : `regf'  
  
restore  
 
 
esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/Table13_Transfer.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Transfer Rate \\ All Students}" ///
		"\specialcell{Transfer Rate \\ Female Students}" ///
		"\specialcell{Transfer Rate \\ Male Students}" ///
		"\specialcell{Transfer Rate \\ Low Strata}" ///
		"\specialcell{Transfer Rate \\ High Strata}" ///
		"\specialcell{Transfer Rate \\ Low Strata and Male}") ///		
	keep(Hom_250_yr1)  ///
	order(Hom_250_yr1) ///
	stats(obs F1 dropout_mean dropout_sd hom_mean hom_sd ,  ///
		labels("Observations" "First-Stage F" "D.V. Mean" "D.V. Standard Deviation" ///
		"Homicides Mean" "Homicides Standard Deviation"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include school and year fixed effects. All regressions control for comuna-level variables from Table ~\ref{tab:Table1}. Other controls vary at the school-year level: number of students, percentage of students who are female, housing units of social interest and human build settlements. Indicator variables for the grade are also included. All regressions estimated at the school-grade level with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. Post-Extradition period is 2009-2013. Dependent variable is measured in percentages. Low strata defined as students from socioeconomic strata 1 and 2; High strata as students from strata 3 and higher. All regressions correspond to 2005-2013 period of observation (2014 is censored and not included because we do not have data for 2015 and later to calculate rates for 2014). Transfer in year $t$ defined as enrolling in a different Medellin public school in year $t+1$.")  

	
*Same, but for repeaters
	* Column 1 - all students
preserve

collapse turnover dropout transfer repeater comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata_* , by(DANE_sede YEAR grado)
	
local rega = "rega"
eststo `rega': ivreghdfe repeater (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rega'
estadd local hom_sd = `sd' : `rega'
sum repeater if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `rega'
estadd local dropout_sd = `sd' : `rega'  
  
restore 


	* Column 2 - female students
preserve

keep if genero == 0

collapse turnover dropout transfer repeater comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regb = "regb"
eststo `regb': ivreghdfe repeater (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regb'
estadd local hom_sd = `sd' : `regb'
sum repeater if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regb'
estadd local dropout_sd = `sd' : `regb'  
  
restore  


	* Column 3 - male students
preserve

keep if genero == 1

collapse turnover dropout transfer repeater comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regc = "regc"
eststo `regc': ivreghdfe repeater (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regc'
estadd local hom_sd = `sd' : `regc'
sum repeater if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regc'
estadd local dropout_sd = `sd' : `regc'  
  
restore  


	* Column 4 - low strata
preserve

keep if high_strata == 0

collapse turnover dropout transfer repeater comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regd = "regd"
eststo `regd': ivreghdfe repeater (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regd'
estadd local hom_sd = `sd' : `regd'
sum repeater if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regd'
estadd local dropout_sd = `sd' : `regd'  
  
restore 


	* Column 5 - high strata
preserve

keep if high_strata == 1

collapse turnover dropout transfer repeater comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local rege = "rege"
eststo `rege': ivreghdfe repeater (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rege'
estadd local hom_sd = `sd' : `rege'
sum repeater if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `rege'
estadd local dropout_sd = `sd' : `rege'  
  
restore 
 
 
	* Column 6 - low strata, males
preserve

keep if high_strata == 0 & genero2 == 1

collapse turnover dropout transfer repeater comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_students ///
	age_* shift_* strata* , by(DANE_sede YEAR grado)
	
local regf = "regf"
eststo `regf': ivreghdfe repeater (Hom_250_yr1 = berna_post_2008 ) i.YEAR /// run model
	log_number_schools log_population log_public_investment /// comuna-level controls
	log_vis_unidades_school settlement_extent_school pct_female /// school-level 
	log_number_students i.grado , ///  
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_students )
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regf'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }   
  
sum Hom if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regf'
estadd local hom_sd = `sd' : `regf'
sum repeater if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local dropout_mean = `mean' : `regf'
estadd local dropout_sd = `sd' : `regf'  
  
restore  
 
 
esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/Table13_Repeat.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Repeat Rate \\ All Students}" ///
		"\specialcell{Repeat Rate \\ Female Students}" ///
		"\specialcell{Repeat Rate \\ Male Students}" ///
		"\specialcell{Repeat Rate \\ Low Strata}" ///
		"\specialcell{Repeat Rate \\ High Strata}" ///
		"\specialcell{Repeat Rate \\ Low Strata and Male}") ///		
	keep(Hom_250_yr1)  ///
	order(Hom_250_yr1) ///
	stats(obs F1 dropout_mean dropout_sd hom_mean hom_sd ,  ///
		labels("Observations" "First-Stage F" "D.V. Mean" "D.V. Standard Deviation" ///
		"Homicides Mean" "Homicides Standard Deviation"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions include school and year fixed effects. All regressions control for comuna-level variables from Table ~\ref{tab:Table1}. Other controls vary at the school-year level: number of students, percentage of students who are female, housing units of social interest and human build settlements. Indicator variables for the grade are also included. All regressions estimated at the school-grade level with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. Post-Extradition period is 2009-2013. Dependent variable is measured in percentages. Low strata defined as students from socioeconomic strata 1 and 2; High strata as students from strata 3 and higher. All regressions correspond to 2005-2013 period of observation (2014 is censored and not included because we do not have data for 2015 and later to calculate rates for 2014). Repetition in year $t$ defined as enrolling in the same grade level in any Medellin public school in year $t+1$.") 	
 
 


*Table 14 

use "$path/_dta/Teachers.dta" , clear


gen berna_post_2008 = (berna_comuna == 1 & inrange(YEAR , 2009 , 2013))
label var berna_post_2008 "Berna Comuna X Post-Extradition Period"
gen log_public_investment = log(public_investment) , after(public_investment)
gen log_population = log(population) , after(population)
bys comuna YEAR DANE_sede : gen temp = 1 if _n ==1 // sum number of schools by comuna
bys comuna YEAR : egen number_schools = total(temp)
drop temp
gen log_number_schools = log(number_schools) , after(number_schools) // take logs
gen log_vis_unidades_school = log(vis_unidades_school + 1)
bys DANE_sede YEAR: egen pct_female = mean(mujer) // calculate percent female teachers by school
replace pct_female = pct_female * 100
bys DANE_sede YEAR : gen number_teachers = _N // sum number of teachers by school
gen log_number_teachers = log(number_teachers)

gen year_start=substr(fecha_vinculacion,-4,.)
destring year_start, replace
*Experience
gen experience = YEAR - year_start

*The "turnover" variable (in our case, the decision not to work at the same school next year)
*turnover=1 if the teacher does not return the following year; =0 if they return or if the variable is censored.
bys num_doc DANE_sede (YEAR): gen turnover=100 if _n==_N & YEAR!=2013	
bys num_doc DANE_sede (YEAR): replace turnover=0 if _n!=_N & YEAR!=2013	
	

	* Column 1 - all teachers
preserve

collapse turnover comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , by(DANE_sede YEAR)
	
local rega = "rega"
eststo `rega': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , /// run model 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_teachers)
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rega'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rega'
  }   
  
sum Hom_250_yr1 if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rega'
estadd local hom_sd = `sd' : `rega'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local turnover_mean = `mean' : `rega'
estadd local turnover_sd = `sd' : `rega'  
  
restore  
  
  
	* Column 2 - 12 years experience or less
preserve

keep if experience <= 12

collapse turnover comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , by(DANE_sede YEAR)
	
local regb = "regb"
eststo `regb': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , /// run model 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_teachers)
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regb'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regb'
  }   
  
sum Hom_250_yr1 if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regb'
estadd local hom_sd = `sd' : `regb'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local turnover_mean = `mean' : `regb'
estadd local turnover_sd = `sd' : `regb'  
  
restore   
 
 
 
	* Column 3 - more than 12 years experience
preserve

keep if experience > 12

collapse turnover comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , by(DANE_sede YEAR)
	
local regc = "regc"
eststo `regc': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , /// run model 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_teachers)
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regc'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regc'
  }   
  
sum Hom_250_yr1 if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regc'
estadd local hom_sd = `sd' : `regc'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local turnover_mean = `mean' : `regc'
estadd local turnover_sd = `sd' : `regc'  
  
restore    
 
 
 
	* Column 4 - secondary or two year technical degree
preserve

keep if inrange( nivel_educativo_aprobado , 0 , 5)

collapse turnover comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , by(DANE_sede YEAR)
	
local regd = "regd"
eststo `regd': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , /// run model 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_teachers)
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regd'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regd'
  }   
  
sum Hom_250_yr1 if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regd'
estadd local hom_sd = `sd' : `regd'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local turnover_mean = `mean' : `regd'
estadd local turnover_sd = `sd' : `regd'  
  
restore    
 
  
 
	* Column 5 - undergrad or post-grad degree
preserve

keep if inrange( nivel_educativo_aprobado , 6 , 9)

collapse turnover comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , by(DANE_sede YEAR)
	
local rege = "rege"
eststo `rege': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , /// run model 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_teachers)
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `rege'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `rege'
  }   
  
sum Hom_250_yr1 if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `rege'
estadd local hom_sd = `sd' : `rege'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local turnover_mean = `mean' : `rege'
estadd local turnover_sd = `sd' : `rege' 
  
restore     
 
 
	* Column 6 - undergrad or post-grad degree and more than 12 years experience
preserve

keep if inrange( nivel_educativo_aprobado , 6 , 9) & experience > 12

collapse turnover comuna berna_post_2008 Hom_250_yr1 ///
	log_public_investment log_number_schools log_population ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , by(DANE_sede YEAR)
	
local regf = "regf"
eststo `regf': ivreghdfe turnover (Hom_250_yr1 = berna_post_2008 ) i.YEAR ///
	log_number_schools log_population log_public_investment ///
	log_vis_unidades_school settlement_extent_school ///
	pct_female log_number_teachers , /// run model 
	noconstant absorb(DANE_sede) cluster(comuna) first ///
	partial(log_number_schools log_population log_public_investment /// 
		log_vis_unidades_school settlement_extent_school ///
		pct_female log_number_teachers)
	
estadd local obs=e(N)    // store additional info for table	
matrix first = e(first)
estadd local F1 = round(first[4,1]	, .01) : `regf'

local indep_vars: colnames e(b) // set up locals to report results and perform wild bootstrap
local n_vars: colsof e(b)
local indep_hypotheses
foreach var of local indep_vars {
   local indep_hypotheses="`indep_hypotheses'{`var'}"
}
boottest `indep_hypotheses' , /// wild bootstrapped p-vals for reg coefficients
	cluster(comuna) nograph seed(123023) 

matrix boot_pval = J(1,`n_vars',.) // store p-vals
forval k=1/`n_vars' {
matrix boot_pval[1,`k']	= r(p_`k')
	
}	
foreach mat in "boot_pval" {
   matrix colnames `mat' = `indep_vars'
   estadd matrix  `mat' = `mat' : `regf'
  }   
  
sum Hom_250_yr1 if e(sample) == 1
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)'
estadd local hom_mean = `mean' : `regf'
estadd local hom_sd = `sd' : `regf'
sum turnover if e(sample) == 1 
local mean : di %9.3f `r(mean)'
local sd : di %9.3f `r(sd)' 
estadd local turnover_mean = `mean' : `regf'
estadd local turnover_sd = `sd' : `regf'  
  
restore     
  
 
esttab  `rega' `regb' `regc' `regd' `rege' `regf' using ///
	"$path/_out/$S_DATE/Table14.tex" , ///
	replace unstack label nonotes booktabs varwidth(35) wrap ///
	cells(b(fmt(3)) boot_pval(fmt(3) par)) ///
	mtitles ("\specialcell{Turnover Rate \\ All Teahcers}" ///
		"\specialcell{Turnover Rate \\ Teachers With 0-12 \\ Years Experience}" ///
		"\specialcell{Turnover Rate \\ Teachers With More \\ Than 12 Years \\ Experience}" ///
		"\specialcell{Turnover Rate \\ Teachers With Less \\ Than Undergraduate \\ Degree}" ///
		"\specialcell{Turnover Rate \\ Teachers With \\ Undergraduate \\ or Post-Graduate \\ Degree }" ///
		"\specialcell{Turnover Rate \\ Teachers With More \\ Than 12 Years \\ Experience and \\ Undergraduate \\ or Post-Graduate \\ Degree }") ///
	keep(Hom_250_yr1)  ///
	order(Hom_250_yr1) ///
	stats(obs F1 turnover_mean turnover_sd hom_mean hom_sd ,  ///
		labels("Observations" "First-Stage F" "D.V. Mean" "D.V. Standard Deviation" ///
		"Homicides Mean" "Homicides Standard Deviation"))	///
	addnote("Note: Comuna-level wild bootstrapped p-values in parentheses. All regressions correspond to 2008-2012 period of observation (2013 is censored because we do not have data for 2014 and later). All regressions control for investment, population and the number of schools, variables that vary at the comuna-year level. Other controls vary at the school-year level: number of teachers, percentage of teachers who are women, and housing units of social interest and human built settlements within 500 meters around the school. Regressions estimated with 2SLS, with the interaction of Berna-controlled comuna and post-extradition period as an instrument for homicides. Post-Extradition period is 2009-2013.")    
   
  
  