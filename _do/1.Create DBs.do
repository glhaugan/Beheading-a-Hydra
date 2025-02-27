/*******************************************************************************
****		Effect of Don Berna's Extradition on Homicides in Medellin		****
****					Do-File for Creating Databases						****
***							haugan-gregory@norc.org							****
*******************************************************************************/

global path "H:/4. Personal/Violence_Education"

do "$path/_do/1. Database Preparation/1.Crime_Building_Students.do"
do "$path/_do/1. Database Preparation/2.1.Students_Building.do"
do "$path/_do/1. Database Preparation/2.2.Teachers_Building.do"
do "$path/_do/1. Database Preparation/2.3.Dropout_Building.do"
do "$path/_do/1. Database Preparation/3.ComunaLevel_Building.do"
do "$path/_do/1. Database Preparation/4.MonthlyHomicides_byComuna.do"
do "$path/_do/1. Database Preparation/5.VIS Building.do"
// Run "$path/_do/1. Database Preparation/6.ExtractSpatialCovars.do"

*Merge in Settlement Growth and Investment datasets
use "$path/_dta/RawData/SettlementGrowth/schools_settlementextent.dta" , clear
reshape long extent_set_ , i(COLE_CODIGO_COLEGIO) j(YEAR)
rename extent_set_ settlement_extent_school 
label var settlement_extent_school "Percentage of Developed Land Within 500m of School"
sort COLE_CODIGO_COLEGIO YEAR
keep COLE_CODIGO_COLEGIO YEAR settlement_extent_school
tempfile temp1
save "`temp1'" , replace

use "$path/_dta/RawData/SettlementGrowth/comunas_settlementextent.dta" , clear
reshape long extent_set_ , i(ID) j(YEAR)
rename (extent_set_ ID) (settlement_extent_comuna comuna) 
label var settlement_extent_comuna "Percentage of Developed Land in Comuna"
sort comuna YEAR
keep comuna YEAR settlement_extent_comuna
tempfile temp2
save "`temp2'" , replace

import excel using ///
	"$path/_dta/RawData/Public Investment/AnnualInvestmentPerComuna.xlsx" , ///
	clear firstrow
drop M
reshape long Year_ , i(Comuna) j(YEAR)
rename (Year_ Comuna) (public_investment comuna)
replace public_investment = public_investment * 1000
label var public_investment "Public Investment in Millions of Pesos"
sort comuna YEAR
tempfile temp3
save "`temp3'" , replace

import excel using ///
	"$path/_dta/RawData/HDI/HDI2004.xlsx" , ///
	clear firstrow
label var HDI2004 "Comuna Human Development Index in 2004"
sort comuna 
tempfile temp4
save "`temp4'" , replace


use "$path/_dta/ViolenceOnEducation.dta" , clear
sort COLE_CODIGO_COLEGIO YEAR
merge m:1 COLE_CODIGO_COLEGIO YEAR using "`temp1'", nogenerate keep(1 3) ///
	keepus(settlement_extent_school)
merge m:1 comuna YEAR using "`temp2'", nogenerate keep(1 3) ///
	keepus(settlement_extent_comuna)
merge m:1 comuna YEAR using "`temp3'", nogenerate keep(1 3) ///
	keepus(public_investment)
merge m:1 comuna using "`temp4'", nogenerate keep(1 3) 
order settlement_extent_school settlement_extent_comuna public_investment , ///
	after(vis_unidades_school)	
save "$path/_dta/ViolenceOnEducation.dta" , replace


use "$path/_dta/Teachers.dta" , clear
sort COLE_CODIGO_COLEGIO YEAR
merge m:1 COLE_CODIGO_COLEGIO YEAR using "`temp1'", nogenerate keep(1 3) ///
	keepus(settlement_extent_school)
merge m:1 comuna YEAR using "`temp2'", nogenerate keep(1 3) ///
	keepus(settlement_extent_comuna)
merge m:1 comuna YEAR using "`temp3'", nogenerate keep(1 3) ///
	keepus(public_investment)
merge m:1 comuna using "`temp4'", nogenerate keep(1 3) 	
order settlement_extent_school settlement_extent_comuna public_investment , ///
	after(vis_unidades_school)	
save "$path/_dta/Teachers.dta" , replace


use "$path/_dta/Dropout.dta" , clear
sort COLE_CODIGO_COLEGIO YEAR
merge m:1 COLE_CODIGO_COLEGIO YEAR using "`temp1'", nogenerate keep(1 3) ///
	keepus(settlement_extent_school)
merge m:1 comuna YEAR using "`temp2'", nogenerate keep(1 3) ///
	keepus(settlement_extent_comuna)
merge m:1 comuna YEAR using "`temp3'", nogenerate keep(1 3) ///
	keepus(public_investment)
merge m:1 comuna using "`temp4'", nogenerate keep(1 3) 	
order settlement_extent_school settlement_extent_comuna public_investment , ///
	after(vis_unidades_school)	
save "$path/_dta/Dropout.dta" , replace