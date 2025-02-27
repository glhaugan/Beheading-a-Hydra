********************************************************************************
***					Creating Files for Near Table Generation				 ***
********************************************************************************


global cd1 "H:/4. Personal/Violence_Education/_dta/RawData"
cap mkdir "$cd1"
cd "$cd1"
cap mkdir "$cd1/NearTables"
cap mkdir "$cd1/NearTables/ForTableGeneration"
set more off

use "$cd1/Medellin_crimes.dta", clear

**There are nine crime types. We will only use 7. Rename them to facilitate looping later on.
gen crime = "Manslaughter" if delito == "Hom Culposo (AT)"
replace crime = "Homicide" if delito == "Homicidio Comun"
replace crime = "Auto_Theft" if (delito == "Hur. Carro" | delito == "Hur. Moto")
replace crime = "Street_Muggings" if delito == "Hur. Personas"
replace crime = "Home_Burglaries" if delito == "Hur. Residencia"
replace crime = "Drug_Arrests" if delito == "Tra. o Fab. de Estupefacientes"

*Some formatting
replace fecha = substr(fecha,1,length(fecha)-7)
gen date = date(fecha, "DMY")
format date %td

*Data for generating near tables of homicides within 1-4 weeks, 1-24 months of the exam, and 1 month and 1 year leads
save "$cd1/motherfile" , replace


	*Export 2004 - October 2 Exam Date (Oct 2,2004 = 16346 in Stata's date format)
local delitos "Manslaughter Homicide Auto_Theft Street_Muggings Home_Burglaries Drug_Arrests"

	foreach i of numlist 0/6 { // for day of exam and each of 6 days prior
	keep if (16346 - (`i')) == date // keep only events in date range
	
		foreach d in "Homicide"{ // homicides only
		preserve
		keep if crime == "`d'" 
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2004_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}

	foreach i of numlist 1 2 3 4 { // for each week 1-4 weeks before exam
	keep if (16346 - (7*`i')) <= date & date < (16346 - (7*(`i'-1)))
	
		foreach d in "Homicide"{ // homicides only
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2004_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}

	forvalues i = 1/24 { // for each month 1/24 months before exam
	keep if (16346 - (30*`i')) <= date & date < (16346 - (30*(`i'-1)))
	
		foreach d of local delitos{ // for each crime
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2004_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	foreach i of numlist 1/24 { // for each month 1/24 months after exam
	keep if (16346 + (30*`i')) >= date & (date > (16346 + (30*(`i'-1))))
	
		foreach d in "Homicide"{ // homicides only
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2004_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}


	*Export 2005 - October 9 Exam Date (Oct 9,2005 = 16718 in Stata's date format)
	
	foreach i of numlist 0/6 {
	keep if (16718 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2005_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	foreach i of numlist 1 2 3 4 {
	keep if (16718 - (7*`i')) <= date & date < (16718 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2005_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}

	forvalues i = 1/24 {
	keep if (16718 - (30*`i')) <= date & date < (16718 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2005_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	foreach i of numlist 1/24 {
	keep if (16718 + (30*`i')) >= date & (date > (16718 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2005_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	*Export 2006 - Sept 24 Exam Date (Sept 24,2006 = 17068 in Stata's date format)
		
	foreach i of numlist 0/6 {
	keep if (17068 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2006_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}		
		
	foreach i of numlist 1 2 3 4 {
	keep if (17068 - (7*`i')) <= date & date < (17068 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2006_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
		
	forvalues i = 1/24 {
	keep if (17068 - (30*`i')) <= date & date < (17068 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2006_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}

	foreach i of numlist 1/24 {
	keep if (17068 + (30*`i')) >= date & (date > (17068 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2006_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
		
	*Export 2007 - Sept 23 Exam Date (Sept 23, 2007 = 17432 in Stata's date format)
		
	foreach i of numlist 0/6 {
	keep if (17432 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2007_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}			
		
	foreach i of numlist 1 2 3 4 {
	keep if (17432 - (7*`i')) <= date & date < (17432 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2007_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	forvalues i = 1/24 {
	keep if (17432 - (30*`i')) <= date & date < (17432 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2007_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}	

	foreach i of numlist 1/24 {
	keep if (17432 + (30*`i')) >= date & (date > (17432 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2007_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	*Export 2008 - Sept 21 Exam Date (Sept 21, 2008 = 17796 in Stata's date format)

	foreach i of numlist 0/6 {
	keep if (17796 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2008_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}				
			
	foreach i of numlist 1 2 3 4 {
	keep if (17796 - (7*`i')) <= date & date < (17796 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2008_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	forvalues i = 1/24 {
	keep if (17796 - (30*`i')) <= date & date < (17796 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2008_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}	

	foreach i of numlist 1/24 {
	keep if (17796 + (30*`i')) >= date & (date > (17796 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2008_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	
	*Export 2009 - Sept 13 Exam Date (Sept 13, 2009 = 18153 in Stata's date format)
		
	foreach i of numlist 0/6 {
	keep if (18153 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2009_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}			
		
	foreach i of numlist 1 2 3 4 {
	keep if (18153 - (7*`i')) <= date & date < (18153 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2009_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	forvalues i = 1/24 {
	keep if (18153 - (30*`i')) <= date & date < (18153 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2009_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}	

	foreach i of numlist 1/24 {
	keep if (18153 + (30*`i')) >= date & (date > (18153 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2009_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}

	
	*Export 2010 - Sept 12 Exam Date (Sept 12, 2010 = 18517 in Stata's date format)

	foreach i of numlist 0/6 {
	keep if (18517 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2010_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}				
			
	foreach i of numlist 1 2 3 4 {
	keep if (18517 - (7*`i')) <= date & date < (18517 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2010_wk`i'.dta", replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	forvalues i = 1/24 {
	keep if (18517 - (30*`i')) <= date & date < (18517 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2010_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}	

	foreach i of numlist 1/24 {
	keep if (18517 + (30*`i')) >= date & (date > (18517 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2010_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	*Export 2011 - Sept 4 Exam Date (Sept 4, 2011 = 18874 in Stata's date format)

	foreach i of numlist 0/6 {
	keep if (18874 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2011_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}				
			
	foreach i of numlist 1 2 3 4 {
	keep if (18874 - (7*`i')) <= date & date < (18874 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2011_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	forvalues i = 1/24 {
	keep if (18874 - (30*`i')) <= date & date < (18874 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2011_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}	

	foreach i of numlist 1/24 {
	keep if (18874 + (30*`i')) >= date & (date > (18874 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2011_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	*Export 2012 - Sept 2 Exam Date (Sept 2, 2012 = 19238 in Stata's date format)
		
	foreach i of numlist 0/6 {
	keep if (19238 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2012_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}			
		
	foreach i of numlist 1 2 3 4 {
	keep if (19238 - (7*`i')) <= date & date < (19238 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2012_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	forvalues i = 1/24 {
	keep if (19238 - (30*`i')) <= date & date < (19238 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2012_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}	

	foreach i of numlist 1/24 {
	keep if (19238 + (30*`i')) >= date & (date > (19238 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2012_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	*Export 2013 - Aug 25 Exam Date (Aug 25, 2013 = 19595 in Stata's date format)
			
	foreach i of numlist 0/6 {
	keep if (19595 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2013_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}					
			
	foreach i of numlist 1 2 3 4 {
	keep if (19595 - (7*`i')) <= date & date < (19595 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2013_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	forvalues i = 1/24 {
	keep if (19595 - (30*`i')) <= date & date < (19595 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2013_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}	

	foreach i of numlist 1/24 {
	keep if (19595 + (30*`i')) >= date & (date > (19595 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2013_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}


	*Export 2014 - Aug 3 Exam Date (Aug 3, 2014 = 19938 in Stata's date format)
			
	foreach i of numlist 0/6 {
	keep if (19938 - (`i')) == date
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2014_day`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}					
			
	foreach i of numlist 1 2 3 4 {
	keep if (19938 - (7*`i')) <= date & date < (19938 - (7*(`i'-1)))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2014_wk`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
	
	forvalues i = 1/24 {
	keep if (19938 - (30*`i')) <= date & date < (19938 - (30*(`i'-1)))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2014_m`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}	

	foreach i of numlist 1/24 {
	keep if (19938 + (30*`i')) >= date & (date > (19938 + (30*(`i'-1))))
	
		foreach d in "Homicide"{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		capture save "$cd1/NearTables/ForTableGeneration/`d'_2014_lead`i'.dta" , replace
		restore
		}
	use "$cd1/motherfile" , clear
	}
*

********************************************************************************
**********************		Build 	Near 	Tables		************************
********************************************************************************

*Complete list of schools
use "$cd1/NearTables/ForTableGeneration/Schools_20062.dta" , clear
append using "$cd1/NearTables/ForTableGeneration/Schools_20072.dta" ///
"$cd1/NearTables/ForTableGeneration/Schools_20082.dta" ///
"$cd1/NearTables/ForTableGeneration/Schools_20092.dta" ///
"$cd1/NearTables/ForTableGeneration/Schools_20102.dta" ///
"$cd1/NearTables/ForTableGeneration/Schools_20122.dta" ///
"$cd1/NearTables/ForTableGeneration/Schools_20132.dta" ///
"$cd1/NearTables/ForTableGeneration/Schools_20142.dta" , force

drop YEAR_SEMESTER
bys COLE_CODIGO_COLEGIO: keep if _n == 1 // keep one obs per school
save "$cd1/NearTables/ForTableGeneration/Schools_All.dta" , replace

foreach year of numlist 2004/2014{

local delitos "Manslaughter Homicide Auto_Theft Street_Muggings Home_Burglaries Drug_Arrests"

		foreach day of numlist 0/6{
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear
		*buffer of .5 means 1/2 km radius (500 meters)
		local buffer = .5
		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/Homicide_`year'_day`day'.dta",  ///
			n(delito_id y x ) within(`buffer') long near(0)
			
			capture confirm variable km_to_delito_id
			if _rc{
				gen Homicide_day`day' = 0
				}
			else{	
				gen Homicide_day`day' = 1
			}
			
		capture collapse (sum) Homicide_day`day' , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/Homicide_`year'_day`day'.dta" , replace
		}	

		foreach week of numlist 1 2 3 4{
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear
		*buffer of .5 means 1/2 km radius (500 meters)
		local buffer = .5
		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/Homicide_`year'_wk`week'.dta",  ///
			n(delito_id y x ) within(`buffer') long near(0)
			
			capture confirm variable km_to_delito_id
			if _rc{
				gen Homicide_wk`week' = 0
				}
			else{	
				gen Homicide_wk`week' = 1
			}
			
		capture collapse (sum) Homicide_wk`week' , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/Homicide_`year'_wk`week'.dta" , replace
		} 
		
		foreach forward of numlist 1/24{
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear
		*buffer of .5 means 3/4 km radius (500 meters)
		local buffer = .5
		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/Homicide_`year'_lead`forward'.dta",  ///
			n(delito_id y x ) within(`buffer') long near(0)
		
			capture confirm variable km_to_delito_id
			if _rc{
				gen Homicide_lead`forward' = 0
				}
			else{	
				gen Homicide_lead`forward' = 1
			}
	
		capture collapse (sum) Homicide_lead`forward' , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/Homicide_`year'_lead`forward'.dta" , replace
		}		
		
	foreach crime of local delitos{
				
		foreach month of numlist 1/24{
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear
		*buffer of .5 means 3/4 km radius (500 meters)
		local buffer = .5
		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/`crime'_`year'_m`month'.dta",  ///
			n(delito_id y x ) within(`buffer') long near(0)
		
			capture confirm variable km_to_delito_id
			if _rc{
				gen `crime'_m`month' = 0
				}
			else{	
				gen `crime'_m`month' = 1
			}
		
		capture collapse (sum) `crime'_m`month' , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/`crime'_`year'_m`month'.dta" , replace
		}
		
	if "`crime'" == "Homicide"{	
	
		use "$cd1/NearTables/`crime'_`year'_day0.dta" , clear
		foreach day of numlist 1/6{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_day`day'.dta" , gen(md`day')
		drop md`day'
		}	
	
		foreach week of numlist 1 2 3 4{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_wk`week'.dta" , gen(mwk`week')
		drop mwk`week'
		}	
	
		foreach forward of numlist 1/24{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_lead`forward'.dta" , gen(mlead`forward')
		drop mlead`forward'
		}
		
		foreach month of numlist 1/24{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_m`month'.dta" , gen(mm`month')
		drop mm`month'
		}
	}
	
	if "`crime'" != "Homicide"{
		foreach month of numlist 1/24{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_m`month'.dta" , gen(mm`month')
		drop mm`month'
		}
	}
	
	save "$cd1/NearTables/`crime'_`year'.dta" , replace
	}
	
	use "$cd1/NearTables/Manslaughter_`year'.dta" , clear
	local delitos "Homicide Auto_Theft Street_Muggings Home_Burglaries Drug_Arrests"
	foreach crime of local delitos{
	merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'.dta" , gen(m`crime'`year')
	drop m`crime'`year'
	}
	
	gen YEAR = `year'
	save "$cd1/NearTables/`year'.dta" , replace
}


** Bringing all years together

use "$cd1/NearTables/2004.dta" , clear 


append using "$cd1/NearTables/2005.dta" "$cd1/NearTables/2006.dta" 	///
	"$cd1/NearTables/2007.dta" ///
	"$cd1/NearTables/2008.dta" "$cd1/NearTables/2009.dta" ///
	"$cd1/NearTables/2010.dta" "$cd1/NearTables/2011.dta" ///
	"$cd1/NearTables/2012.dta" "$cd1/NearTables/2013.dta" ///
	"$cd1/NearTables/2014.dta" , force

drop delito_id km_to_delito_id

save "$cd1/NearTables/Violence_All" , replace



****************			Greater Distances				********************

clear
	*Create year-month tables at greater distances
	foreach year of numlist 2004/2014{
		foreach month of numlist 1/24{
	
		*Increasing distances by 250 meters each loop
		foreach dist of numlist 100 250 500 750{ 
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear
		
		local buffer = `dist'
		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/Homicide_`year'_m`month'.dta",  ///
			n(delito_id y x ) within(0.`buffer') long near(0)
		
			capture confirm variable km_to_delito_id
			if _rc{
				gen Homicide_m`month'_`dist' = 0
				}
			else{	
				gen Homicide_m`month'_`dist' = 1
			}
		
		capture collapse (sum) Homicide_m`month' , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/Homicide_`year'_m`month'_`dist'.dta" , replace
		}
		
		foreach dist of numlist 000 250 500 750{
			foreach i of numlist 1 2{
			use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear

			local buffer = `dist'
			capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/Homicide_`year'_m`month'.dta",  ///
			n(delito_id y x ) within(`i'.`buffer') long near(0)
		
			capture confirm variable km_to_delito_id
			if _rc{
				gen Homicide_m`month'_`i'`dist' = 0
				}
			else{	
				gen Homicide_m`month'_`i'`dist' = 1
			}
		
			capture collapse (sum) Homicide_m`month' , by(COLE_CODIGO_COLEGIO)
			save "$cd1/NearTables/Homicide_`year'_m`month'_`i'`dist'.dta" , replace
			}
		}
	}
}

*Merge into yearly distance databases
foreach year of numlist 2004/2014{
	foreach dist of numlist 100 250 500 750 10 1250 1500 1750 20 2250 2500 2750{
		use "$cd1/NearTables/Homicide_`year'_m1_`dist'.dta" , clear
		
		foreach month of numlist 2/24{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/Homicide_`year'_m`month'_`dist'.dta" , nogenerate
		erase "$cd1/NearTables/Homicide_`year'_m`month'_`dist'.dta"
		}

	egen Hom_`dist'_2ms = rowtotal(Homicide_m1_`dist' - Homicide_m2_`dist')		
	egen Hom_`dist'_yr1 = rowtotal(Homicide_m1_`dist' - Homicide_m12_`dist')
	egen Hom_`dist'_yr2 = rowtotal(Homicide_m13_`dist' - Homicide_m24_`dist')
	egen Hom_`dist'_2yrs = rowtotal(Homicide_m1_`dist' - Homicide_m24_`dist')
	gen YEAR = `year'
	keep COLE_CODIGO_COLEGIO YEAR Hom_`dist'_2ms Hom_`dist'_yr1 Hom_`dist'_yr2 ///
		Hom_`dist'_2yrs
	save "$cd1/NearTables/Homicide_`year'_Greater`dist'.dta" , replace
	erase "$cd1/NearTables/Homicide_`year'_m1_`dist'.dta"
	}
}
	
*Merge into yearly databases
foreach year of numlist 2004/2014{
use "$cd1/NearTables/Homicide_`year'_Greater100.dta" , clear

	foreach dist of numlist 250 500 750 10 1250 1500 1750 20 2250 2500 2750{

	merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/Homicide_`year'_Greater`dist'.dta" , nogenerate
	erase "$cd1/NearTables/Homicide_`year'_Greater`dist'.dta"
	}

save "$cd1/NearTables/GreaterDist_`year'.dta" , replace
erase "$cd1/NearTables/Homicide_`year'_Greater100.dta"
}
	
** Bringing all years together
use "$cd1/NearTables/GreaterDist_2004.dta" , clear 

append using "$cd1/NearTables/GreaterDist_2005.dta" ///
	"$cd1/NearTables/GreaterDist_2006.dta" ///
	"$cd1/NearTables/GreaterDist_2007.dta" "$cd1/NearTables/GreaterDist_2008.dta" ///
	"$cd1/NearTables/GreaterDist_2009.dta" "$cd1/NearTables/GreaterDist_2010.dta"  ///
	"$cd1/NearTables/GreaterDist_2011.dta" "$cd1/NearTables/GreaterDist_2012.dta" ///
	"$cd1/NearTables/GreaterDist_2013.dta" "$cd1/NearTables/GreaterDist_2014.dta" , ///
	force

order COLE_CODIGO_COLEGIO YEAR

foreach i of numlist 10 20{
rename Hom_`i'_2ms Hom_`i'00_2ms 
rename Hom_`i'_yr1 Hom_`i'00_yr1 
rename Hom_`i'_yr2 Hom_`i'00_yr2
rename Hom_`i'_2yrs Hom_`i'00_2yrs
}

foreach var of varlist Hom_250_2ms - Hom_2750_2yrs{
replace `var' = 0 if `var' == .
}

save "$cd1/NearTables/Violence_All_GreaterDist.dta" , replace

** Bringing together with Violence_All.dta
merge  1:1 COLE_CODIGO_COLEGIO YEAR using "$cd1/NearTables/Violence_All.dta" , nogenerate

order Homicide_day* Homicide_wk* Homicide_m* Homicide_m24 Homicide_lead* ///
		Homicide_lead24 ///
	Manslaughter_m1-Manslaughter_m23 Manslaughter_m24 ///
	Auto_Theft_m1-Auto_Theft_m23 Auto_Theft_m24 ///
	Street_Muggings_m1-Street_Muggings_m23 Street_Muggings_m24 ///
	Home_Burglaries_m1-Home_Burglaries_m23 Home_Burglaries_m24 ///
	Drug_Arrests_m1-Drug_Arrests_m23 Drug_Arrests_m24 , after(Hom_2750_2yrs)


foreach var in Manslaughter Homicide Auto_Theft Street_Muggings Home_Burglaries ///
	Drug_Arrests{
	foreach i of numlist 1/24{
		label var `var'_m`i' "Incidents of `var' within 500m of school - Month `i' before exam"
		
		if "`var'" == "Homicide" {
		label var `var'_lead`i' "Incidents of `var' within 500m of school - Month `i' after exam"	
		}
	}
	
}	

label var Homicide_day0 "Incidents of Homicide within 500m of school - Day of exam"
foreach i of numlist 1/6{
	label var Homicide_day`i' "Incidents of Homicide within 500m of school - Day `i' before exam"
}
foreach i of numlist 1/4{
	label var Homicide_wk`i' "Incidents of Homicide within 500m of school - Week `i' before exam"
}	

**Identifying total crimes in different periods
*Homicides, Manslaughter, Muggings, and Drug Arrests
local crimes "Homicide Manslaughter Drug_Arrests Street_Muggings"
foreach crime of local crimes{

/*egen `crime'_q1 = rowtotal(`crime'_m1 - `crime'_m3 )
egen `crime'_q2 = rowtotal(`crime'_m4 - `crime'_m6 )
egen `crime'_q3 = rowtotal(`crime'_m7 - `crime'_m9 )
egen `crime'_q4 = rowtotal(`crime'_m10 - `crime'_m12 )
egen `crime'_q5 = rowtotal(`crime'_m13 - `crime'_m15 )
egen `crime'_q6 = rowtotal(`crime'_m16 - `crime'_m18 )
egen `crime'_q7 = rowtotal(`crime'_m19 - `crime'_m21 )
egen `crime'_q8 = rowtotal(`crime'_m22 - `crime'_m24 )

egen `crime'_sem1 = rowtotal(`crime'_m1 - `crime'_m6 )
egen `crime'_sem2 = rowtotal(`crime'_m7 - `crime'_m12 )
egen `crime'_sem3 = rowtotal(`crime'_m13 - `crime'_m18 )
egen `crime'_sem4 = rowtotal(`crime'_m19 - `crime'_m24 )*/

egen `crime'_yr1 = rowtotal(`crime'_m1 - `crime'_m12)
egen `crime'_yr2 = rowtotal(`crime'_m13 - `crime'_m24)
egen `crime'_2yrs = rowtotal(`crime'_m1 - `crime'_m24)
}


foreach var in Manslaughter Homicide Street_Muggings Drug_Arrests{
		
	label var `var'_yr1 "`var' within 500m of school - 12-month period before exam"
	label var `var'_yr2 "`var' within 500m of school - 13-24 month period before exam"
	label var `var'_2yrs "`var' within 500m of school - 2-year period before exam"		
	order `var'_yr1 `var'_yr2 `var'_2yrs , after(`var'_m24)
}

for X in var Hom_* Homicide_day0 - Drug_Arrests_2yrs : replace X = 0 if X == .

foreach i of numlist 250 500 750 1000 1250 1500 1750 2000 2250 2500 2750{
	label var Hom_`i'_2ms "Homicides within `i'm of school - 2-month period before exam"
	label var Hom_`i'_yr1 "Homicides within `i'm of school - 12-month period before exam"
	label var Hom_`i'_yr2 "Homicides within `i'm of school - 13-24 month period before exam"	
	label var Hom_`i'_2yrs "Homicides within `i'm of school - 2-year period before exam"		
}

save "$cd1/NearTables/Violence_All.dta" , replace
erase "$cd1/NearTables/Violence_All_GreaterDist.dta"



****************			250 meter radius				********************

use "$cd1/NearTables/ForTableGeneration/Schools_All.dta" , replace
local buffer = 250 // set buffer for 250 meters

foreach year of numlist 2004/2014{

local delitos "Manslaughter Homicide Auto_Theft Street_Muggings Home_Burglaries Drug_Arrests"

		foreach day of numlist 0/6{
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear

		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/Homicide_`year'_day`day'.dta",  ///
			n(delito_id y x ) within(0.`buffer') long near(0)
			
			capture confirm variable km_to_delito_id
			if _rc{
				gen Homicide_day`day'_`buffer'm = 0
				}
			else{	
				gen Homicide_day`day'_`buffer'm = 1
			}
			
		capture collapse (sum) Homicide_day`day'_`buffer'm , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/Homicide_`year'_day`day'_`buffer'm.dta" , replace
		}	

		foreach week of numlist 1 2 3 4{
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear

		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/Homicide_`year'_wk`week'.dta",  ///
			n(delito_id y x ) within(0.`buffer') long near(0)
			
			capture confirm variable km_to_delito_id
			if _rc{
				gen Homicide_wk`week'_`buffer'm = 0
				}
			else{	
				gen Homicide_wk`week'_`buffer'm = 1
			}
			
		capture collapse (sum) Homicide_wk`week'_`buffer'm , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/Homicide_`year'_wk`week'_`buffer'm.dta" , replace
		} 
		
		foreach forward of numlist 1/24{
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear

		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/Homicide_`year'_lead`forward'.dta",  ///
			n(delito_id y x ) within(0.`buffer') long near(0)
		
			capture confirm variable km_to_delito_id
			if _rc{
				gen Homicide_lead`forward'_`buffer'm = 0
				}
			else{	
				gen Homicide_lead`forward'_`buffer'm = 1
			}
	
		capture collapse (sum) Homicide_lead`forward'_`buffer'm , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/Homicide_`year'_lead`forward'_`buffer'm.dta" , replace
		}		
		
	foreach crime of local delitos{
				
		foreach month of numlist 1/24{
		use "$cd1/NearTables/ForTableGeneration/Schools_All.dta", clear

		capture geonear COLE_CODIGO_COLEGIO y x using "$cd1/NearTables/ForTableGeneration/`crime'_`year'_m`month'.dta",  ///
			n(delito_id y x ) within(0.`buffer') long near(0)
		
			capture confirm variable km_to_delito_id
			if _rc{
				gen `crime'_m`month'_`buffer'm = 0
				}
			else{	
				gen `crime'_m`month'_`buffer'm = 1
			}
		
		capture collapse (sum) `crime'_m`month'_`buffer'm , by(COLE_CODIGO_COLEGIO)
		save "$cd1/NearTables/`crime'_`year'_m`month'_`buffer'm.dta" , replace
		}
		
	if "`crime'" == "Homicide"{	
	
		use "$cd1/NearTables/`crime'_`year'_day0_`buffer'm.dta" , clear
		foreach day of numlist 1/6{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_day`day'_`buffer'm.dta" , nogenerate
		}	
	
		foreach week of numlist 1 2 3 4{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_wk`week'_`buffer'm.dta" , nogenerate
		}	
	
		foreach forward of numlist 1/24{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_lead`forward'_`buffer'm.dta" , nogenerate
		}
		
		foreach month of numlist 1/24{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_m`month'_`buffer'm.dta" , nogenerate
		}
	}
	
	if "`crime'" != "Homicide"{
		foreach month of numlist 1/24{
		merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_m`month'_`buffer'm.dta" , nogenerate
		}
	}
	
	save "$cd1/NearTables/`crime'_`year'_`buffer'm.dta" , replace
	}
	
	use "$cd1/NearTables/Manslaughter_`year'_`buffer'm.dta" , clear
	local delitos "Homicide Auto_Theft Street_Muggings Home_Burglaries Drug_Arrests"
	foreach crime of local delitos{
	merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'_`buffer'm.dta" , nogenerate
	}
	
	gen YEAR = `year'
	save "$cd1/NearTables/`year'_`buffer'm.dta" , replace
}


** Bringing all years together

use "$cd1/NearTables/2004_`buffer'm.dta" , clear 

append using "$cd1/NearTables/2005_`buffer'm.dta" "$cd1/NearTables/2006_`buffer'm.dta" 	///
	"$cd1/NearTables/2007_`buffer'm.dta" ///
	"$cd1/NearTables/2008_`buffer'm.dta" "$cd1/NearTables/2009_`buffer'm.dta" ///
	"$cd1/NearTables/2010_`buffer'm.dta" "$cd1/NearTables/2011_`buffer'm.dta" ///
	"$cd1/NearTables/2012_`buffer'm.dta" "$cd1/NearTables/2013_`buffer'm.dta" ///
	"$cd1/NearTables/2014_`buffer'm.dta" , force

drop delito_id km_to_delito_id


order COLE_CODIGO_COLEGIO YEAR ///
	Homicide_day*_250m Homicide_wk*_250m Homicide_m*_250m Homicide_m24_250m ///
	Homicide_lead*_250m Homicide_lead24_250m ///
	Manslaughter_m1_250m-Manslaughter_m23_250m Manslaughter_m24_250m ///
	Auto_Theft_m1_250m-Auto_Theft_m23_250m Auto_Theft_m24_250m ///
	Street_Muggings_m1_250m-Street_Muggings_m23_250m Street_Muggings_m24_250m ///
	Home_Burglaries_m1_250m-Home_Burglaries_m23_250m Home_Burglaries_m24_250m ///
	Drug_Arrests_m1_250m-Drug_Arrests_m23_250m Drug_Arrests_m24_250m

foreach var in Manslaughter Homicide Auto_Theft Street_Muggings Home_Burglaries ///
	Drug_Arrests{
	foreach i of numlist 1/24{
		label var `var'_m`i'_`buffer'm "Incidents of `var' within `buffer'm of school - Month `i' before exam"
		
		if "`var'" == "Homicide" {
		label var `var'_lead`i'_`buffer'm "Incidents of `var' within `buffer'm of school - Month `i' after exam"	
		}
	}
	
}	

label var Homicide_day0 "Incidents of Homicide within `buffer'm of school - Day of exam"
foreach i of numlist 1/6{
	label var Homicide_day`i'_`buffer'm "Incidents of Homicide within `buffer'm of school - Day `i' before exam"
}
foreach i of numlist 1/4{
	label var Homicide_wk`i'_`buffer'm "Incidents of Homicide within `buffer'm of school - Week `i' before exam"
}	


for X in var *_250m : replace X = 0 if X == .
merge 1:1 COLE_CODIGO_COLEGIO YEAR using "$cd1/NearTables/Violence_All.dta" , nogenerate

merge m:1 COLE_CODIGO_COLEGIO using  ///
	"$cd1/NearTables/ForTableGeneration/Schools_All.dta" , nogenerate keepus(x y)
geoinpoly y x using ///
	"$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_coord.dta"
merge m:1 _ID using ///
	"$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_db.dta" , ///
	keepus(LIMITECOMU IDENTIFICA) nogenerate keep(1 3)
preserve
import excel using "$cd1/Base de datos Proyecto Medellín Gangs.xlsx" , clear ///
	sheet("Censo GDO 2016") firstrow
gen gang_presence = 1	
collapse gang_presence , by(Cod_Barrio Comuna TipoB)	
bys Cod_Barrio : gen n = _n
reshape wide TipoB , i(Cod_Barrio Comuna gang_presence) j(n)

tempfile temp
save "`temp'" , replace
restore	

rename IDENTIFICA Cod_Barrio
merge m:1 Cod_Barrio using "`temp'" , nogenerate keep(1 3)
drop y x _ID LIMITECOMU Comuna TipoB4 TipoB5
label var Cod_Barrio "Barrio ID"
label var gang_presence "Combo Presence"
label var TipoB1 "Banda/Ranzon 1 with combo presence in barrio"
label var TipoB2 "Banda/Ranzon 2 with combo presence in barrio"
label var TipoB3 "Banda/Ranzon 3 with combo presence in barrio"

save "$cd1/NearTables/Violence_All.dta" , replace



****************		Erasing extra databases				********************

local delitos "Manslaughter Homicide Auto_Theft Street_Muggings Home_Burglaries Drug_Arrests"
foreach crime of local delitos{
	foreach year of numlist 2004/2014{
		foreach month of numlist 1/24{
		erase "$cd1/NearTables/`crime'_`year'_m`month'.dta"
		erase "$cd1/NearTables/ForTableGeneration/`crime'_`year'_m`month'.dta"		
		erase "$cd1/NearTables/`crime'_`year'_m`month'_250m.dta"		
		}
		
		if "`crime'" == "Homicide"{
		foreach day of numlist 0/6{
		erase "$cd1/NearTables/`crime'_`year'_day`day'.dta"
		erase "$cd1/NearTables/ForTableGeneration/`crime'_`year'_day`day'.dta"	
		erase "$cd1/NearTables/`crime'_`year'_day`day'_250m.dta"		
		}
		foreach week of numlist 1/4{
		erase "$cd1/NearTables/`crime'_`year'_wk`week'.dta"
		erase "$cd1/NearTables/ForTableGeneration/`crime'_`year'_wk`week'.dta"	
		erase "$cd1/NearTables/`crime'_`year'_wk`week'_250m.dta"			
		}			
		foreach forward of numlist 1/24{
		erase "$cd1/NearTables/`crime'_`year'_lead`forward'.dta"
		erase "$cd1/NearTables/ForTableGeneration/`crime'_`year'_lead`forward'.dta"		
		erase "$cd1/NearTables/`crime'_`year'_lead`forward'_250m.dta"
		}
		}
		erase "$cd1/NearTables/`crime'_`year'.dta"
		erase "$cd1/NearTables/`crime'_`year'_250m.dta"
	}
}
*

foreach i of  numlist 2004/2014{
erase "$cd1/NearTables/`i'.dta"
erase "$cd1/NearTables/`i'_250m.dta"
erase "$cd1/NearTables/GreaterDist_`i'.dta"
}

erase "$cd1/motherfile.dta" 
