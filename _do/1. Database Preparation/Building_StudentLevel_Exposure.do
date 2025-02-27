********************************************************************************
***		Don Berna and the Effect of Homicide Exposure on Students			 ***
***		Do-file for measuring student-level exposure to homicides			 ***
***				Written by: Greg Haugan, haugan-gregory@norc.org			 ***
********************************************************************************


global cd1 "H:/4. Personal/Violence_Education/_dta/RawData"
cd "$cd1"
set more off


****************		Step 1: Prepare Crimes Data 		********************

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

*Save data for generating near tables of homicides within 1-24 months of the exam, and 1-24 month leads
tempfile motherfile
save "`motherfile'" , replace

local delitos "Homicide" // Let's just run using homicides. Can be run for all crimes using line below instead.
*local delitos "Manslaughter Homicide Auto_Theft Street_Muggings Home_Burglaries Drug_Arrests"
			*Export 2009 - Sept 13 Exam Date (Sept 13, 2009 = 18153 in Stata's date format)
	forvalues i = 1/24 { // for each month 1, 2...24 before exam
	keep if (18153 - (30*`i')) <= date & date < (18153 - (30*(`i'-1))) // keep crimes in that month
	
		foreach d of local delitos{ // save separate dataset for each crime for each month
		preserve
		keep if crime == "`d'"
		keep delito_id x y
		tempfile `d'_2009_m`i'
		capture save "``d'_2009_m`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}	

	foreach i of numlist 1/12 { // repeat excerise using months after exam
	keep if (18153 + (30*`i')) >= date & (date > (18153 + (30*(`i'-1))))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2009_lead`i'
		capture save "``d'_2009_lead`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}

	
			*Export 2010 - Sept 12 Exam Date (Sept 12, 2010 = 18517 in Stata's date format)
	forvalues i = 1/24 { // for each month 1, 2...24 before exam
	keep if (18517 - (30*`i')) <= date & date < (18517 - (30*(`i'-1))) // keep crimes in that month
	
		foreach d of local delitos{ // save separate dataset for each crime for each month
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2010_m`i'
		capture save "``d'_2010_m`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}	

	foreach i of numlist 1/12 {
	keep if (18517 + (30*`i')) >= date & (date > (18517 + (30*(`i'-1))))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2010_lead`i'
		capture save "``d'_2010_lead`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}
	
			*Export 2011 - Sept 4 Exam Date (Sept 4, 2011 = 18874 in Stata's date format)
	forvalues i = 1/24 { // for each month 1, 2...24 before exam
	keep if (18874 - (30*`i')) <= date & date < (18874 - (30*(`i'-1))) // keep crimes in that month
	
		foreach d of local delitos{ // save separate dataset for each crime for each month
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2011_m`i'
		capture save "``d'_2011_m`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}	

	foreach i of numlist 1/12 {
	keep if (18874 + (30*`i')) >= date & (date > (18874 + (30*(`i'-1))))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2011_lead`i'
		capture save "``d'_2011_lead`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}
	
			*Export 2012 - Sept 2 Exam Date (Sept 2, 2012 = 19238 in Stata's date format)
	forvalues i = 1/24 { // for each month 1, 2...24 before exam
	keep if (19238 - (30*`i')) <= date & date < (19238 - (30*(`i'-1))) // keep crimes in that month
	
		foreach d of local delitos{ // save separate dataset for each crime for each month
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2012_m`i'
		capture save "``d'_2012_m`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}	

	foreach i of numlist 1/12 {
	keep if (19238 + (30*`i')) >= date & (date > (19238 + (30*(`i'-1))))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2012_lead`i'
		capture save "``d'_2012_lead`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}
	
			*Export 2013 - Aug 25 Exam Date (Aug 25, 2013 = 19595 in Stata's date format)
	forvalues i = 1/24 { // for each month 1, 2...24 before exam
	keep if (19595 - (30*`i')) <= date & date < (19595 - (30*(`i'-1))) // keep crimes in that month
	
		foreach d of local delitos{ // save separate dataset for each crime for each month
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2013_m`i'
		capture save "``d'_2013_m`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}	

	foreach i of numlist 1/12 {
	keep if (19595 + (30*`i')) >= date & (date > (19595 + (30*(`i'-1))))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2013_lead`i'
		capture save "``d'_2013_lead`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}


			*Export 2014 - Aug 3 Exam Date (Aug 3, 2014 = 19938 in Stata's date format)
	forvalues i = 1/24 { // for each month 1, 2...24 before exam
	keep if (19938 - (30*`i')) <= date & date < (19938 - (30*(`i'-1))) // keep crimes in that month
	
		foreach d of local delitos{ // save separate dataset for each crime for each month
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2014_m`i'
		capture save "``d'_2014_m`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}	

	*Crimes data does not go far enough for us to do leads for 2014
/*	foreach i of numlist 1/12 {
	keep if (19938 + (30*`i')) >= date & (date > (19938 + (30*(`i'-1))))
	
		foreach d of local delitos{
		preserve
		keep if crime == "`d'"
		keep delito_id x y 
		tempfile `d'_2014_lead`i'
		capture save "``d'_2014_lead`i''" , replace
		restore
		}
	use "`motherfile'" , clear
	}
*/



**************** 	Step 2: Measure Student-Level Exposure	********************

**** Read in our student dataset
use "$cd1/data_icfes_08.jun.22.dta" , clear

unique documento // some students appear in more than one year.
bys documento : egen min = min(periodo) // keep only first instance of those students
keep if min == periodo 

keep if inrange(icfes_estudiante_edad , 16 , 19) // only analyzing students aged 16-19. do this now to reduce the number of students we have to run this with. 
keep if icfes_colegio_origen == 0 // only public schools

unique documento // make sure documento is a unique ID
drop min

tempfile students_all
save "`students_all'" , replace

* Find crimes within 750m of each residence. Use the near(0) option
* to exclude schools if no crime in range during the time period
foreach dist of numlist 100 250{
foreach year of numlist 2009/2014{

local delitos "Homicide" // define crimes we wan't to run this for

	foreach crime of local delitos{
		
		foreach month of numlist 1/24{ 
		use "`students_all'" , clear // read in full student database
		keep if periodo == `year'2
		
		local buffer = .`dist' // buffer of .1 means 100 meter radius, .25 = 250 m
		*Find crimes within buffer of residence for given time period
		capture geonear documento lat lon using "``crime'_`year'_m`month''" ,  ///
			n(delito_id y x ) within(`buffer') long near(0)

		*Count number of homicides within buffer per student
		capture collapse (count) `crime'_m`month'_`dist' = delito_id , by(documento)
		tempfile `crime'_`year'_m`month'_`dist'
		save 	 "``crime'_`year'_m`month'_`dist''" , replace

		}
		
		if `year' <= 2013{ // we only have leads for 2009-2013 (not 2014)
		foreach forward of numlist 1/12{ // repeat for months after exam
		use "`students_all'" , clear // read in full student database

		keep if periodo == `year'2
		
		local buffer = .`dist' // buffer of .1 means 100 meter radius, .25 = 250 m
		capture geonear documento lat lon using "``crime'_`year'_lead`forward''" ,  ///
			n(delito_id y x ) within(`buffer') long near(0)
	
		*Count number of homicides within buffer per student
		capture collapse (count) `crime'_lead`forward'_`dist' = delito_id , by(documento)
		tempfile `crime'_`year'_lead`forward'_`dist'
		save "``crime'_`year'_lead`forward'_`dist''" , replace
	
		}
		}
		
	use "``crime'_`year'_m1_`dist''"	
	foreach month of numlist 2/24{ // merge montly exposure datasets into single file
	merge 1:1 documento using "``crime'_`year'_m`month'_`dist''" , nogenerate
	}
	
	if `year' <= 2013{ // we only have leads for 2009-2013 (not 2014)
		foreach forward of numlist 1/12{
		merge  1:1 documento using "``crime'_`year'_lead`forward'_`dist''" , nogenerate
		}
	}
	
	tempfile `crime'_`year'_`dist'
	save "``crime'_`year'_`dist''" , replace
	}

	use "`Homicide_`year'_`dist''" , clear

*	Use this if we're doing the task for multiple crimes
	/*local delitos "Manslaughter Auto_Theft Street_Muggings Home_Burglaries Drug_Arrests"
	foreach crime of local delitos{
	merge  1:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/`crime'_`year'.dta" , gen(m`crime'`year')
	drop m`crime'`year'
	}*/
	
	gen periodo = `year'2
	tempfile Y`year'_`dist'
	save "`Y`year'_`dist''" , replace
	
}
}

** Bringing all years together
use "`Y2009_250'" , clear 
append using "`Y2010_250'" "`Y2011_250'" ///
	"`Y2012_250'" "`Y2013_250'" "`Y2014_250'" , force
	
foreach year of numlist 2009/2014{	
merge 1:1 documento periodo using "`Y`year'_100'" , nogenerate
}

tempfile Violence_All
save "`Violence_All'" , replace


**************** 		Step 3: Create Final Database		********************

use "`students_all'" , clear // Read in student data
merge 1:1 documento periodo using "`Violence_All'" 

foreach dist of numlist 100 250{
foreach i of numlist 1/24{
	label var Homicide_m`i'_`dist' "Homicides within `dist'm of student home, Month `i' before exam"
	replace Homicide_m`i'_`dist' = 0 if Homicide_m`i'_`dist' == . & !mi(lat)
	rename Homicide_m`i'_`dist' Homicide_m`i'_`dist'm_res
}
foreach i of numlist 1/12{
	label var Homicide_lead`i'_`dist' "Homicides with `dist'm of student home, Month `i' after exam"
	replace Homicide_lead`i'_`dist' = 0 if Homicide_lead`i'_`dist' == . & !mi(lat) & periodo != 20142
	rename Homicide_lead`i'_`dist' Homicide_lead`i'_`dist'm_res
}
}

*Convert Barrio shapefiles to dta
shp2dta using "$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014" , ///
	database("$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_db.dta") ///
	coordinates("$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_coord.dta") ///
	replace genid(_ID)
*Get comuna numbers of student residences
geoinpoly lat lon using "$cd1/Shapefiles/Comunas/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$cd1/Shapefiles/Comunas/comunadb.dta" , gen(comuna_merge) ///
	keepus(NUMERO_COM)
rename NUMERO_COM NUMERO_COM_home	
destring NUMERO_COM_home , gen(comuna_home)
drop comunaid

geoinpoly lat lon using "$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_coord.dta"
merge m:1 _ID using "$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_db.dta" , ///
	gen(barrio_merge) keepus(IDENTIFICA NOMBRE) keep(1 3)

drop if NUMERO_COM_home == "" & !mi(lon) // drop observations outside of urban medellin

label var NUMERO_COM_home "Comuna of Student's Home"
label var comuna_home "Comuna of Student's Home"
label var IDENTIFICA "Barrio ID for Student's Home" 
label var NOMBRE "Barrio Name for Student's Home"
rename IDENTIFICA barrio_id_res
rename NOMBRE barrio_res

*Generate dummy for Don Berna Comunas	
gen berna_comuna = (comuna_home == 1 | comuna_home == 13 | comuna_home == 8 | comuna_home == 3 | ///
	comuna_home == 5 | comuna_home == 6)
label var berna_comuna "Student Lives in Berna-Controlled Neighborhood"	

**Demobilized population from Bloques Cacique Nutibara and Heroes de Granada living in each comuna
gen BernaStrength = 0 
*Add numbers from each Berna-controlled block and divide by total number of paramilitaries 
	*in the comuna, as reported in Alonso and Valencia article
replace BernaStrength = (176 + 245) / 527 if comuna_home == 1
replace BernaStrength = (21 + 68) / 179 if comuna_home == 13 
replace BernaStrength = (99 + 117) / 324 if comuna_home == 8 
replace BernaStrength = (97 + 275) / 490 if comuna_home == 3 
replace BernaStrength = (8 + 36) / 212 if comuna_home == 5 
replace BernaStrength = (46 + 51) / 248 if comuna_home == 6 
replace BernaStrength = (0 + 1) / 12 if comuna_home == 7
label var BernaStrength "Student's Home Comuna-Level Measure of Berna Hegemony"


***Control Variables
*Gender is string, make factor 
gen female = 1 if icfes_estudiante_genero == 0
replace female = 0 if icfes_estudiante_genero == 1
label var female "Female Student"

*Create reformatted rural dwelling variable
gen rural = 1 if estu_areareside == "Area Rural" 
replace rural = 0 if estu_areareside == "Cabecera Municipal" 
label var rural "Rural dwelling"

*Shift is string, make factor 
gen COLE_INST_JORNADA = "Morning" if icfes_estudiante_colegio_jornada == 2
replace COLE_INST_JORNADA = "Full Day" if icfes_estudiante_colegio_jornada == 1
replace COLE_INST_JORNADA = "Afternoon" if icfes_estudiante_colegio_jornada == 4
replace COLE_INST_JORNADA = "Night" if icfes_estudiante_colegio_jornada == 5

*Shift
encode COLE_INST_JORNADA , gen(shift)
label var shift "Shift"

*Strata
gen Strata = "Strata 1" if icfes_estudiante_estrato == 1
replace Strata = "Strata 2" if icfes_estudiante_estrato == 2 
replace Strata = "Strata 3" if icfes_estudiante_estrato == 3  
replace Strata = "Strata 4" if icfes_estudiante_estrato == 4  	
replace Strata = "Strata 5" if icfes_estudiante_estrato == 5 
replace Strata = "Strata 6" if icfes_estudiante_estrato == 6
encode Strata , gen(strata)
drop Strata
label var strata "Socioeconomic Strata"

*Mother's and father's education. Data dictionary for 2013 shows different values for dropout status.
gen mother_dropout = 1 if icfes_madre_educacion == 1
replace mother_dropout = 0 if inlist(icfes_madre_educacion , 2 , 3 , 4)
label var mother_dropout "Mother Highest Education is Primary or Less"

*Age
gen ESTU_EDAD = icfes_estudiante_edad
label var ESTU_EDAD "Age"

*Year
gen YEAR = 2009 if periodo == 20092
foreach i of numlist 2010/2014{
	replace YEAR = `i' if periodo == `i'2
}
label var YEAR "Exam Year"

*School IDs
gen COLE_INST_NOMBRE = cole_nombre_sede
gen DANE_sede = cole_cod_dane_sede
gen CODIGO_DANE = codigodane
gen COLE_CODIGO_COLEGIO = icfes_estudiante_colegio_codigo

*Dependent Variables
gen MATEMATICAS_PUNT = icfes_puntaje_matematicas
label var MATEMATICAS_PUNT "Math Raw Score" 

order YEAR documento NUMERO_COM_home comuna_home COLE_INST_NOMBRE DANE_sede ///
	CODIGO_DANE COLE_CODIGO_COLEGIO female rural shift mother_dropout ESTU_EDAD ///
	strata MATEMATICAS_PUNT Homicide*

*Bring in School-level violence and lat/long data	
replace DANE_sede = CODIGO_DANE if mi(DANE_sede)	
	
preserve
use "$cd1/NearTables/Violence_All.dta" , clear
keep if inrange(YEAR , 2009 , 2014)
bys COLE_CODIGO_COLEGIO YEAR : keep if _n == 1

merge m:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/ForTableGeneration/Schools_All.dta" 

for X in var Homicide_m1 - Homicide_m24 : rename X X_school
for X in var Homicide_m1_school - Homicide_m24_school : replace X = 0 if X == .

*Get comuna numbers of schools
geoinpoly y x using "$cd1/Shapefiles/Comunas/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$cd1/Shapefiles/Comunas/comunadb.dta" , gen(comuna_merge) ///
	keepus(NUMERO_COM)
destring NUMERO_COM , gen(comuna)
drop comunaid

geoinpoly y x using "$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_coord.dta"
merge m:1 _ID using "$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_db.dta" , ///
	gen(barrio_merge_school) keepus(IDENTIFICA NOMBRE) keep(1 3)

rename (NUMERO_COM comuna) (NUMERO_COM_study comuna_study)
label var NUMERO_COM_study "Comuna of Student's School"
label var comuna_study "Comuna of Student's School"
label var IDENTIFICA "Barrio ID of Student's School"
label var NOMBRE "Barrio Name of Student's School"

rename IDENTIFICA barrio_id_school
rename NOMBRE barrio_school

replace DANE_sede = 105001013421 if DANE_sede == . & COLE_CODIGO_COLEGIO == 35931 // found the DANE_sede in the 2009-2014 database based on the COLE_CODIGO_COLEGIO

bys DANE_sede YEAR : keep if _n == 1

tempfile temp
save "`temp'"
restore
	
destring COLE_CODIGO_COLEGIO, replace	
merge m:1 DANE_sede YEAR using "`temp'" , ///
	keepus(Homicide_m?_250* Homicide_m??_250* Hom_100_* NUMERO_COM_study ///
	comuna_study y x barrio_id_school barrio_school) keep(1 3) generate(school_merge)

label var Hom_100_2ms "Incidents of Homicide within 100m of school - mos. 1-2 before exam"
label var Hom_100_yr1 "Incidents of Homicide within 100m of school - mos. 1-12 before exam"
label var Hom_100_yr2 "Incidents of Homicide within 100m of school - mos. 13-24 before exam"
label var Hom_100_2yrs	"Incidents of Homicide within 100m of school - 24 mos before exam"
	
*Measure distance between home and school
geodist lat lon y x , generate(school_dist) 	
label var school_dist "Distance between student home and school (km)"
order 	school_dist , after(x)
	
*Final Cleanup	
order NUMERO_COM_study comuna_study berna_comuna BernaStrength , after(comuna_home)
order lat lon y x , after(COLE_CODIGO_COLEGIO)
label var lat "Latitude - Home"
label var lon "Longitude - Home"	
label var y "Latitude - School"
label var x "Longitude - School"	
	
label var COLE_INST_NOMBRE "School Name"

order MATEMATICAS_PUNT , after(x)
	
save "H:/4. Personal/Violence_Education/_dta/ViolenceOnEducation_StudentLevelExposure_2009_2014.dta" , replace	

/*
egen homicides = rowtotal(Homicide_m1 - Homicide_m12) , missing
egen homicides_school = rowtotal(Homicide_m1_school - Homicide_m12_school) , missing
gen lnhomicides = ln(homicides)
gen lnmath = ln(MATEMATICAS_PUNT)
gen lndist = ln(school_dist)
gen long_dist = school_dist >= 1 if !mi(school_dist)

areg lnmath homicides homicides_school lndist i.shift i.mother_dropout i.strata i.ESTU_EDAD i.female i.YEAR , absorb(DANE_sede) cluster( comuna_home )