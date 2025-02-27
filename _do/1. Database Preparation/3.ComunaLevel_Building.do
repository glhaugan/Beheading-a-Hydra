/*Necesitamos lo que te dije en el correo. "What I need is just one* database (instead of multiple smaller databases) at the comuna-year level with information to run all the comuna-year level regressions. That database will have the homicides the year prior to the exam (which I do not know how to build), SABER 11 test scores, dropout, dropout by gender, dropout by strata,  transfers, transfers by gender, transfers by strata, teaching turnover,  teaching turnover according to the teachers' educational attainment and -if I am understanding anything- teaching turnover by the teachers' qualifications as according to your classification in the table below."

Tal vez solo me falta precisar que cuando las variables son porcentajes, también dejes las variables en niveles. Es decir, si tenemos dropout como porcentaje definido de la siguiente forma:

turnover_seconday_pct  = 100* (# secondary teachers  who left their school / # of secondary teachers)

Dejemos en la base de datos también las variables "# secondary teachers  who left their school" y
"# of secondary teachers"

Creo que esto ya lo estabas haciendo. Necesitamos el turnover solo para los maestros de secundaria. También incluir en la misma base el turnover según educational attainment y según tus definciones de low- medium- y high-skill.
*/


/* Install from ssc if not installed already
ssc install geoinpoly
ssc install shp2dta
ssc install spmap
ssc install estout
ssc install reghdfe
ssc install ftools
*/

global path "H:/4. Personal/Violence_Education"

set more off
set matsize 2000


/*This creates comuna-level database for homicides (and other crimes) at yearly frequency, with years defined by exam dates.*/

		**** Generate Barrio-Level Panel at Annual Frequency ****
	
*Read in crimes data and identify which barrio they occurred in
use "$path/_dta/RawData/Medellin_crimes.dta" , clear

geoinpoly y x using "$path/_dta/RawData/Shapefiles/Barrios Reprojected/BarrioVereda_2014_coord.dta"

merge m:1 _ID using ///
	"$path/_dta/RawData/Shapefiles/Barrios Reprojected/BarrioVereda_2014_db.dta" , ///
	keep(3) nogenerate	
	
	
*Generate separate count variables for each crime
tab delito , gen(delito_)
for X in var delito_* : replace X = total if X == 1 // rather than binary, assign values as the total count
foreach i of numlist 1/9{
	local lab_`i' : var lab delito_`i'
	local lab_`i' = subinstr("`lab_`i''" , "delito==" , "" , .)
	di "lab_`i'"
}

*Count crimes by neighborhood by year
drop fecha
gen temp = string(mes_num) + "/" + string(dia) + "/" + string(year)
gen fecha = date(temp, "MDY") , after(year)
format fecha %td
drop temp
	
	*year defined by exam date, so requires some manipulation
foreach i of numlist 1/9{
	
	*2004 - October 2 Exam Date (Oct 2,2004 = 16346 in Stata's date format)
	gen delito_`i'_2004 = delito_`i' if inrange(fecha , 16346 - 365 , 16346 - 1)
	
	*2005 - October 9 Exam Date (Oct 9,2005 = 16718 in Stata's date format)
	gen delito_`i'_2005 = delito_`i' if inrange(fecha , 16718 - 365 , 16718 - 1)
	
	*2006 - Sept 24 Exam Date (Sept 24,2006 = 17068 in Stata's date format)
	gen delito_`i'_2006 = delito_`i' if inrange(fecha , 17068 - 365 , 17068 - 1)	
	
	*2007 - Sept 23 Exam Date (Sept 23, 2007 = 17432 in Stata's date format)
	gen delito_`i'_2007 = delito_`i' if inrange(fecha , 17432 - 365 , 17432 - 1)
	
	*2008 - Sept 21 Exam Date (Sept 21, 2008 = 17796 in Stata's date format)
	gen delito_`i'_2008 = delito_`i' if inrange(fecha , 17796 - 365 , 17796 - 1)
	
	*2009 - Sept 13 Exam Date (Sept 13, 2009 = 18153 in Stata's date format)
	gen delito_`i'_2009 = delito_`i' if inrange(fecha , 18153 - 365 , 18153 - 1)
	
	*2010 - Sept 12 Exam Date (Sept 12, 2010 = 18517 in Stata's date format)
	gen delito_`i'_2010 = delito_`i' if inrange(fecha , 18517 - 365 , 18517 - 1)	
	
	*2011 - Sept 4 Exam Date (Sept 4, 2011 = 18874 in Stata's date format)
	gen delito_`i'_2011 = delito_`i' if inrange(fecha , 18874 - 365 , 18874 - 1)
	
	*2012 - Sept 2 Exam Date (Sept 2, 2012 = 19238 in Stata's date format)
	gen delito_`i'_2012 = delito_`i' if inrange(fecha , 19238 - 365 , 19238 - 1)	
	
	*2013 - Aug 25 Exam Date (Aug 25, 2013 = 19595 in Stata's date format)
	gen delito_`i'_2013 = delito_`i' if inrange(fecha , 19595 - 365 , 19595 - 1)	

	*2014 - Aug 3 Exam Date (Aug 3, 2014 = 19938 in Stata's date format)
	gen delito_`i'_2014 = delito_`i' if inrange(fecha , 19938 - 365 , 19938 - 1)	
	
}


	*Also counting by month for homicides
	foreach delito of numlist 1/9{
	foreach i of numlist 1/48{
	gen delito_`delito'_m`i'_2004 = delito_`delito' if ///
		inrange(fecha , 16346 - (30*`i') , 16346 - (30*(`i'-1)))
	
	gen delito_`delito'_m`i'_2005 = delito_`delito' if ///
		inrange(fecha , 16718 - (30*`i') , 16718 - (30*(`i'-1)))
	
	gen delito_`delito'_m`i'_2006 = delito_`delito' if ///
		inrange(fecha , 17068 - (30*`i') , 17068 - (30*(`i'-1)))	
	
	gen delito_`delito'_m`i'_2007 = delito_`delito' if ///
		inrange(fecha , 17432 - (30*`i') , 17432 - (30*(`i'-1)))
	
	gen delito_`delito'_m`i'_2008 = delito_`delito' if ///
		inrange(fecha , 17796 - (30*`i') , 17796 - (30*(`i'-1)))
	
	gen delito_`delito'_m`i'_2009 = delito_`delito' if ///
		inrange(fecha , 18153 - (30*`i') , 18153 - (30*(`i'-1)))
	
	gen delito_`delito'_m`i'_2010 = delito_`delito' if ///
		inrange(fecha , 18517 - (30*`i') , 18517 - (30*(`i'-1)))	
	
	gen delito_`delito'_m`i'_2011 = delito_`delito' if ///
		inrange(fecha , 18874 - (30*`i') , 18874 - (30*(`i'-1)))
	
	gen delito_`delito'_m`i'_2012 = delito_`delito' if ///
		inrange(fecha , 19238 - (30*`i') , 19238 - (30*(`i'-1)))	
	
	gen delito_`delito'_m`i'_2013 = delito_`delito' if ///
		inrange(fecha , 19595 - (30*`i') , 19595 - (30*(`i'-1)))
		
	gen delito_`delito'_m`i'_2014 = delito_`delito' if ///
		inrange(fecha , 19938 - (30*`i') , 19938 - (30*(`i'-1)))			
	}
	}
	
gen exam_date_2004 = 16346
gen exam_date_2005 = 16718
gen exam_date_2006 = 17068
gen exam_date_2007 = 17432
gen exam_date_2008 = 17796
gen exam_date_2009 = 18153
gen exam_date_2010 = 18517
gen exam_date_2011 = 18874
gen exam_date_2012 = 19238
gen exam_date_2013 = 19595
gen exam_date_2014 = 19938


collapse (sum) delito_1_* delito_2_* delito_3_* delito_4_* delito_5_* ///
	delito_6_* delito_7_* delito_8_* delito_9_* (mean) exam_date_* , by(LIMITECOMU)

local varlist = ""
foreach delito of numlist 1/9{
	foreach month of numlist 1/48{
		local varlist = "`varlist'" + " delito_`delito'_m`month'_"
	}
}
di "`varlist'"
	
reshape long delito_1_ delito_2_ delito_3_ delito_4_ delito_5_ ///
	delito_6_ delito_7_ delito_8_ delito_9_ `varlist' exam_date_ , i(LIMITECOMU) j(year)	
	
foreach delito of numlist 1/9{
	rename delito_`delito'_ delito_`delito'
	label var delito_`delito' "Total `lab_`delito'' in Comuna - 1 year before exam"

foreach i of numlist 1/48{
	rename delito_`delito'_m`i'_ delito_`delito'_m`i'
	label var delito_`delito'_m`i' "Total `lab_`delito'' in Comuna in Month `i' Before Exam"
}
}


*Organize dataset
drop if inlist(LIMITECOMU , "50" , "60" , "70" , "80" , "90" , "SN01" , "SN02")
destring LIMITECOMU , replace
rename LIMITECOMU COMUNA
label var year "year"

rename exam_date_ exam_date
label var exam_date "Date of ICFES Exam"
format exam_date %td
sort COMUNA year

******	Bring in comuna controls

*Merge in Population Data
preserve
import delimited using "$path/_dta/RawData/Medellin_Population.csv" , ///
	varnames(1) clear
replace grupoedad = "0-14" if inlist(grupoedad , "0-4" , "9-May" , "14-Oct")
replace grupoedad = "20-29" if inlist(grupoedad , "20-24" , "25-29")
replace grupoedad = "30-49" if inlist(grupoedad , "30-34" , "35-39" , "40-44" , ///
	"45-49")
replace grupoedad = "50-69" if inlist(grupoedad , "50-54" , "55-59" , "60-64" , ///
	"65-69")	
replace grupoedad = "70+" if inlist(grupoedad , "70-74" , "75-79" , "80 Y MÁS" , ///
	"65-69")	
encode grupoedad , gen(agegroup)	
	
collapse (sum) hombres* mujeres* , ///
	by(codigodanemunicipio tipodivisiongeografica nombredivisiongeografica agegroup)
keep if tipodivisiongeografica == "Comuna"

replace nombredivisiongeografica = "La America" ///
	if nombredivisiongeografica == "La AmÃ©rica"
replace nombredivisiongeografica = "Belen" if nombredivisiongeografica == "BelÃ©n"

gen COMUNA = .
replace COMUNA = 16 if nombredivisiongeografica == "Belen"
replace COMUNA = 12 if nombredivisiongeografica == "La America"
replace COMUNA = 3 if nombredivisiongeografica == "Manrique"
replace COMUNA = 1 if nombredivisiongeografica == "Popular"
replace COMUNA = 10 if nombredivisiongeografica == "La Candelaria"
replace COMUNA = 4 if nombredivisiongeografica == "Aranjuez"
replace COMUNA = 15 if nombredivisiongeografica == "Guayabal"
replace COMUNA = 11 if nombredivisiongeografica == "Laureles - Estadio"
replace COMUNA = 8 if nombredivisiongeografica == "Villa Hermosa"
replace COMUNA = 14 if nombredivisiongeografica == "El Poblado"
replace COMUNA = 6 if nombredivisiongeografica == "Doce de Octubre"
replace COMUNA = 5 if nombredivisiongeografica == "Castilla"
replace COMUNA = 13 if nombredivisiongeografica == "San Javier"
replace COMUNA = 9 if nombredivisiongeografica == "Buenos Aires"
replace COMUNA = 2 if nombredivisiongeografica == "Santa Cruz"
replace COMUNA = 7 if nombredivisiongeografica == "Robledo"

*impute 2004
gen hombres2004 = hombres2005 + (hombres2005 - hombres2006) , before(hombres2005)
gen mujeres2004 = mujeres2005 + (mujeres2006 - mujeres2005) , before(mujeres2005)

foreach year of numlist 2004/2014{
	gen population`year' = hombres`year' + mujeres`year'
}

keep nombredivisiongeografica COMUNA population* agegroup
rename nombredivisiongeografica comuna_name

reshape long population , i(comuna_name COMUNA agegroup) j(year)
reshape wide population , i(comuna_name COMUNA year) j(agegroup)

label var population1 "0-14 age group population"
label var population2 "15-19 age group population"
label var population3 "20-29 age group population"
label var population4 "30-49 age group population"
label var population5 "50-69 age group population"
label var population6 "70+ age group population"

rename (population1 population2 population3 population4 population5 population6) ///
	(population_0_14 population_15_19 population_20_29 population_30_49 ///
	population_50_69 population_70plus)

keep if inrange(year , 2004 , 2014)

tempfile pop
save "`pop'" , replace

restore

merge m:1 COMUNA year using "`pop'" , nogenerate

label var COMUNA "Comuna Number"
label var year "Year"
egen population = rowtotal(population*) 
order population , before(population_0_14)
label var population "Total Comuna Population"

*Generate homicide rate	
gen homicide_rate = (delito_2 / population) * 100000 , before(delito_1)
label var homicide_rate "Comuna Annual Homicides per 100,000 inhabitants"
	
*Generate dummy for Don Berna Comunas	
gen berna_comuna = (COMUNA == 1 | COMUNA == 13 | COMUNA == 8 | COMUNA == 3 | ///
	COMUNA == 5 | COMUNA == 6) , before(homicide_rate)
label var berna_comuna "Berna-Controlled Comuna"	

**Demobilized population from Bloques Cacique Nutibara and Heroes de Granada living in each comuna
gen BernaStrength = 0 , before(homicide_rate)
*Add numbers from each Berna-controlled block and divide by total number of paramilitaries 
	*in the comuna, as reported in Alonso and Valencia article
replace BernaStrength = (176 + 245) / 527 if COMUNA == 1
replace BernaStrength = (21 + 68) / 179 if COMUNA == 13 
replace BernaStrength = (99 + 117) / 324 if COMUNA == 8 
replace BernaStrength = (97 + 275) / 490 if COMUNA == 3 
replace BernaStrength = (8 + 36) / 212 if COMUNA == 5 
replace BernaStrength = (46 + 51) / 248 if COMUNA == 6 
replace BernaStrength = (0 + 1) / 12 if COMUNA == 7
label var BernaStrength "Comuna-Level Measure of Berna Hegemony"

*Generate dummies for post-Berna time periods
gen post_transfer = (year > 2007) , before(homicide_rate) // Berna Prison Transfer on August 17, 2007 (571 in Stata months)
label var post_transfer "Post-Prison Transfer Period"
gen post_extradition = (year > 2008) , before(homicide_rate) // Berna Extradition on May 13 2008 (580 in Stata months)
label var post_extradition "Post-Extradition Period"

gen interaction_comuna_transfer = berna_comuna * post_transfer , ///
	after(post_extradition)
gen interaction_strength_transfer = BernaStrength * post_transfer , ///
	after(interaction_comuna_transfer)
label var interaction_comuna_transfer "Berna-Controlled Comuna = 1 $\times$ Post-Prison Transfer"
label var interaction_strength_transfer "BernaStrength $\times$ Post-Prison Transfer"

order COMUNA comuna_name year exam_date

preserve
import excel using "$path/_dta/RawData/Avance de base de datos Panel.xlsx" , ///
	firstrow clear
keep Codigodelbarrio cod_clean Barrio Ano NodeCapturas
keep if inrange(Ano , 2005 , 2014)
drop if cod_clean == ""

gen comuna = substr(cod_clean,1,2)
destring comuna , replace
collapse (sum) NodeCapturas , by(comuna Ano)
rename (Ano NodeCapturas comuna) (year capturas COMUNA)
label var capturas "Arrests"

sort year COMUNA
tempfile arrests
save "`arrests'" , replace
restore

sort year COMUNA
merge 1:1 year COMUNA using "`arrests'" , nogenerate 
order capturas , after(delito_9)
sort COMUNA year

save "$path/_dta/ViolenceonEducation_ComunaLevel_FullDataset.dta" , replace


/******	Create database with comuna controls
import excel using "$path/_dta/RawData/ComunaControls.xlsx" , firstrow clear

label var population_2004 "Comuna 2004 Population"
label var strata1 "Comuna 2004 Strata 1 Population"
label var strata2 "Comuna 2004 Strata 2 Population"
label var strata3 "Comuna 2004 Strata 3 Population"
label var strata4 "Comuna 2004 Strata 4 Population"
label var strata5 "Comuna 2004 Strata 5 Population"
label var strata6 "Comuna 2004 Strata 6 Population"
label var hectares "Comuna Size - Hectares"
label var displaced_2007 "Comuna Violently Displaced Population - 2007"

gen low_income_population = (strata1 + strata2) / population_2004
replace low_income_population = 1 if low_income_population > 1 & !mi(low_income_population)
label var low_income_population "2004 Comuna % of residents in low-income housing"

gen population_density = population_2004 / hectares
label var population_density "2004 Comuna Pop. Density (Residents per Hectare)"

merge 1:1 comuna using "`pop'" , nogenerate

gen displaced_population = displaced_2007 / population2007
label var displaced_population "2007 Displaced Population (% of Total)"

preserve
import delimited using "$path/_dta/RawData/Medellin_Population.csv", varnames(1) clear

keep if tipodivisiongeografica == "Comuna" & inlist(grupoedad, "15-19" , "20-24" , "25-29") 

replace nombredivisiongeografica = "La America" if nombredivisiongeografica == "La AmÃ©rica"
replace nombredivisiongeografica = "Belen" if nombredivisiongeografica == "BelÃ©n"

collapse (sum) hombres* mujeres* , ///
	by(codigodanemunicipio tipodivisiongeografica nombredivisiongeografica)

gen comuna = .
replace comuna = 16 if nombredivisiongeografica == "Belen"
replace comuna = 12 if nombredivisiongeografica == "La America"
replace comuna = 3 if nombredivisiongeografica == "Manrique"
replace comuna = 1 if nombredivisiongeografica == "Popular"
replace comuna = 10 if nombredivisiongeografica == "La Candelaria"
replace comuna = 4 if nombredivisiongeografica == "Aranjuez"
replace comuna = 15 if nombredivisiongeografica == "Guayabal"
replace comuna = 11 if nombredivisiongeografica == "Laureles - Estadio"
replace comuna = 8 if nombredivisiongeografica == "Villa Hermosa"
replace comuna = 14 if nombredivisiongeografica == "El Poblado"
replace comuna = 6 if nombredivisiongeografica == "Doce de Octubre"
replace comuna = 5 if nombredivisiongeografica == "Castilla"
replace comuna = 13 if nombredivisiongeografica == "San Javier"
replace comuna = 9 if nombredivisiongeografica == "Buenos Aires"
replace comuna = 2 if nombredivisiongeografica == "Santa Cruz"
replace comuna = 7 if nombredivisiongeografica == "Robledo"
	
gen young_2005 = (hombres2005 + mujeres2005)
keep young_2005 comuna

save "`pop'" , replace
restore

merge 1:1 comuna using "`pop'" , nogenerate

gen young_pop_2005 = (young_2005) / population2005
label var young_pop_2005 "Comuna 2005 Pop. Aged 15-29 (% of Total)"

keep comuna comuna_name low_income_population population_density ///
	displaced_population young_pop_2005 population2005 hectares

preserve
import excel using "$path/_dta/RawData/Avance de base de datos Panel.xlsx" , firstrow clear
keep Codigodelbarrio cod_clean Barrio Ano NodeCapturas
keep if Ano == 2005
drop if cod_clean == ""
rename NodeCapturas capturas_2005

gen comuna = substr(cod_clean,1,2)
destring comuna , replace
collapse (sum) capturas_2005 , by(comuna)

save "`pop'" , replace
restore

merge 1:1 comuna using "`pop'" , nogenerate

gen capturas = capturas_2005 / population2005
label var capturas "Arrests per Capita (2005)"

drop capturas_2005 population2005

save "$path/_dta/ComunaControls.dta" , replace*/



***************		Generate Comuna-Level Student Database 		****************

use "$path/_dta/ViolenceOnEducation.dta" , clear

*Shift
tab shift , gen(shift_)

*Strata
tab strata , gen(strata_)

*Age
tab ESTU_EDAD , gen(ESTU_EDAD_)


/*
**Only schools that appear in all years, 2006-2013
bys DANE_sede YEAR : gen ones = _n == 1
bys DANE_sede : egen total = sum(ones)
keep if total == 8
drop ones total
*/

/* To impute Comuna 14 database

gen imputed = ""
local new = _N + 1
set obs `new'
replace YEAR = 2010 in `new'
replace imputed = "YES" in `new'
local new = _N + 1
set obs `new'
replace YEAR = 2011 if mi(YEAR)
replace imputed = "YES" in `new'
		
ds , not(type string)
foreach var of varlist `r(varlist)'{

sum `var' if YEAR == 2009 & comuna == 14
cap local mean_09 = `r(mean)'
sum `var' if YEAR == 2012 & comuna == 14
cap local mean_12 = `r(mean)'

replace `var' = ((`mean_12'-`mean_09')/3)+`mean_09' if YEAR == 2010 & imputed == "YES"
replace `var' = ((`mean_12'-`mean_09')/3)*2+`mean_09' if YEAR == 2011 & imputed == "YES"

}		
	
drop if mi(comuna)	
	
save "H:/4. Personal/Berna_Paper/Berna Oct 2021/_dta/ViolenceOnEducation_ImputedComuna14.dta" , replace		
		


*/

gen total_students = 1

collapse (mean) LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT ///
	FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT stnd_lenguaje ///
	stnd_matematicas stnd_ciencias_sociales stnd_filosofia stnd_biologia ///
	stnd_quimica stnd_fisica ESTU_EDAD_* female minority rural shift_* strata_* ///
	Hom_750_yr1 Homicide_m1 Homicide_m2 Homicide_m3 Homicide_m4 Homicide_m5 ///
	Homicide_m6 Homicide_m7 Homicide_m8 Homicide_m9 Homicide_m10 Homicide_m11 ///
	Homicide_m12 (count) total_students , by(comuna YEAR)
	
foreach i of numlist 1/12{
	replace Homicide_m`i' = 0 if Homicide_m`i' == .
}	
	
label var YEAR "Exam Year"
label var comuna "Comuna"	
label var LENGUAJE_PUNT "Language - Raw Score"
label var MATEMATICAS_PUNT "Math - Raw Score"
label var CIENCIAS_SOCIALES_PUNT "Social Studies - Raw Score"
label var FILOSOFIA_PUNT "Philosophy - Raw Score"
label var BIOLOGIA_PUNT "Biology - Raw Score"
label var QUIMICA_PUNT "Chemistry - Raw Score"
label var FISICA_PUNT "Physics - Raw Score"
label var stnd_lenguaje "Language - Standardized Score"
label var stnd_matematicas "Math - Standardized Score" 
label var stnd_ciencias_sociales "Social Studies - Standardized Score"
label var stnd_filosofia "Philosophy - Standardized Score"
label var stnd_biologia "Biology - Standardized Score"
label var stnd_quimica "Chemistry - Standardized Score"
label var stnd_fisica "Physics - Standardized Score"
label var ESTU_EDAD_1 "Percentage of students aged 16"	
label var ESTU_EDAD_2 "Percentage of students aged 17"	
label var ESTU_EDAD_3 "Percentage of students aged 18"	
label var ESTU_EDAD_4 "Percentage of students aged 19"
label var female "Percentage of students who are female"
label var rural "Percentage of students who live in rural dwelling"
label var minority "Percentage of students from ethnic minority groups"	
label var shift_1 "Percentage of students attending afternoon shift"
label var shift_2 "Percentage of students attending full day shift"
label var shift_3 "Percentage of students attending morning shift"
label var shift_4 "Percentage of students attending night shift"
label var shift_5 "Percentage of students attending weekend shift"
label var strata_1 "Percentage of students in Strata 1"
label var strata_2 "Percentage of students in Strata 2"
label var strata_3 "Percentage of students in Strata 3"
label var strata_4 "Percentage of students in Strata 4"
label var strata_5 "Percentage of students in Strata 5"
label var strata_6 "Percentage of students in Strata 6"
label var Hom_750_yr1 "Avg. Homicides Per School in 1 year before exam (750m radius)" 
label var Homicide_m1 "Avg. Homicides Per School in month 1 before exam (750m radius)" 
label var Homicide_m2 "Avg. Homicides Per School in month 2 before exam (750m radius)" 
label var Homicide_m3 "Avg. Homicides Per School in month 3 before exam (750m radius)" 
label var Homicide_m4 "Avg. Homicides Per School in month 4 before exam (750m radius)" 
label var Homicide_m5 "Avg. Homicides Per School in month 5 before exam (750m radius)" 
label var Homicide_m6 "Avg. Homicides Per School in month 6 before exam (750m radius)" 
label var Homicide_m7 "Avg. Homicides Per School in month 7 before exam (750m radius)" 
label var Homicide_m8 "Avg. Homicides Per School in month 8 before exam (750m radius)" 
label var Homicide_m9 "Avg. Homicides Per School in month 9 before exam (750m radius)" 
label var Homicide_m10 "Avg. Homicides Per School in month 10 before exam (750m radius)" 
label var Homicide_m11 "Avg. Homicides Per School in month 11 before exam (750m radius)" 
label var Homicide_m12 "Avg. Homicides Per School in month 12 before exam (750m radius)" 
label var total_students "Total number of public school students with ICFES data"

rename (comuna YEAR) (COMUNA year)
sort COMUNA year

order COMUNA year ESTU_EDAD_* female minority rural shift_* strata_* ///
	total_students LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT ///
	FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT stnd_lenguaje ///
	stnd_matematicas stnd_ciencias_sociales stnd_filosofia stnd_biologia ///
	stnd_quimica stnd_fisica Hom_750_yr1 Homicide_m1 Homicide_m2 Homicide_m3 ///
	Homicide_m4 Homicide_m5 Homicide_m6 Homicide_m7 Homicide_m8 Homicide_m9 ///
	Homicide_m10 Homicide_m11 Homicide_m12

tempfile students
save "`students'" , replace


use "$path/_dta/ViolenceonEducation_ComunaLevel_FullDataset.dta" , clear
merge 1:1 COMUNA year using "`students'" , nogenerate

save "$path/_dta/ViolenceonEducation_ComunaLevel_FullDataset.dta" , replace



***************		Generate Comuna-Level Teacher Database 		****************

/*From Rafael: create db of teacher turnover by skill level at the comuna level. We can define teacher dropout as the number of teachers that move out over the number of teachers in the comuna. And we can do the same for high quality teachers: number of high quality teachers that  dropout over number of high quality teachers in the comuna. For the latter, we can include another option (Dividing by the total number of professors).  With luck both measures would work. But please do leave in the database the variables that are both in the nunerator and in the denominator in the rates defined above*/


use "$path/_dta/Teachers.dta" , clear


*We will need a variable that indicates survival time and one for turnover.
*Survival time will be number of years the teacher has been at the same school.
	*This is the time they are exposed to the risk of leaving the school.
*There is one observation for each teacher in each year they are working in a public school.
sort num_doc DANE_sede YEAR
bys num_doc DANE_sede: gen survtime=_n
tab survtime, gen(_ST)	
*The "turnover" variable (in our case, the decision not to work at the same school next year)
*turnover=1 if the teacher does not return the following year; =0 if they return or if the variable is censored.
sort num_doc DANE_sede YEAR
by num_doc DANE_sede: gen turnover=1 if _n==_N & YEAR!=2013	
by num_doc DANE_sede: replace turnover=0 if _n!=_N & YEAR!=2013
	
*There is a "Left Censoring" problem - we know when teachers entered the public school system, but 
	*for teachers entering before 2008, we don't know when they started working in their current school.
*Generate a "start year" variable to restrict the sample to teachers that entered the system in 2008 or later. 
gen year_start=substr(fecha_vinculacion,-4,.)
destring year_start, replace
*Experience
gen experience = YEAR - year_start

*Creating other variables
	*Age
gen year_birth=substr(fecha_nacimiento,-4,.)
destring year_birth, replace
gen age= YEAR-year_birth
tab age
	*dropping two strange observations
drop if age==11 | age==14
gen age_group=1 if age<30
replace age_group=2 if age>=30 & age<=45
replace age_group=3 if age>45 & age<=60
replace age_group=4 if age>60
tab age_group , gen(age_group_)

	*Salary
encode escalafon, gen(pay_scale)
	*Highest education completed
gen highest_secondary = (nivel_educativo_aprobado >= 0 & nivel_educativo_aprobado <= 3)
gen highest_twoyear = (nivel_educativo_aprobado == 4 | nivel_educativo_aprobado == 5)
gen highest_undergrad = (nivel_educativo_aprobado == 6 | nivel_educativo_aprobado == 7)
gen highest_postgrad = (nivel_educativo_aprobado == 8 | nivel_educativo_aprobado == 9)
	*Permanent Contract
gen perm_contract = (tipo_vinculacion == 1)
	*Secondary School Teacher
gen secondary = (nivel_ensenanza == 3)
	*Year dummy
tab YEAR, gen(_T)
	*School dummy
tab DANE_sede, gen(_S)

*Teacher qualifications
gen LowQuals=1 if nivel_educativo_aprobado==0 | nivel_educativo_aprobado==1 | nivel_educativo_aprobado==2 | nivel_educativo_aprobado==3
replace LowQuals=0 if nivel_educativo_aprobado!=0 & nivel_educativo_aprobado!=1 & nivel_educativo_aprobado!=2 & nivel_educativo_aprobado!=3
label var LowQuals "Teacher with low qualifications (Normalista/Bachiller or less)"
	
gen MidQuals = 1 if inrange(nivel_educativo_aprobado , 4 , 7) & ///
	!mi(nivel_educativo_aprobado)
replace MidQuals = 0 if !inrange(nivel_educativo_aprobado , 4 , 7) & ///
	!mi(nivel_educativo_aprobado)
label var MidQuals "Teacher with medium qualifications (2-year or undergrad degree)"	
	
gen HighQuals=1 if nivel_educativo_aprobado==8 | nivel_educativo_aprobado==9
replace HighQuals=0 if nivel_educativo_aprobado!=8 & nivel_educativo_aprobado!=9
label var HighQuals "Teacher with high qualifications (Post-graduate degree)"

order num_doc DANE_sede YEAR turnover survtime age

label var age "Age"
label var experience "Teaching Experience (Years)"
label var mujer "Sex=Female"
label var turnover "Turnover=1"
label var highest_secondary "Highest Education Completed = High School"
label var highest_twoyear "Highest Education Completed = 2yr Degree"
label var highest_undergrad "Highest Education Completed = Undergrad Degree"
label var highest_postgrad "Highest Education Completed = Postgrad Degree"
label var secondary "Secondary School Teacher"


keep if nivel_ensenanza==3

gen all_teachers = 1

for X in var highest_secondary highest_twoyear highest_undergrad ///
	highest_postgrad mujer age_group_* : gen turnover_X = 0 if X == 1
for X in var highest_secondary highest_twoyear highest_undergrad ///
	highest_postgrad mujer age_group_* : replace turnover_X = 1 if X == 1 & turnover == 1
	
collapse (sum) all_teachers highest_secondary ///
	highest_twoyear highest_undergrad highest_postgrad mujer age_group_* ///
	turnover_all_teachers = turnover turnover_highest_secondary ///
	turnover_highest_twoyear turnover_highest_undergrad turnover_highest_postgrad ///
	turnover_mujer turnover_age_group_1 turnover_age_group_2 turnover_age_group_3 ///
	turnover_age_group_4 ///
	(mean) turnover pct_turnover_highest_secondary = turnover_highest_secondary ///
	pct_turnover_highest_twoyear = turnover_highest_twoyear ///
	pct_turnover_highest_undergrad = turnover_highest_undergrad ///
	pct_turnover_highest_postgrad = turnover_highest_postgrad ///
	pct_turnover_mujer = turnover_mujer pct_turnover_age_group_1 = turnover_age_group_1 ///
	pct_turnover_age_group_2 = turnover_age_group_2 ///
	pct_turnover_age_group_3 = turnover_age_group_3 ///
	pct_turnover_age_group_4 = turnover_age_group_4 , by(comuna YEAR)

label var all_teachers "Total Number of Teachers"
label var highest_secondary "Total Teachers: Highest Education Completed = High School"
label var highest_twoyear "Total Teachers: Highest Education Completed = 2yr Degree"
label var highest_undergrad "Total Teachers: Highest Education Completed = Undergrad Degree"
label var highest_postgrad "Total Teachers: Highest Education Completed = Postgrad Degree"	
label var mujer "Total Teachers: Sex = Female"	
label var age_group_1 "Total Teachers: Age < 30"	
label var age_group_2 "Total Teachers: Age = 30-45"	
label var age_group_3 "Total Teachers: Age = 46-60"	
label var age_group_4 "Total Teachers: Age = 61+"
label var turnover_all_teachers "Total Number of Teachers Who Left Their School"
label var turnover_highest_secondary "Total Teachers Who Left Their School: Highest Education Completed = High School"
label var turnover_highest_twoyear "Total Teachers Who Left Their School: Highest Education Completed = 2yr Degree"
label var turnover_highest_undergrad "Total Teachers Who Left Their School: Highest Education Completed = Undergrad Degree"
label var turnover_highest_postgrad "Total Teachers Who Left Their School: Highest Education Completed = Postgrad Degree"
label var turnover_mujer "Total Teachers Who Left Their School: Sex = Female"	
label var turnover_age_group_1 "Total Teachers Who Left Their School: Age < 30"
label var turnover_age_group_2 "Total Teachers Who Left Their School: Age = 30-45"
label var turnover_age_group_3 "Total Teachers Who Left Their School: Age = 46-60"
label var turnover_age_group_4 "Total Teachers Who Left Their School: Age = 61+"
label var turnover "Percentage of Teachers Who Left Their School"	
label var pct_turnover_highest_secondary "% of Teachers Who Left Their School: Highest Education Completed = High School"
label var pct_turnover_highest_twoyear "% of Teachers Who Left Their School: Highest Education Completed = 2yr Degree"
label var pct_turnover_highest_undergrad "% of Teachers Who Left Their School: Highest Education Completed = Undergrad Degree"
label var pct_turnover_highest_postgrad "% of Teachers Who Left Their School: Highest Education Completed = Postgrad Degree"
label var pct_turnover_mujer "% of Teachers Who Left Their School: Sex = Female"	
label var pct_turnover_age_group_1 "% of Teachers Who Left Their School: Age < 30"
label var pct_turnover_age_group_2 "% of Teachers Who Left Their School: Age = 30-45"
label var pct_turnover_age_group_3 "% of Teachers Who Left Their School: Age = 46-60"
label var pct_turnover_age_group_4 "% of Teachers Who Left Their School: Age = 61+"

rename (comuna YEAR) (COMUNA year)

drop if mi(COMUNA)

tempfile teachers
save "`teachers'" , replace


use "$path/_dta/ViolenceonEducation_ComunaLevel_FullDataset.dta" , clear
merge 1:1 COMUNA year using "`teachers'" , nogenerate

order female minority rural strata_* ESTU_EDAD_* shift_* , after(stnd_fisica)

save "$path/_dta/ViolenceonEducation_ComunaLevel_FullDataset.dta" , replace




*Merge in Comuna info to other datasets
use "$path/_dta/ViolenceOnEducation.dta" , clear
rename (YEAR comuna) (year COMUNA)

merge m:1 year COMUNA using  "$path/_dta/ViolenceonEducation_ComunaLevel_FullDataset.dta" , keepus(berna_comuna - homicide_rate population - population_70plus) nogenerate keep(3)

rename (year COMUNA) (YEAR comuna)
	
save "$path/_dta/ViolenceOnEducation.dta" , replace


use "$path/_dta/Teachers.dta" , clear
rename (YEAR NUMERO_COM) (year COMUNA)
destring COMUNA , replace

merge m:1 year COMUNA using  "$path/_dta/ViolenceonEducation_ComunaLevel_FullDataset.dta" , keepus(berna_comuna - homicide_rate population - population_70plus) nogenerate keep(3)

rename year YEAR

order YEAR COMUNA comuna DANE_sede berna_comuna - homicide_rate ///
	population - population_70plus

drop m1	
save "$path/_dta/Teachers.dta" , replace



use "$path/_dta/Dropout.dta" , clear
rename (YEAR comuna) (year COMUNA)

merge m:1 year COMUNA using  "$path/_dta/ViolenceonEducation_ComunaLevel_FullDataset.dta" , keepus(homicide_rate population - population_70plus) nogenerate keep(1 3)

rename (year COMUNA) (YEAR comuna)
	
save "$path/_dta/Dropout.dta" , replace