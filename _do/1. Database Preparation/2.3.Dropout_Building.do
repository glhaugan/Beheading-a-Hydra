********************************************************************************
*********************			Dropout		Database		********************
********************************************************************************

global cd2 "H:/4. Personal/Violence_Education/_dta"
cap mkdir "$cd2"

global cd1 "H:/4. Personal/Violence_Education/_dta/RawData"
cap mkdir "$cd1"
cap mkdir "$cd1/Schools"

cd "$cd1"
set more off




use "$cd1/Dropout.dta" , clear

drop Manslaughter_wk1 - Drug_Arrests_lead365 Homicide_q1 - mean_Robberies_2yrs


*We will need a variable that indicates survival time and one for turnover.
	*This is the time they are exposed to the risk of leaving the school.
	*We will simplify this as grade level.
label var grado "School Grade Level"

*The "turnover" variable (in our case, the decision not to study at the same school next year)
*turnover=1 if the student does not return the following year; =0 if they return or if the variable is censored.
drop if mi(idglobal20150903) // these seem to be school obs from a database merged in earlier that didn't match anything in the student database
sort idglobal20150903 DANE_sede YEAR
bys idglobal20150903 DANE_sede (YEAR): gen turnover = 1 if _n == _N & YEAR != 2014 & grado != 11	
bys idglobal20150903 DANE_sede (YEAR): replace turnover = 0 if _n != _N & YEAR != 2014

*Transfers - Didn't drop out, but went to a different school.
	*This is going to throw an error because transfer is generated twice.
	*On 3/29/23 I ran this and got the error message. I think the first transfer
	*is an old definition which was discovered to be wrong (because 
	*deserta2 doesn't seem correct). The second definition was created 
	*more recently, but I think when it was written maybe I left it so it would 
	*throw an error so that I would come back to it and review. 
	*Still need to double check this.
*gen transfer = 1 if turnover == 1 & deserta2 == 0
*replace transfer = 0 if turnover == 0 | (turnover == 1 & deserta2 == 1) 
sort idglobal20150903 YEAR
gen transfer = turnover
bys idglobal20150903 (YEAR) : replace transfer = 0 if _n == _N & turnover == 1

**Control Variables
*Minority
gen minority = (etnia != "0" & etnia != "NO APLICA" & etnia != "No Aplica") if etnia != "."
label var minority "Ethnic Minority"

*Gender
gen female = 1 if genero2 == 0
replace female = 0 if genero2 == 1
label var female "Female"

*Label outcomes
label var repite2 "Repeating Grade Level"
label var deserta2 "Dropout"
label var transfer "Transfer"
label var turnover "Dropout or Transfer"

**Comuna numbers
geoinpoly y x using "$cd1/Shapefiles/Comunas/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$cd1/Shapefiles/Comunas/comunadb.dta" , gen(comuna_merge) ///
	keepus(NUMERO_COM)
destring NUMERO_COM , gen(comuna)
drop comunaid

*Generate dummy for Don Berna Comunas	
gen berna_comuna = (comuna == 1 | comuna == 13 | comuna == 8 | comuna == 3 | ///
	comuna == 5 | comuna == 6)
label var berna_comuna "Berna-Controlled Neighborhood"	

**Demobilized population from Bloques Cacique Nutibara and Heroes de Granada living in each comuna
gen BernaStrength = 0 
*Add numbers from each Berna-controlled block and divide by total number of paramilitaries 
	*in the comuna, as reported in Alonso and Valencia article
replace BernaStrength = (176 + 245) / 527 if comuna == 1
replace BernaStrength = (21 + 68) / 179 if comuna == 13 
replace BernaStrength = (99 + 117) / 324 if comuna == 8 
replace BernaStrength = (97 + 275) / 490 if comuna == 3 
replace BernaStrength = (8 + 36) / 212 if comuna == 5 
replace BernaStrength = (46 + 51) / 248 if comuna == 6 
replace BernaStrength = (0 + 1) / 12 if comuna == 7
label var BernaStrength "Comuna-Level Measure of Berna Hegemony"

*Generate dummies for post-Berna time periods
gen post_transfer = (YEAR > 2007) // Berna Prison Transfer on August 17, 2007 (571 in Stata months)
label var post_transfer "Post-Prison Transfer Period"
gen post_extradition = (YEAR > 2008) // Berna Extradition on May 13 2008 (580 in Stata months)
label var post_extradition "Post-Extradition Period"

gen interaction_comuna_transfer = berna_comuna * post_transfer , ///
	after(post_extradition)
gen interaction_strength_transfer = BernaStrength * post_transfer , ///
	after(interaction_comuna_transfer)
label var interaction_comuna_transfer "Berna-Controlled Neighborhood = 1 $\times$ Post-Prison Transfer"
label var interaction_strength_transfer "BernaStrength $\times$ Post-Prison Transfer"

**Only schools that appear in all years, 2005-2014
bys DANE_sede YEAR : gen ones = _n == 1
bys DANE_sede : egen total = sum(ones)
keep if total == 10
drop ones total

*Drop Obs outside urban Medellin
drop if comuna == . & YEAR != 2014

merge m:1 COLE_CODIGO_COLEGIO YEAR using "$cd1/NearTables/Violence_All.dta" , ///
	keep(1 3) nogenerate
	
order idglobal20150903 YEAR DANE_sede dane_sede dane2 COLE_CODIGO_COLEGIO COLE_INST_NOMBRE	
	
save "$cd2/Dropout.dta" , replace