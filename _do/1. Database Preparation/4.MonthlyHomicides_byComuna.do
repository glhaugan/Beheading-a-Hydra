global path "H:/4. Personal/Violence_Education"


******	Create database with monthly totals, by comuna
use "$path/_dta/RawData/Medellin_crimes.dta" , clear
drop if delito == ""

*Format date
replace fecha = substr(fecha,1,length(fecha)-7)
gen Month = substr(fecha,4,.)
gen month = monthly(Month, "MY")
format month %tm
drop Month

*Identify the comuna where each incident took place
geoinpoly y x using "$path/_dta/RawData/Shapefiles/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$path/_dta/RawData/Shapefiles/comunadb.dta" , gen(comuna_merge) keepus(NUMERO_COM)
destring NUMERO_COM , gen(comuna)

*Generate dummy for Don Berna Comunas
gen berna_comuna = (comuna == 1 | comuna == 13 | comuna == 8 | comuna == 3 | comuna == 5 | comuna == 6)

*Generate dummies for post-Berna time periods
gen post_transfer = (month > 571) // Berna Prison Transfer on August 17, 2007 (571 in Stata months)
label var post_transfer "Post-Prison Transfer Period"
gen post_extradition = (month > 580) // Berna Extradition on May 13 2008 (580 in Stata months)
label var post_extradition "Post-Extradition Period"

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

*Encode crimes
encode delito , gen(crime)

*Get monthly crimes and reshape data
collapse (sum) total (mean) berna_comuna post_transfer post_extradition BernaStrength ///
	, by(comuna comunaid month mes_num year crime)
	
replace BernaStrength = round(BernaStrength , .000000001) // Required for Stata version 17. Originally programmed without this line while running an earlier version of Stata. With Stata 17, some of the BernaStrength values within comunas are off by a fraction of a fraction of a fraction of a decimal point due to the variable structure, which generates an error when the reshape on the next line is run.

reshape wide total , i(comuna comunaid mes_num year month berna_comuna ///
	post_transfer post_extradition) j(crime)

*Create clusters (by period and comuna)	
gen period = 1 if month < 571
replace period = 2 if inrange(month, 571 , 579)
replace period = 3 if month >= 580
egen cluster = group(comuna period)	
	
*Clean-up and labeling
order comuna comunaid mes_num year month berna_comuna BernaStrength post_transfer ///
	post_extradition BernaStrength total* cluster	

foreach var of varlist total*{
	replace `var' = 0 if mi(`var')
}	
	
rename (total1 total2 total3 total4 total5 total6 total7 total8 total9 mes_num) ///
	(hom_culposo homicides car_theft moto_theft robberies burglaries les_personales_at ///
	les_personales drugs month_num)
label var hom_culposo "Homicidio Culposo (AT)"
label var homicides "Homicides"
label var car_theft "Car Thefts"
label var moto_theft "Motorcycle Thefts"
label var robberies "Personal Robberies"
label var burglaries "Home Burglaries"
label var les_personales_at "Lesiones Personales (AT)"
label var les_personales "Lesiones Personales"
label var drugs "Narcotics Trafficking or Fabrication"

label var comuna "Comuna Number"
label var comunaid "Comuna ID - From Shapefile"	
label var month_num "Month Number"	
label var year "Year"	
label var month "month-year"	
label var berna_comuna "Berna-Controlled Comuna"
label var post_transfer "Post-Prison Transfer Period"
label var post_extradition "Post-Extradition Period"
label var BernaStrength "Berna Degree of Hegemony in Comuna"

label var cluster "Comuna-Period Cluster"
	
sort comuna month

drop if mi(comuna)
	
save "$path/_dta/MonthlyHomicides_ByComuna.dta" , replace

*Merge in Population Data
import delimited using "$path/_dta/RawData/Medellin_Population.csv", varnames(1) clear
collapse (sum) hombres* mujeres* , ///
	by(codigodanemunicipio tipodivisiongeografica nombredivisiongeografica)
keep if tipodivisiongeografica == "Comuna"

replace nombredivisiongeografica = "La America" if nombredivisiongeografica == "La AmÃ©rica"
replace nombredivisiongeografica = "Belen" if nombredivisiongeografica == "BelÃ©n"

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

foreach year of numlist 1993 2005/2015{
	gen population`year' = hombres`year' + mujeres`year'
}
gen population2004 = population2005

keep nombredivisiongeografica comuna population*
rename nombredivisiongeografica comuna_name

tempfile pop
save "`pop'"

reshape long population , i(comuna_name comuna) j(year)
drop if inlist(year , 1993 , 2015)

merge 1:m comuna year using "$path/_dta/MonthlyHomicides_ByComuna.dta" , nogenerate

sort comuna year month_num
order comuna_name comuna comunaid year month_num month

gen homicides_per = (homicides / population) * 100000
label var homicides_per "Homicides per 100,000 Inhabitants"

save "$path/_dta/MonthlyHomicides_ByComuna.dta" , replace


********************************************************************************
***			Generating Vivienda de Interes Social Control Variable			 ***
********************************************************************************

preserve


import excel using /// read in VIS data
	"$path/_dta/RawData/Proyectos vivienda VIS_Medellín.xlsx", clear firstrow

*Convert dates
tostring fecha_inicio_venta fecha_inicio_construccion ///
	fecha_terminacion_construccion fecha_entrega , replace
foreach var of varlist fecha_inicio_venta fecha_inicio_construccion ///
	fecha_terminacion_construccion fecha_entrega{
		replace `var' = substr(`var' , 1 , 4) + "/" + substr(`var' , 5 , 2) + ///
			"/" + substr(`var' , 7 , 2)
		gen `var'2 = date(`var' , "YMD") , after(`var')
		format `var'2 %td	
	}

	
	
*Drop if construction began after 2014
drop if fecha_inicio_construccion2 >= 20089 // 20089 is Jan 1, 2015

drop if last_estado == "Cancelado" // Drop cancelled phases

*We now have a dataset with 555 observations. All but six have values for fecha_terminacion_construccion.
sort idproyecto idetapa idtipo
order idproyecto idetapa idtipo nombre_proyecto nombre_etapa nombre_tipo tipo_vivienda

*Discovered what some variables mean after looking at this a long time. 
*Label them so we don't forget
label var idproyecto "Construction Project ID"
label var idetapa "Building Phase ID"
// This is the only one I'm not sure what it means, but doesn't seem to matter 
// too much. We have Tipo-level database with some proyecto- or etapa-level variables
// and the tipo-level variables seem to add up to equal the proyect- or etapa-level ones.
label var idtipo "Tipo ID"
label var activo "Project Status"
label var numunidades "Number of Units in Building Phase"
label var last_estado "Last Status of Phase"
replace last_estado = "Terminado Vendido y Entregado" if last_estado == "TVE"
replace last_estado = "Terminado y Entregado " if last_estado == "TE"
label var last_fase "Last Construction Phase"
label var unidades_por_tipo "Number of Units in Tipo"

*Format GPS
tostring longitud latitud , replace
replace longitud = substr(longitud , 1 , 3) + "." + substr(longitud , 4 , .)
replace latitud = substr(latitud , 1 , 1) + "." + substr(latitud , 2 , .)
destring longitud latitud , replace

*Let's keep only the few variables we need
keep idproyecto idetapa idtipo nombre_proyecto nombre_etapa nombre_tipo ///
	tipo_vivienda longitud latitud regional ciudad zona barrio ///
	direccion_proyecto direccion_descriptiva fecha_terminacion_construccion2 ///
	fecha_terminacion_construccion unidades_por_tipo

*Get Comunas from Shapefile	
geoinpoly latitud longitud using "$path/_dta/RawData/Shapefiles/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$path/_dta/RawData/Shapefiles/comunadb.dta" , gen(comuna_merge) keepus(NUMERO_COM NOMBRE_COM)
destring NUMERO_COM , gen(comuna)
// use comuna from VIS database for those not in comuna shape (mostly these are rural areas, but a couple look like they are on the outskirts of comunas)
replace NOMBRE_COM = zona if mi(NOMBRE_COM) 
	
*Collapse to comuna-month level	
collapse (sum) unidades_por_tipo , by(fecha_terminacion_construccion NOMBRE_COM)
sort NOMBRE_COM fecha_terminacion_construccion
replace NOMBRE_COM = proper(NOMBRE_COM) // prepare for merge
replace fecha_terminacion_construccion = ///
	substr(fecha_terminacion_construccion , 1 , 7)
gen month = monthly(fecha_terminacion_construccion , "YM") , ///
	after(fecha_terminacion_construccion)
format month %tm
drop if month == . | month > 658
rename NOMBRE_COM comuna_name
replace comuna_name = "Doce de Octubre" if comuna_name == "Doce De Octubre"
replace comuna_name = "Laureles - Estadio" if comuna_name == "Laureles-Estadio"
drop if inlist(comuna_name , "San Antonio De Prado" , "San Cristobal")
replace month = 528 if month < 528

tempfile temp
save "`temp'" , replace

restore

*Merge VIS data with Monthly Homicides data
merge 1:1 comuna_name month using "`temp'" , keepus(unidades_por_tipo) nogenerate
rename unidades_por_tipo vis_unidades
	
*missing means no VIS constructed that month	
replace vis_unidades = 0 if vis_unidades == . 
bys comuna (month) : gen cum12month=vis_unidades[1] // generate running total
bys comuna (month) : replace cum12month=vis_unidades[_n]+ cum12month[_n-1] if _n>1
replace vis_unidades = cum12month
label var vis_unidades "Number of Active Units of VIS or VIP Housing"	
drop cum12month

save "$path/_dta/MonthlyHomicides_ByComuna.dta" , replace
