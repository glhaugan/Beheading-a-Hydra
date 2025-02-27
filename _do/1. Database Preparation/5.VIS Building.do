********************************************************************************
***			Generating Vivienda de Interes Social Control Variable			 ***
********************************************************************************


global path "H:/4. Personal/Violence_Education"
cd "$path"

set more off



*Comuna-Level VIS
use "$path/_dta/MonthlyHomicides_ByComuna.dta" , clear

keep if (year == 2004 & month_num == 10) | /// *2004 - October 2 Exam Date 
	(year == 2005 & month_num == 10) | /// *2005 - October 9 Exam Date 
	(year == 2006 & month_num == 9) | /// *2006 - Sept 24 Exam Date 
	(year == 2007 & month_num == 9) | /// *2007 - Sept 23 Exam Date 
	(year == 2008 & month_num == 9) | /// *2008 - Sept 21 Exam Date 
	(year == 2009 & month_num == 9) | /// *2009 - Sept 13 Exam Date 
	(year == 2010 & month_num == 9) | /// *2010 - Sept 12 Exam Date 
	(year == 2011 & month_num == 9) | /// *2011 - Sept 4 Exam Date 
	(year == 2012 & month_num == 9) | /// *2012 - Sept 2 Exam Date 
	(year == 2013 & month_num == 8) | /// *2013 - Aug 25 Exam Date 
	(year == 2014 & month_num == 8) // * 2014 - Aug 3 Exam Date
keep comuna_name comuna year vis_unidades 

label var vis_unidades "Number of Active Units of VIS or VIP Housing in Comuna on Exam Day"

rename (year vis_unidades) (YEAR vis_unidades_comuna)

tempfile temp
save "`temp'" , replace


*Merge in Comuna info to other datasets
use "$path/_dta/ViolenceOnEducation.dta" , clear
merge m:1 YEAR comuna using "`temp'" , nogenerate keep(1 3)
order comuna_name , after(comuna)
order vis_unidades_comuna , after(population_70plus)
save "$path/_dta/ViolenceOnEducation.dta" , replace


use "$path/_dta/Teachers.dta" , clear
merge m:1 YEAR comuna using "`temp'" , nogenerate keep(1 3)
order comuna_name , after(comuna)
order vis_unidades_comuna , after(population_70plus)
save "$path/_dta/Teachers.dta" , replace


use "$path/_dta/Dropout.dta" , clear
merge m:1 YEAR comuna using "`temp'" , nogenerate keep(1 3)
order comuna_name , after(comuna)
order vis_unidades_comuna , after(population_70plus)
save "$path/_dta/Dropout.dta" , replace



*School-Level VIS
import excel using "$path/_dta/RawData/Proyectos vivienda VIS_Medellín.xlsx", clear firstrow

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

	
	
*Drop if began after 2014
drop if fecha_inicio_construccion2 >= 20089 // 20089 is Jan 1, 2015

*Drop cancelled phases
drop if last_estado == "Cancelado"

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
	tipo_vivienda longitud latitud zona barrio ///
	direccion_proyecto fecha_terminacion_construccion2 ///
	fecha_terminacion_construccion unidades_por_tipo

drop if fecha_terminacion_construccion2 == .	
	
preserve	
capture geonear idtipo latitud longitud using ///
	"$path/_dta/RawData/NearTables/ForTableGeneration/Schools_All.dta" ,  ///
			n(COLE_CODIGO_COLEGIO y x ) within(0.5) long near(0)
			
rename km_to_COLE_CODIGO_COLEGIO km_to_			
reshape wide km_to_ , i(idtipo) j(COLE_CODIGO_COLEGIO)			

tempfile temp
save "`temp'" , replace
restore

merge 1:1 idtipo using "`temp'"	

for X in var km_to_* : replace X = unidades_por_tipo if !mi(X)

collapse (sum) km_to_* , ///
	by(fecha_terminacion_construccion fecha_terminacion_construccion2)
reshape long km_to_ , ///
	i(fecha_terminacion_construccion fecha_terminacion_construccion2) ///
	j(COLE_CODIGO_COLEGIO)


sort COLE_CODIGO_COLEGIO fecha_terminacion_construccion2
replace fecha_terminacion_construccion = ///
	substr(fecha_terminacion_construccion , 1 , 7)
gen year = ///
	substr(fecha_terminacion_construccion , 1 , 4)
gen month = ///
	substr(fecha_terminacion_construccion , 6 , 2)	
gen year_month = monthly(fecha_terminacion_construccion , "YM") , ///
	after(fecha_terminacion_construccion)
format year_month %tm
drop if year_month == . | year_month >= 656

gen YEAR = 2004 if year_month < 537 // *2004 - October 2 Exam Date
replace YEAR = 2005 if inrange(year_month , 538 , 549) // *2005 - October 9 Exam Date 
replace YEAR = 2006	if inrange(year_month , 550 , 560) // *2006 - Sept 24 Exam Date  
replace YEAR = 2007	if inrange(year_month , 561 , 572) // *2007 - Sept 23 Exam Date 
replace YEAR = 2008	if inrange(year_month , 573 , 584) // *2008 - Sept 21 Exam Date 
replace YEAR = 2009	if inrange(year_month , 585 , 596) // *2009 - Sept 13 Exam Date 
replace YEAR = 2010	if inrange(year_month , 597 , 608) // *2010 - Sept 12 Exam Date 
replace YEAR = 2011	if inrange(year_month , 609 , 620) // *2011 - Sept 4 Exam Date 
replace YEAR = 2012	if inrange(year_month , 621 , 632) // *2012 - Sept 2 Exam Date 
replace YEAR = 2013	if inrange(year_month , 633 , 643) // *2013 - Aug 25 Exam Date 
replace YEAR = 2014	if inrange(year_month , 644 , 655) // * 2014 - Aug 3 Exam Date

keep COLE_CODIGO_COLEGIO km_to_ YEAR
collapse (sum) km_to_ , by(COLE_CODIGO_COLEGIO YEAR)
reshape wide km_to_ , i(YEAR) j(COLE_CODIGO_COLEGIO)
set obs 11
replace YEAR = 2005 in 9
replace YEAR = 2006 in 10
replace YEAR = 2007 in 11
sort YEAR
for X in var km_to_* : replace X = 0 if X == . & inrange(YEAR , 2005 , 2007)

reshape long km_to_ , i(YEAR) j(COLE_CODIGO_COLEGIO)
bys COLE_CODIGO_COLEGIO (YEAR) : gen vis_unidades_school = km_to_[1]
bys COLE_CODIGO_COLEGIO (YEAR) : replace vis_unidades_school=km_to_[_n]+ vis_unidades_school[_n-1] if _n>1
label var vis_unidades_school "Number of Active Units of VIS or VIP Housing within 500m of school on Exam Day"

drop km_to_

sort COLE_CODIGO_COLEGIO YEAR
tempfile temp
save "`temp'" , replace


*Merge in Comuna info to other datasets
use "$path/_dta/ViolenceOnEducation.dta" , clear
sort COLE_CODIGO_COLEGIO YEAR
merge m:1 YEAR COLE_CODIGO_COLEGIO using "`temp'" , nogenerate keep(1 3) ///
	keepus(vis_unidades_school)
replace vis_unidades_school = 0 if vis_unidades_school == .
save "$path/_dta/ViolenceOnEducation.dta" , replace


use "$path/_dta/Teachers.dta" , clear
sort COLE_CODIGO_COLEGIO YEAR
merge m:1 YEAR COLE_CODIGO_COLEGIO using "`temp'" , nogenerate keep(1 3) ///
	keepus(vis_unidades_school)
replace vis_unidades_school = 0 if vis_unidades_school == .
order vis_unidades_school , after(vis_unidades_comuna)
save "$path/_dta/Teachers.dta" , replace


use "$path/_dta/Dropout.dta" , clear
sort COLE_CODIGO_COLEGIO YEAR
merge m:1 COLE_CODIGO_COLEGIO YEAR using "`temp'" , nogenerate keep(1 3)
replace vis_unidades_school = 0 if vis_unidades_school == .
order vis_unidades_school , after(vis_unidades_comuna)
save "$path/_dta/Dropout.dta" , replace