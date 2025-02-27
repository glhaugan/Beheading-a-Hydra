 *******************************************************************************
*Create 2004-2014 Sem. 2 Saber 11 Database of Public School Students in Medellin*
 *******************************************************************************

global cd2 "H:/4. Personal/Violence_Education/_dta"
cap mkdir "$cd2"

global cd1 "H:/4. Personal/Violence_Education/_dta/RawData"
cap mkdir "$cd1"
cap mkdir "$cd1/Schools"

cd "$cd1"
set more off

***************				2006			************************************
clear
import excel "$cd1/Schools/SB11-20062-RGSTRO-CLFCCN-V1-0.xlsx" , firstrow

gen YEAR = 2006

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ESTU_ZONA_RESIDE ///
ESTU_EXAM_COD_MPIOPRESENTACION COLE_CODIGO_COLEGIO CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO ///
COLE_GENERO_POBLACION COLE_NATURALEZA COLE_INST_JORNADA PLAN_CODIGODANEINSTITUCION COLE_CARACTER_COLEGIO ///
ESTU_ETNIA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ESTU_HORAS_TRABAJO COLE_INST_VLR_PENSION ///
LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ESTU_PUESTO ECON_AREA_VIVE

format %14.0g CODIGO_DANE

*We only want Medellin public schools. Drop others.
gen public = 1 if COLE_NATURALEZA == "O"
replace public = 0 if COLE_NATURALEZA == "N"
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public == 1
drop really_public public COLE_NATURALEZA

for X in var LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT ///
	BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT : replace X = round(X , .01)	// this is not really changing anything except how the data is stored in Stata, and it facilitates a merge.

preserve
use "$cd1/Schools/greg2006.dta" , clear
keep if icfes_semestre == 2
bys icfes_estudiante_colegio_codigo : egen really_private = mean(icfes_colegio_origen)
keep if really_private == 0

gen ESTU_GENERO = "F" if icfes_estudiante_genero == 0
replace ESTU_GENERO = "M" if icfes_estudiante_genero == 1

rename (icfes_estudiante_colegio_codigo ///
	icfes_puntaje_lenguaje icfes_puntaje_matematicas icfes_puntaje_sociales ///
	icfes_puntaje_filosofia icfes_puntaje_biologia icfes_puntaje_quimica ///
	icfes_puntaje_fisica) ///
	(COLE_CODIGO_COLEGIO LENGUAJE_PUNT MATEMATICAS_PUNT ///
	CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT)

recast double LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT ///
	BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT
for X in var LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT ///
	BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT : replace X = round(X , .01)	
	
keep COLE_CODIGO_COLEGIO ESTU_GENERO LENGUAJE_PUNT MATEMATICAS_PUNT ///
	CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ///
	icfes_estudiante_estrato icfes_madre_educacion
tempfile 2006
save "`2006'"	
restore

merge 1:1 COLE_CODIGO_COLEGIO ESTU_GENERO LENGUAJE_PUNT MATEMATICAS_PUNT ///
	CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ///
	using "`2006'" , keep(1 3) nogenerate // merging on these variables b/c student IDs don't match. Excel dataset likely anonymized.

replace ESTU_ESTRATO = icfes_estudiante_estrato
replace FAMI_COD_EDUCA_MADRE = icfes_madre_educacion
	
save "$cd1/Schools/Medellin_Public_2006.dta" , replace


***************				2007			************************************

clear
import excel "$cd1/Schools/SB11-20072-RGSTRO-CLFCCN-V1-0.xlsx" , firstrow

gen YEAR = 2007

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ESTU_ZONA_RESIDE ///
ESTU_EXAM_COD_MPIOPRESENTACION COLE_CODIGO_COLEGIO CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO ///
COLE_GENERO_POBLACION COLE_NATURALEZA COLE_INST_JORNADA PLAN_CODIGODANEINSTITUCION COLE_CARACTER_COLEGIO ///
ESTU_ETNIA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ESTU_HORAS_TRABAJO COLE_INST_VLR_PENSION ///
LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ESTU_PUESTO ECON_AREA_VIVE 

format %14.0g CODIGO_DANE

*We only want Medellin public schools. Drop others.
gen public = 1 if COLE_NATURALEZA == "O"
replace public = 0 if COLE_NATURALEZA == "N"
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public == 1
drop really_public public


for X in var LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT ///
	BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT : replace X = round(X , .01)	// this is not really changing anything except how the data is stored in Stata, and it facilitates a merge.

preserve
use "$cd1/Schools/greg2007.dta" , clear
keep if icfes_semestre == 2
bys icfes_estudiante_colegio_codigo : egen really_private = mean(icfes_colegio_origen)
keep if really_private == 0

gen ESTU_GENERO = "F" if icfes_estudiante_genero == 0
replace ESTU_GENERO = "M" if icfes_estudiante_genero == 1

rename (icfes_estudiante_colegio_codigo ///
	icfes_puntaje_lenguaje icfes_puntaje_matematicas icfes_puntaje_sociales ///
	icfes_puntaje_filosofia icfes_puntaje_biologia icfes_puntaje_quimica ///
	icfes_puntaje_fisica) ///
	(COLE_CODIGO_COLEGIO LENGUAJE_PUNT MATEMATICAS_PUNT ///
	CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT)

recast double LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT ///
	BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT
for X in var LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT ///
	BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT : replace X = round(X , .01)	
	
keep COLE_CODIGO_COLEGIO ESTU_GENERO LENGUAJE_PUNT MATEMATICAS_PUNT ///
	CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ///
	icfes_estudiante_estrato icfes_madre_educacion icfes_estudiante_edad
tempfile 2007
save "`2007'"	
restore

merge m:1 COLE_CODIGO_COLEGIO ESTU_GENERO LENGUAJE_PUNT MATEMATICAS_PUNT ///
	CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ///
	using "`2007'" , keep(1 3) nogenerate // merging on these variables b/c student IDs don't match. Excel dataset likely anonymized.

replace ESTU_ESTRATO = icfes_estudiante_estrato
replace FAMI_COD_EDUCA_MADRE = icfes_madre_educacion


save "$cd1/Schools/Medellin_Public_2007.dta" , replace


***************				2008			************************************

clear
import excel "$cd1/Schools/SB11-20082-RGSTRO-CLFCCN-V1-0.xlsx" , firstrow

gen YEAR = 2008

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ESTU_ZONA_RESIDE ///
ESTU_EXAM_COD_MPIOPRESENTACION COLE_CODIGO_COLEGIO CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO ///
COLE_GENERO_POBLACION COLE_NATURALEZA COLE_INST_JORNADA PLAN_CODIGODANEINSTITUCION COLE_CARACTER_COLEGIO ///
ESTU_ETNIA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ESTU_HORAS_TRABAJO COLE_INST_VLR_PENSION ECON_SN_INTERNET ///
LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ESTU_PUESTO ECON_AREA_VIVE 

format %14.0g CODIGO_DANE

*We only want Medellin public schools. Drop others.
gen public = 1 if COLE_NATURALEZA == "O"
replace public = 0 if COLE_NATURALEZA == "N"
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public == 1

drop really_public public

save "$cd1/Schools/Medellin_Public_2008.dta" , replace


***************				2009			************************************

clear
import excel "$cd1/Schools/SB11-20092-RGSTRO-CLFCCN-V1-0.xlsx" , firstrow

gen YEAR = 2009

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ESTU_ZONA_RESIDE ///
ESTU_EXAM_COD_MPIOPRESENTACION COLE_CODIGO_COLEGIO CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO ///
COLE_GENERO_POBLACION COLE_NATURALEZA COLE_INST_JORNADA PLAN_CODIGODANEINSTITUCION COLE_CARACTER_COLEGIO ///
ESTU_ETNIA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ESTU_HORAS_TRABAJO COLE_INST_VLR_PENSION ECON_SN_INTERNET ///
LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ESTU_PUESTO ECON_AREA_VIVE 

format %14.0g CODIGO_DANE

*We only want Medellin public schools. Drop others.
gen public = 1 if COLE_NATURALEZA == "O"
replace public = 0 if COLE_NATURALEZA == "N"
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public == 1

drop public really_public

save "$cd1/Schools/Medellin_Public_2009.dta" , replace



***************				2010			************************************

use "$cd1/Schools/SB 11 2010-2 (Resultados).dta" , clear

gen YEAR = 2010

gen ESTU_EDAD = 2010 - ESTU_NACIMIENTO_ANNO if ESTU_NACIMIENTO_MES <= 9
replace ESTU_EDAD = (2010 - ESTU_NACIMIENTO_ANNO) - 1 if ESTU_NACIMIENTO_MES > 9

rename ESTU_CONSECUTIVO FTP_CONSECUTIVO
rename COLE_CODIGO_INST COLE_CODIGO_COLEGIO

local temas "LENGUAJE MATEMATICA CIENCIAS_SOCIALES FILOSOFIA BIOLOGIA QU*MICA FISICA"
foreach tema of local temas{
rename TEMA_`tema' `tema'_PUNT
}
rename TEMA_PUESTO ESTU_PUESTO
rename MATEMATICA_PUNT MATEMATICAS_PUNT
rename QU*MICA_PUNT QUIMICA_PUNT

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ECON_AREA_VIVE COLE_CODIGO_MCPIO ///
COLE_CODIGO_COLEGIO COLE_INST_NOMBRE COLE_INST_JORNADA ESTU_ETNIA FAMI_COD_EDUCA_PADRE ///
FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ///
COLE_INST_VLR_PENSION ECON_SN_INTERNET LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT ///
FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ESTU_PUESTO ECON_AREA_VIVE 

foreach var of varlist ESTU_CODIGO_RESIDE_MCPIO-COLE_CODIGO_COLEGIO ESTU_ETNIA-ECON_SN_INTERNET {
destring `var' , replace
}

foreach var of varlist LENGUAJE_PUNT-ESTU_PUESTO {
destring `var' , replace dpcomma
}

*We only want Medellin public schools. Drop others.
keep if COLE_CODIGO_MCPIO == 5001
gen public = 1 if COLE_INST_VLR_PENSION == 0
replace public = 0 if COLE_INST_VLR_PENSION != 0
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public > .5

drop public really_public

save "$cd1/Schools/Medellin_Public_2010.dta" , replace



***************				2011			************************************

use "$cd1/Schools/SB 11 2011-2 (Resultados).dta" , clear

gen YEAR = 2011

gen ESTU_EDAD = 2011 - ESTU_NACIMIENTO_ANNO if ESTU_NACIMIENTO_MES <= 9
replace ESTU_EDAD = (2011 - ESTU_NACIMIENTO_ANNO) - 1 if ESTU_NACIMIENTO_MES > 9

rename ESTU_CONSECUTIVO FTP_CONSECUTIVO

local temas "LENGUAJE MATEMATICA CIENCIAS_SOCIALES FILOSOFIA BIOLOGIA QU*MICA FISICA"
foreach tema of local temas{
rename TEMA_`tema' `tema'_PUNT
}
rename TEMA_PUESTO ESTU_PUESTO
rename MATEMATICA_PUNT MATEMATICAS_PUNT
rename QU*MICA_PUNT QUIMICA_PUNT

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ECON_AREA_VIVE COLE_CODIGO_MCPIO ///
COLE_CODIGO_COLEGIO COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO COLE_INST_JORNADA COLE_CARACTER_COLEGIO ///
ESTU_ETNIA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ESTU_HORAS_TRABAJO COLE_INST_VLR_PENSION ECON_SN_INTERNET ///
LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ESTU_PUESTO ECON_AREA_VIVE 

foreach var of varlist COLE_CODIGO_MCPIO COLE_CODIGO_COLEGIO ESTU_ETNIA-COLE_INST_VLR_PENSION{
destring `var' , replace
}

foreach var of varlist LENGUAJE_PUNT-ESTU_PUESTO {
destring `var' , replace dpcomma
}

*We only want Medellin public schools. Drop others.
keep if COLE_CODIGO_MCPIO == 5001
gen public = 1 if COLE_INST_VLR_PENSION == 0
replace public = 0 if COLE_INST_VLR_PENSION != 0
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public > .5

drop public really_public

save "$cd1/Schools/Medellin_Public_2011.dta" , replace


***************				2012			************************************

use "$cd1/Schools/SB 11 2012-2 (Resultados).dta" , clear

gen YEAR = 2012

gen ESTU_EDAD = 2012 - ESTU_NACIMIENTO_ANNO if ESTU_NACIMIENTO_MES <= 9
replace ESTU_EDAD = (2012 - ESTU_NACIMIENTO_ANNO) - 1 if ESTU_NACIMIENTO_MES > 9

rename ESTU_CONSECUTIVO FTP_CONSECUTIVO

local temas "LENGUAJE MATEMATICA CIENCIAS_SOCIALES FILOSOFIA BIOLOGIA QUIMICA FISICA"
foreach tema of local temas{
rename TEMA_`tema' `tema'_PUNT
}
rename TEMA_PUESTO ESTU_PUESTO
rename MATEMATICA_PUNT MATEMATICAS_PUNT

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ECON_AREA_VIVE COLE_CODIGO_MCPIO ///
COLE_CODIGO_COLEGIO COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO COLE_INST_JORNADA COLE_CARACTER_COLEGIO ///
ESTU_ETNIA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ESTU_HORAS_TRABAJO COLE_INST_VLR_PENSION ECON_SN_INTERNET ///
LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ESTU_PUESTO ECON_AREA_VIVE 

*We only want Medellin public schools. Drop others.
keep if COLE_CODIGO_MCPIO == 5001
gen public = 1 if COLE_INST_VLR_PENSION == 0
replace public = 0 if COLE_INST_VLR_PENSION != 0
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public > .5

drop public really_public

save "$cd1/Schools/Medellin_Public_2012.dta" , replace




***************				2013			************************************

use "$cd1/Schools/sb11 2013-20132-cruda-nonames.dta" , clear

keep if periodo == 20132
drop periodo

*Variable names upper case to be consistent with other years
foreach var of varlist _all {
	local new_var=upper("`var'")
	rename `var' `new_var'
}

gen YEAR = 2013

rename ESTU_CONSECUTIVO FTP_CONSECUTIVO

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ESTU_ZONA_RESIDE ECON_AREA_VIVE ///
ESTU_EXAM_COD_MPIOPRESENTACION COLE_CODIGO_COLEGIO CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO ///
COLE_GENERO_POBLACION COLE_NATURALEZA COLE_INST_JORNADA COLE_CARACTER_COLEGIO ///
ESTU_ETNIA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ESTU_HORAS_TRABAJO COLE_INST_VLR_PENSION ECON_SN_INTERNET ///
LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT ESTU_PUESTO 

*We only want Medellin public schools. Drop others.
keep if ESTU_CODIGO_RESIDE_MCPIO == 5001
gen public = 1 if COLE_NATURALEZA == "O"
replace public = 0 if COLE_NATURALEZA == "N"
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public == 1

drop public really_public

save "$cd1/Schools/Medellin_Public_2013.dta" , replace



***************				2014			************************************

use "$cd1/Schools/greg2014.dta" , clear

keep if periodo == 20142 // keep only 2nd semester
drop periodo

*Variable names upper case to be consistent with other years
foreach var of varlist _all {
	local new_var=upper("`var'")
	rename `var' `new_var'
}

gen YEAR = 2014

rename (ESTU_CONSECUTIVO ESTU_FECHANACIMIENTO ESTU_COD_RESIDE_MCPIO ///
	COLE_CODIGO_ICFES ///
	COLE_COD_DANE_SEDE COLE_NOMBRE_SEDE COLE_CALENDARIO COLE_GENERO COLE_JORNADA ///
	COLE_CARACTER FAMI_EDUCACIONPADRE FAMI_EDUCACIONMADRE FAMI_OCUPACIONPADRE ///
	FAMI_OCUPACIONMADRE FAMI_ESTRATOVIVIENDA FAMI_NIVELSISBEN FAMI_TIENECOMPUTADOR ///
	FAMI_INGRESOFMILIARMENSUAL ESTU_TRABAJAACTUALMENTE ESTU_VALORPENSIONCOLEGIO ///
	FAMI_TIENEINTERNET PUNT_LECTURA_CRITICA PUNT_MATEMATICAS ///
		PUNT_C_NATURALES PUNT_SOCIALES_CIUDADANAS) ///
		///
	(FTP_CONSECUTIVO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO COLE_CODIGO_COLEGIO ///
	CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO COLE_GENERO_POBLACION ///
	COLE_INST_JORNADA COLE_CARACTER_COLEGIO FAMI_COD_EDUCA_PADRE2014 ///
	FAMI_COD_EDUCA_MADRE2014 FAMI_COD_OCUP_PADRE2014 FAMI_COD_OCUP_MADRE2014 ///
	ESTU_ESTRATO ///
	FAMI_NIVEL_SISBEN2014 ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL2014 ///
	ESTU_TRABAJA2014 ///
	COLE_INST_VLR_PENSION2014 ECON_SN_INTERNET LECTURA_CRITICA_PUNT MATEMATICAS_PUNT ///
	C_NATURALES_PUNT SOCIALES_CIUDADANAS_PUNT)

*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ///
COLE_CODIGO_COLEGIO CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO ///
COLE_GENERO_POBLACION COLE_NATURALEZA COLE_INST_JORNADA COLE_CARACTER_COLEGIO ///
FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ///
ESTU_ESTRATO FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ///
ESTU_TRABAJA2014 COLE_INST_VLR_PENSION2014 ECON_SN_INTERNET LECTURA_CRITICA_PUNT  ///
MATEMATICAS_PUNT C_NATURALES_PUNT SOCIALES_CIUDADANAS_PUNT 

*We only want Medellin public schools. Drop others.
keep if ESTU_CODIGO_RESIDE_MCPIO == 5001
gen public = 1 if COLE_NATURALEZA == "OFICIAL"
replace public = 0 if COLE_NATURALEZA == "NO OFICIAL"
bys COLE_CODIGO_COLEGIO : egen really_public = mean(public)
keep if really_public == 1

drop public really_public

*Convert DOB to age
gen fecha = date(ESTU_EDAD , "DMY")
replace fecha = floor((19938 - fecha) / 365.25)
drop ESTU_EDAD
rename fecha ESTU_EDAD

*Make same as other years
replace COLE_GENERO_POBLACION = "M" if COLE_GENERO_POBLACION == "FEMENINO"
replace COLE_GENERO_POBLACION = "X" if COLE_GENERO_POBLACION == "MIXTO"
replace COLE_NATURALEZA = "O" if COLE_NATURALEZA == "OFICIAL"
replace COLE_INST_JORNADA = "Afternoon" if COLE_INST_JORNADA == "TARDE"
replace COLE_INST_JORNADA = "Full Day" if COLE_INST_JORNADA == "COMPLETA"
replace COLE_INST_JORNADA = "Morning" if COLE_INST_JORNADA == "MAÃANA"
replace COLE_INST_JORNADA = "Night" if COLE_INST_JORNADA == "NOCHE"
replace COLE_INST_JORNADA = "Weekend" if COLE_INST_JORNADA == "SABATINA"
replace COLE_CARACTER_COLEGIO = "ACADEMICO" if COLE_CARACTER_COLEGIO == "ACADÃMICO"
replace COLE_CARACTER_COLEGIO = "ACADEMICO Y TECNICO" if ///
	COLE_CARACTER_COLEGIO == "TÃCNICO/ACADÃMICO" 
replace COLE_CARACTER_COLEGIO = "TECNICO" if COLE_CARACTER_COLEGIO == "TÃCNICO"
replace ESTU_ESTRATO = subinstr(ESTU_ESTRATO , "Estrato " , "" , .)
destring ESTU_ESTRATO , replace
replace ECON_SN_COMPUTADOR = "0" if ECON_SN_COMPUTADOR == "No"
replace ECON_SN_COMPUTADOR = "1" if ECON_SN_COMPUTADOR == "Si"
destring ECON_SN_COMPUTADOR, replace
replace ECON_SN_INTERNET = "0" if ECON_SN_INTERNET == "No"
replace ECON_SN_INTERNET = "1" if ECON_SN_INTERNET == "Si"
destring ECON_SN_INTERNET, replace

save "$cd1/Schools/Medellin_Public_2014.dta" , replace


***********		Combining	Years	Into	One		Dataset			************

use "$cd1/Schools/Medellin_Public_2006.dta" , clear

append using "$cd1/Schools/Medellin_Public_2007.dta" ///
	"$cd1/Schools/Medellin_Public_2008.dta" "$cd1/Schools/Medellin_Public_2009.dta" /// 
	"$cd1/Schools/Medellin_Public_2010.dta" "$cd1/Schools/Medellin_Public_2011.dta" ///
	"$cd1/Schools/Medellin_Public_2012.dta" "$cd1/Schools/Medellin_Public_2013.dta" ///
	"$cd1/Schools/Medellin_Public_2014.dta" , force

***************			Add in 2004			************************************

*DB was originally created for 2006-2013. Then we decided to add in 2004 and 2005.

preserve
bys CODIGO_DANE : keep if _n == 1

tempfile studentdb
save "`studentdb'" , replace

use "$cd1/Schools/2004.dta" , clear

gen YEAR = 2004

*Missing gender for some students. Impute from name
replace icfes_estudiante_genero = 0 if (mi( icfes_estudiante_genero ) & ///
	upper(substr(icfes_estudiante_nombre , -1 , 1)) == "A") & ///
	icfes_estudiante_nombre != "JUAN CAMILO GARCIA ZAPATA"
replace icfes_estudiante_genero = 1 if ((mi( icfes_estudiante_genero ) & ///
	upper(substr(icfes_estudiante_nombre , -1 , 1)) == "O") | ///
	icfes_estudiante_nombre == "JUAN CAMILO GARCIA ZAPATA") & ///
	icfes_estudiante_nombre != "MONCADA RODRIGUEZ NATALY DEL SOCORRO"	
replace icfes_estudiante_genero = 1 if ((mi( icfes_estudiante_genero ) & ///
	upper(substr(icfes_estudiante_nombre , -1 , 1)) == "N")) & ///
	!inlist(icfes_estudiante_nombre , "ARRIETA GALINDO DURLEY KATRIN" , ///
	"GUZMAN PEREZ CINTHIA YLEN" , "JIMENEZ BETANCUR LESSLY DAJAN" , ///
	"URREA HENAO KAREN" , "CORDOBA VALENCIA KAREN" , ///
	"PEREZ RODELO OBEDIS DEL CARMEN" , "GARCIA GIRALDO MARIA BELEN")
replace icfes_estudiante_genero = 0 if mi(icfes_estudiante_genero) & ///
	inlist(icfes_estudiante_nombre , "ARRIETA GALINDO DURLEY KATRIN" , ///
	"GUZMAN PEREZ CINTHIA YLEN" , "JIMENEZ BETANCUR LESSLY DAJAN" , ///
	"URREA HENAO KAREN" , "CORDOBA VALENCIA KAREN" , ///
	"PEREZ RODELO OBEDIS DEL CARMEN" , "GARCIA GIRALDO MARIA BELEN")
replace icfes_estudiante_genero = 1 if (mi( icfes_estudiante_genero ) & ///
	(strpos(upper(icfes_estudiante_nombre) , "DAVID") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "FELIPE") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "ANDRES") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "JUAN") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "CARLOS") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "ALEXANDER")))
replace icfes_estudiante_genero = 0 if (mi( icfes_estudiante_genero ) & ///
	(strpos(upper(icfes_estudiante_nombre) , "JULIET") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "YULIET") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "KATH") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "KATE") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "MARIA") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "NATAL") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "JANE") > 0 |  ///
	strpos(upper(icfes_estudiante_nombre) , "JENI") > 0 | ///
	strpos(upper(icfes_estudiante_nombre) , "JENNI") > 0))	
replace icfes_estudiante_genero = 0 if mi( icfes_estudiante_genero ) & ///
	upper(substr(icfes_estudiante_nombre , -2 , 2)) == "TH" & ///
	icfes_estudiante_nombre != "PEREZ RODAS BRIAN KEITH"
replace icfes_estudiante_genero = 1 if ///
	icfes_estudiante_nombre == "PEREZ RODAS BRIAN KEITH"
replace icfes_estudiante_genero = 1 if mi(icfes_estudiante_genero) & ///
	(strpos(upper(icfes_estudiante_nombre) , "LUCAS") | ///
	strpos(upper(icfes_estudiante_nombre) , "JAMES") | ///
	strpos(upper(icfes_estudiante_nombre) , "OSCAR") | ///
	strpos(upper(icfes_estudiante_nombre) , "JONATHAN")  | ///
	strpos(upper(icfes_estudiante_nombre) , "ENRIQUE") | ///
	strpos(upper(icfes_estudiante_nombre) , "CRISTIAN") | ///
	strpos(upper(icfes_estudiante_nombre) , "JOSE") | ///
	strpos(upper(icfes_estudiante_nombre) , "JONNY") | ///
	strpos(upper(icfes_estudiante_nombre) , "JOHNNY") | ///
	strpos(upper(icfes_estudiante_nombre) , "JAVIER") | ///
	strpos(upper(icfes_estudiante_nombre) , "JHON") | ///
	strpos(upper(icfes_estudiante_nombre) , "ANDERSON") | ///
	strpos(upper(icfes_estudiante_nombre) , "JOHAN") | ///
	strpos(upper(icfes_estudiante_nombre) , "EDWIN") | ///
	strpos(upper(icfes_estudiante_nombre) , "JULIO") | ///
	strpos(upper(icfes_estudiante_nombre) , "CESAR") | ///
	strpos(upper(icfes_estudiante_nombre) , "GARCIA PALACIOS D AVID") | ///
	strpos(upper(icfes_estudiante_nombre) , "HAMINTON") | ///
	strpos(upper(icfes_estudiante_nombre) , "WILFER") | ///
	strpos(upper(icfes_estudiante_nombre) , "DANIEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "YEFERSON") | ///
	strpos(upper(icfes_estudiante_nombre) , "YIESON") | ///
	strpos(upper(icfes_estudiante_nombre) , "YOVANI") | ///
	strpos(upper(icfes_estudiante_nombre) , "FERNEY") | ///
	strpos(upper(icfes_estudiante_nombre) , "JAIDER") | ///
	strpos(upper(icfes_estudiante_nombre) , "YOHNNY") | ///		
	strpos(upper(icfes_estudiante_nombre) , "JEISON") | ///
	strpos(upper(icfes_estudiante_nombre) , "GABRIEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "HUBER") | ///
	strpos(upper(icfes_estudiante_nombre) , "JORGE") | ///
	strpos(upper(icfes_estudiante_nombre) , "MIGUEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "HERNAN") | ///
	strpos(upper(icfes_estudiante_nombre) , "ANGEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "JAIME") | ///
	strpos(upper(icfes_estudiante_nombre) , "OWEN") | ///
	strpos(upper(icfes_estudiante_nombre) , "HANS") | ///
	strpos(upper(icfes_estudiante_nombre) , "EMMANUEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "CHRISTIAN") | ///
	strpos(upper(icfes_estudiante_nombre) , "MANUEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "JAIBER") | ///
	strpos(upper(icfes_estudiante_nombre) , "JEHISON") | ///
	strpos(upper(icfes_estudiante_nombre) , "WBADIER") | ///
	strpos(upper(icfes_estudiante_nombre) , "JEFFERSON") | ///
	strpos(upper(icfes_estudiante_nombre) , "STEVENS") | ///
	strpos(upper(icfes_estudiante_nombre) , "SAMIR") | ///
	strpos(upper(icfes_estudiante_nombre) , "EDUARD") | ///		
	strpos(upper(icfes_estudiante_nombre) , "JESUS") | ///
	strpos(upper(icfes_estudiante_nombre) , "EXNEIDER") | ///
	strpos(upper(icfes_estudiante_nombre) , "ROGER") | ///
	strpos(upper(icfes_estudiante_nombre) , "FRANCO") | ///
	strpos(upper(icfes_estudiante_nombre) , "GIOVANNY") | ///
	strpos(upper(icfes_estudiante_nombre) , "ARMIN") | ///
	strpos(upper(icfes_estudiante_nombre) , "FRANK") | ///
	strpos(upper(icfes_estudiante_nombre) , "YAIR") | ///
	strpos(upper(icfes_estudiante_nombre) , "YHON") | ///
	strpos(upper(icfes_estudiante_nombre) , "ALDEMAR") | ///
	strpos(upper(icfes_estudiante_nombre) , "EDIBER")) 
replace icfes_estudiante_genero = 0 if mi(icfes_estudiante_genero) & ///
	(strpos(upper(icfes_estudiante_nombre) , "JULIA") | ///
	strpos(upper(icfes_estudiante_nombre) , "LINA") | ///
	strpos(upper(icfes_estudiante_nombre) , "MARILYN") | ///
	strpos(upper(icfes_estudiante_nombre) , "DIANA")  | ///
	strpos(upper(icfes_estudiante_nombre) , "ASTRID") | ///
	strpos(upper(icfes_estudiante_nombre) , "LUZ") | ///
	strpos(upper(icfes_estudiante_nombre) , "YULY") | ///
	strpos(upper(icfes_estudiante_nombre) , "SANDRA") | ///
	strpos(upper(icfes_estudiante_nombre) , "KARLA") | ///
	strpos(upper(icfes_estudiante_nombre) , "LISSETTE") | ///
	strpos(upper(icfes_estudiante_nombre) , "LEDY") | ///
	strpos(upper(icfes_estudiante_nombre) , "ESTEFANI") | ///
	strpos(upper(icfes_estudiante_nombre) , "LEIDY") | ///
	strpos(upper(icfes_estudiante_nombre) , "HEIDY") | ///
	strpos(upper(icfes_estudiante_nombre) , "ERIKA") | ///
	strpos(upper(icfes_estudiante_nombre) , "CATERINE") | ///
	strpos(upper(icfes_estudiante_nombre) , "MARISOL") | ///
	strpos(upper(icfes_estudiante_nombre) , "SURANY") | ///
	strpos(upper(icfes_estudiante_nombre) , "EUNICE") | ///
	strpos(upper(icfes_estudiante_nombre) , "ELIZABET") | ///
	strpos(upper(icfes_estudiante_nombre) , "YENNIFER") | ///
	strpos(upper(icfes_estudiante_nombre) , "LILIANA")	| ///
	strpos(upper(icfes_estudiante_nombre) , "ISABEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "ESTEFANY") | ///
	strpos(upper(icfes_estudiante_nombre) , "MIRIAM") | ///
	strpos(upper(icfes_estudiante_nombre) , "GRACE") | ///
	strpos(upper(icfes_estudiante_nombre) , "AUDREY") | ///
	strpos(upper(icfes_estudiante_nombre) , "PAULA") | ///
	strpos(upper(icfes_estudiante_nombre) , "MICHELLE") | ///
	strpos(upper(icfes_estudiante_nombre) , "CINDY") | ///
	strpos(upper(icfes_estudiante_nombre) , "STEPHANIE") | ///
	strpos(upper(icfes_estudiante_nombre) , "MARIBEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "NANCY") | ///		
	strpos(upper(icfes_estudiante_nombre) , "LICET") | ///
	strpos(upper(icfes_estudiante_nombre) , "YURANI") | ///
	strpos(upper(icfes_estudiante_nombre) , "LADINAY") | ///
	strpos(upper(icfes_estudiante_nombre) , "MAGDA") | ///
	strpos(upper(icfes_estudiante_nombre) , "STEFANY") | ///
	strpos(upper(icfes_estudiante_nombre) , "MILDREY") | ///
	strpos(upper(icfes_estudiante_nombre) , "JENNYFER") | ///
	strpos(upper(icfes_estudiante_nombre) , "CONSUELO") | ///
	strpos(upper(icfes_estudiante_nombre) , "STEPHANIE") | ///
	strpos(upper(icfes_estudiante_nombre) , "MARIBEL") | ///
	strpos(upper(icfes_estudiante_nombre) , "NANCY") | ///
	strpos(upper(icfes_estudiante_nombre) , "YUCELLY") | ///
	strpos(upper(icfes_estudiante_nombre) , "ARELYS") | ///
	strpos(upper(icfes_estudiante_nombre) , "YAMILE") | ///
	strpos(upper(icfes_estudiante_nombre) , "YENNY") | ///
	strpos(upper(icfes_estudiante_nombre) , "MAIRENA") | ///
	strpos(upper(icfes_estudiante_nombre) , "MIRANDA") | ///
	strpos(upper(icfes_estudiante_nombre) , "STHEPHANIE") | ///
	strpos(upper(icfes_estudiante_nombre) , "DENIS SURLEY"))	
	
	
*re-do variables to match format of datasets in other years
rename icfes_snp FTP_CONSECUTIVO
gen ESTU_GENERO = "F" if icfes_estudiante_genero == 0
replace ESTU_GENERO = "M" if icfes_estudiante_genero == 1
gen ESTU_EDAD = icfes_estudiante_edad
rename (cod_dane codigodane icfes_colegio_nombre) ///
	(ESTU_CODIGO_RESIDE_MCPIO CODIGO_DANE COLE_INST_NOMBRE)
decode icfes_estudiante_colegio_calenda	, gen(COLE_CALENDARIO_COLEGIO)
gen COLE_CODIGO_COLEGIO = icfes_estudiante_colegio_codigo
gen COLE_NATURALEZA = "O" if icfes_colegio_origen == 0
replace COLE_NATURALEZA = "N" if icfes_colegio_origen == 1
gen COLE_INST_JORNADA = "COMPLETA U ORDINARIA" if jornada == "C"
replace COLE_INST_JORNADA = "MAÃ‘ANA" if jornada == "M"
replace COLE_INST_JORNADA = "NOCHE" if jornada == "N"
replace COLE_INST_JORNADA = "SABATINA - DOMINICAL" if jornada == "S"
replace COLE_INST_JORNADA = "TARDE" if jornada == "T"
rename (icfes_padre_educacion icfes_padre_ocupacion) (FAMI_COD_EDUCA_PADRE ///
	FAMI_COD_OCUP_PADRE)
rename (icfes_madre_educacion icfes_madre_ocupacion) (FAMI_COD_EDUCA_MADRE ///
	FAMI_COD_OCUP_MADRE)
rename (icfes_estudiante_estrato icfes_nivsis icfes_familia_ingreso ///
	icfes_estudiante_trabaja icfes_estudiante_pension) ///
	(ESTU_ESTRATO FAMI_NIVEL_SISBEN FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ///
	COLE_INST_VLR_PENSION)	
rename (icfes_puntaje_lenguaje icfes_puntaje_matematicas ///
	icfes_sociales_ciudadanas icfes_puntaje_filosofia icfes_puntaje_biologia /// 
	icfes_puntaje_quimica icfes_puntaje_fisica) ///
	(LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT ///
	BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT)	
	
keep if  icfes_semestre == 2	
	
*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ///
	COLE_CODIGO_COLEGIO CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO ///
	COLE_NATURALEZA COLE_INST_JORNADA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE ///
	FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
	FAMI_NIVEL_SISBEN FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ///
	COLE_INST_VLR_PENSION LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT  ///
	FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT

format %14.0g CODIGO_DANE	
	
merge m:1 CODIGO_DANE using "`studentdb'" , keepus(CODIGO_DANE) keep(3)	// We have checked to make sure public schools are found in the DBs for later years
	
*We only want Medellin public schools. Drop others.
keep if ESTU_CODIGO_RESIDE_MCPIO == 5001 & COLE_NATURALEZA == "O"

save "$cd1/Schools/Medellin_Public_2004.dta" , replace
	
restore	
	
	
***************			Add in 2005			************************************

*DB was originally created for 2006-2013. Then we decided to add in 2004 and 2005.

preserve
bys CODIGO_DANE : keep if _n == 1

tempfile studentdb
save "`studentdb'" , replace

use "$cd1/Schools/2005.dta" , clear

gen YEAR = 2005

*re-do variables to match format of datasets in other years
rename icfes_snp FTP_CONSECUTIVO
gen ESTU_GENERO = "F" if icfes_estudiante_genero == 0
replace ESTU_GENERO = "M" if icfes_estudiante_genero == 1
gen ESTU_EDAD = icfes_estudiante_edad
rename (cod_dane codigodane icfes_colegio_nombre) ///
	(ESTU_CODIGO_RESIDE_MCPIO CODIGO_DANE COLE_INST_NOMBRE)
decode icfes_estudiante_colegio_calenda	, gen(COLE_CALENDARIO_COLEGIO)
gen COLE_CODIGO_COLEGIO = icfes_estudiante_colegio_codigo
gen COLE_NATURALEZA = "O" if icfes_colegio_origen == 0
replace COLE_NATURALEZA = "N" if icfes_colegio_origen == 1
gen COLE_INST_JORNADA = "COMPLETA U ORDINARIA" if jornada == "C"
replace COLE_INST_JORNADA = "MAÃ‘ANA" if jornada == "M"
replace COLE_INST_JORNADA = "NOCHE" if jornada == "N"
replace COLE_INST_JORNADA = "SABATINA - DOMINICAL" if jornada == "S"
replace COLE_INST_JORNADA = "TARDE" if jornada == "T"
rename (icfes_padre_educacion icfes_padre_ocupacion) (FAMI_COD_EDUCA_PADRE ///
	FAMI_COD_OCUP_PADRE)
rename (icfes_madre_educacion icfes_madre_ocupacion) (FAMI_COD_EDUCA_MADRE ///
	FAMI_COD_OCUP_MADRE)
rename (icfes_estudiante_estrato icfes_nivsis icfes_familia_ingreso ///
	icfes_estudiante_trabaja icfes_estudiante_pension) ///
	(ESTU_ESTRATO FAMI_NIVEL_SISBEN FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ///
	COLE_INST_VLR_PENSION)	
rename (icfes_puntaje_lenguaje icfes_puntaje_matematicas ///
	icfes_sociales_ciudadanas icfes_puntaje_filosofia icfes_puntaje_biologia /// 
	icfes_puntaje_quimica icfes_puntaje_fisica) ///
	(LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT FILOSOFIA_PUNT ///
	BIOLOGIA_PUNT QUIMICA_PUNT FISICA_PUNT)	
	
keep if  icfes_semestre == 2	
	
*Dropping variables we don't need
keep FTP_CONSECUTIVO YEAR ESTU_GENERO ESTU_EDAD ESTU_CODIGO_RESIDE_MCPIO ///
	COLE_CODIGO_COLEGIO CODIGO_DANE COLE_INST_NOMBRE COLE_CALENDARIO_COLEGIO ///
	COLE_NATURALEZA COLE_INST_JORNADA FAMI_COD_EDUCA_PADRE FAMI_COD_EDUCA_MADRE ///
	FAMI_COD_OCUP_PADRE FAMI_COD_OCUP_MADRE ESTU_ESTRATO ///
	FAMI_NIVEL_SISBEN FAMI_ING_FMILIAR_MENSUAL ESTU_TRABAJA ///
	COLE_INST_VLR_PENSION LENGUAJE_PUNT MATEMATICAS_PUNT CIENCIAS_SOCIALES_PUNT  ///
	FILOSOFIA_PUNT BIOLOGIA_PUNT QUIMICA_PUNT

	
/*ESTU_ZONA_RESIDE ECON_AREA_VIVE ESTU_EXAM_COD_MPIOPRESENTACION COLE_GENERO_POBLACION ESTU_ETNIA FAMI_COD_EDUCA_PADRE FAMI_NIVEL_SISBEN ECON_SN_COMPUTADOR FAMI_ING_FMILIAR_MENSUAL ESTU_HORAS_TRABAJO ECON_SN_INTERNET COLE_CARACTER_COLEGIO*/
	
format %14.0g CODIGO_DANE	
	
merge m:1 CODIGO_DANE using "`studentdb'" , keepus(CODIGO_DANE) keep(3)	// We have checked to make sure public schools are found in the DBs for later years
	
*We only want Medellin public schools. Drop others.
keep if ESTU_CODIGO_RESIDE_MCPIO == 5001 & COLE_NATURALEZA == "O"

save "$cd1/Schools/Medellin_Public_2005.dta" , replace
	
restore		
	
	
	
append using "$cd1/Schools/Medellin_Public_2004.dta" ///
	"$cd1/Schools/Medellin_Public_2005.dta"
	
	
	
	
	

***********			Merging		Violence	Variables				************

merge m:1 COLE_CODIGO_COLEGIO YEAR using "$cd1/NearTables/Violence_All.dta" , gen (m1)

*Drop observations from Violence_All.dta that weren't matched. Probably just observations of schools before they were built, so there's no student data.
drop if m1 == 2

**These schools were not matched and are not really public (for 2010-2012 there's no 
** "public school" variable, so we used "free" schools as an imperfect substitute, which
** gives us some free schools that aren't public).
drop if COLE_CODIGO_COLEGIO == 638 | COLE_CODIGO_COLEGIO == 1164 | COLE_CODIGO_COLEGIO == 124750 | ///
COLE_CODIGO_COLEGIO == 101808 | COLE_CODIGO_COLEGIO == 102020 | COLE_CODIGO_COLEGIO == 114330 | ///
COLE_CODIGO_COLEGIO == 106112 | COLE_CODIGO_COLEGIO == 37739 | COLE_CODIGO_COLEGIO == 56820 | ///
COLE_CODIGO_COLEGIO == 145383 | COLE_CODIGO_COLEGIO == 158576 | COLE_CODIGO_COLEGIO == 139436 | ///
COLE_CODIGO_COLEGIO == 113357 | COLE_CODIGO_COLEGIO == 101907 | COLE_CODIGO_COLEGIO == 95919 | ///
COLE_CODIGO_COLEGIO == 102012 | COLE_CODIGO_COLEGIO == 139428 | COLE_CODIGO_COLEGIO == 145259 | ///
COLE_CODIGO_COLEGIO == 145359 | COLE_CODIGO_COLEGIO == 139444 | COLE_CODIGO_COLEGIO == 145367 | ///
COLE_CODIGO_COLEGIO == 147587 | COLE_CODIGO_COLEGIO == 99648 | COLE_CODIGO_COLEGIO == 93930 | ///
COLE_CODIGO_COLEGIO == 104372 | COLE_CODIGO_COLEGIO == 127001 | COLE_CODIGO_COLEGIO == 82438 | ///
COLE_CODIGO_COLEGIO == 132068 | COLE_CODIGO_COLEGIO == 95802 | COLE_CODIGO_COLEGIO == 53488 | ///
COLE_CODIGO_COLEGIO == 113365 |  COLE_CODIGO_COLEGIO == 74997 | COLE_CODIGO_COLEGIO == 51136 | ///
COLE_CODIGO_COLEGIO == 127019 | COLE_CODIGO_COLEGIO == 147579 | COLE_CODIGO_COLEGIO == 82446 | ///
COLE_CODIGO_COLEGIO == 113340 | COLE_CODIGO_COLEGIO == 136580 | COLE_CODIGO_COLEGIO == 167841 | ///
COLE_CODIGO_COLEGIO == 45880 | COLE_CODIGO_COLEGIO == 80309 | COLE_CODIGO_COLEGIO == 105387 | ///
COLE_CODIGO_COLEGIO == 127027 | COLE_CODIGO_COLEGIO == 93963 | COLE_CODIGO_COLEGIO == 132605 | ///
COLE_CODIGO_COLEGIO == 135467 | COLE_CODIGO_COLEGIO == 135475 | COLE_CODIGO_COLEGIO == 134452 | ///
COLE_CODIGO_COLEGIO == 66498 | COLE_CODIGO_COLEGIO == 69641 | COLE_CODIGO_COLEGIO == 53512 | ///
COLE_CODIGO_COLEGIO == 164491 | COLE_CODIGO_COLEGIO == 114934 |  COLE_CODIGO_COLEGIO == 164509 | ///
COLE_CODIGO_COLEGIO == 128785 | COLE_CODIGO_COLEGIO == 143222 | COLE_CODIGO_COLEGIO == 156448 | ///
COLE_CODIGO_COLEGIO == 155770 | COLE_CODIGO_COLEGIO == 160507 | COLE_CODIGO_COLEGIO == 88666 | ///
COLE_CODIGO_COLEGIO == 93922 | COLE_CODIGO_COLEGIO == 38711 | COLE_CODIGO_COLEGIO == 104620 | ///
COLE_CODIGO_COLEGIO == 95992 | COLE_CODIGO_COLEGIO == 58503 | COLE_CODIGO_COLEGIO == 163253 | ///
COLE_CODIGO_COLEGIO == 156430 | COLE_CODIGO_COLEGIO == 82396 |  COLE_CODIGO_COLEGIO == 82487 | ///
COLE_CODIGO_COLEGIO == 136374 | COLE_CODIGO_COLEGIO == 104430 | COLE_CODIGO_COLEGIO == 79533 | ///
COLE_CODIGO_COLEGIO == 132076 | COLE_CODIGO_COLEGIO == 41988 | COLE_CODIGO_COLEGIO == 156422 | ///
COLE_CODIGO_COLEGIO == 130591 | COLE_CODIGO_COLEGIO == 106799 | COLE_CODIGO_COLEGIO == 157792 | ///
COLE_CODIGO_COLEGIO == 95935 | COLE_CODIGO_COLEGIO == 114330 | COLE_CODIGO_COLEGIO == 80317

**These schools were not matched and are public, but have very few observations in very few years 
**Most don't appear to actually be located in Medellin (but may have students from Medellin attending)
**In other cases, they are public but are not traditional schools (they're adult education programs, etc.)
bys COLE_CODIGO_COLEGIO: gen obs = _N
drop if obs < = 10
drop obs

drop if COLE_CODIGO_COLEGIO == 1206 | CODIGO_DANE == 105002000055 | CODIGO_DANE == 105031010432 | ///
CODIGO_DANE == 105034000740 | CODIGO_DANE == 105036000097 | COLE_CODIGO_COLEGIO == 101394 | COLE_CODIGO_COLEGIO == 1420 |  ///
COLE_CODIGO_COLEGIO == 24562 | COLE_CODIGO_COLEGIO ==27987 | COLE_CODIGO_COLEGIO == 27870 | COLE_CODIGO_COLEGIO == 2063 | ///
COLE_CODIGO_COLEGIO == 33324 | COLE_CODIGO_COLEGIO == 35949 | COLE_CODIGO_COLEGIO == 39982 | COLE_CODIGO_COLEGIO == 46037 | ///
COLE_CODIGO_COLEGIO == 67413 | COLE_CODIGO_COLEGIO == 113639 | COLE_CODIGO_COLEGIO == 66506 | COLE_CODIGO_COLEGIO == 85860 | ///
COLE_CODIGO_COLEGIO == 42127 | COLE_CODIGO_COLEGIO == 46946 |  COLE_CODIGO_COLEGIO == 117325 | COLE_CODIGO_COLEGIO == 57091 | ///
COLE_CODIGO_COLEGIO == 106914 | COLE_CODIGO_COLEGIO == 1909 | COLE_CODIGO_COLEGIO == 2071 | COLE_CODIGO_COLEGIO == 1883 | ///
COLE_CODIGO_COLEGIO == 36020 | COLE_CODIGO_COLEGIO == 2055 | COLE_CODIGO_COLEGIO == 95885 | COLE_CODIGO_COLEGIO == 1867 | ///
COLE_CODIGO_COLEGIO == 136408 | COLE_CODIGO_COLEGIO == 2469 | COLE_CODIGO_COLEGIO == 45997 | COLE_CODIGO_COLEGIO == 56887 

*Remaining unmatched observations are schools that had no crimes within the buffer for the year
tab m1
drop m1

**********************		Standardizing Test Scores		********************

*We use information available at http://www.icfesinteractivo.gov.co/historicos/
*for the national mean and standard deviation of each subject on the exam for 
*the second semester of each year.

merge m:1 YEAR using "$cd1/NationalAverages" , nogenerate

local icfes "LENGUAJE MATEMATICAS CIENCIAS_SOCIALES FILOSOFIA BIOLOGIA QUIMICA FISICA"

foreach tema of local icfes{
gen STND_`tema' = ((`tema'_PUNT - avg_`tema') / sd_`tema')
rename STND_`tema' , lower
}

order LECTURA_CRITICA_PUNT C_NATURALES_PUNT SOCIALES_CIUDADANAS_PUNT , ///
	after(FISICA_PUNT)

***************		 Identifying High- & Low- Crime Periods		****************


/*Robberies
egen Robberies_q1 = rowtotal(Auto_Theft_m1 - Auto_Theft_m3 Home_Burglaries_m1 - Home_Burglaries_m3)
egen Robberies_q2 = rowtotal(Auto_Theft_m4 - Auto_Theft_m6 Home_Burglaries_m4 - Home_Burglaries_m6)
egen Robberies_q3 = rowtotal(Auto_Theft_m7 - Auto_Theft_m9 Home_Burglaries_m7 - Home_Burglaries_m9)
egen Robberies_q4 = rowtotal(Auto_Theft_m10 - Auto_Theft_m12 Home_Burglaries_m10 - Home_Burglaries_m12)
egen Robberies_q5 = rowtotal(Auto_Theft_m13 - Auto_Theft_m15 Home_Burglaries_m13 - Home_Burglaries_m15)
egen Robberies_q6 = rowtotal(Auto_Theft_m16 - Auto_Theft_m18 Home_Burglaries_m16 - Home_Burglaries_m18)
egen Robberies_q7 = rowtotal(Auto_Theft_m19 - Auto_Theft_m21 Home_Burglaries_m19 - Home_Burglaries_m21)
egen Robberies_q8 = rowtotal(Auto_Theft_m22 - Auto_Theft_m24 Home_Burglaries_m22 - Home_Burglaries_m24)

egen Robberies_sem1 = rowtotal(Auto_Theft_m1 - Auto_Theft_m6 Home_Burglaries_m1 - Home_Burglaries_m6)
egen Robberies_sem2 = rowtotal(Auto_Theft_m7 - Auto_Theft_m12 Home_Burglaries_m7 - Home_Burglaries_m12)
egen Robberies_sem3 = rowtotal(Auto_Theft_m13 - Auto_Theft_m18 Home_Burglaries_m13 - Home_Burglaries_m18)
egen Robberies_sem4 = rowtotal(Auto_Theft_m19 - Auto_Theft_m24 Home_Burglaries_m19 - Home_Burglaries_m24)

egen Robberies_yr1 = rowtotal(Auto_Theft_m1 - Auto_Theft_m12 Home_Burglaries_m1 - Home_Burglaries_m12)
egen Robberies_yr2 = rowtotal(Auto_Theft_m13 - Auto_Theft_m24 Home_Burglaries_m13 - Home_Burglaries_m24) 
egen Robberies_2yrs = rowtotal(Auto_Theft_m1 - Auto_Theft_m24 Home_Burglaries_m1 - Home_Burglaries_m24)*/ 




**Generating average violence for each school in different periods
/*preserve

*Collapsing from student-level down to school-year
collapse Hom_250_yr1 - Drug_Arrests_lead365 Homicide_q1 - Robberies_2yrs, by(COLE_CODIGO_COLEGIO YEAR)
*Missings are actually zeros
foreach var of varlist Hom_250_yr1 - Robberies_2yrs{
replace `var' = 0 if `var' == .
}
*Collapsing down to school-level
collapse Hom_250_yr1 - Robberies_2yrs , by(COLE_CODIGO_COLEGIO)
keep COLE_CODIGO_COLEGIO Hom_250_yr1 - Robberies_2yrs

foreach var of varlist Hom_250_yr1 - Robberies_2yrs{
rename `var' mean_`var'
}

save "$cd1/NearTables/Violence_temp.dta" , replace
restore

merge m:1 COLE_CODIGO_COLEGIO using "$cd1/NearTables/Violence_temp.dta", nogenerate
erase "$cd1/NearTables/Violence_temp.dta"*/

save "$cd2/ViolenceOnEducation.dta" , replace


**Going to need x and y coordinates and DANE code for all schools in the complete database
use "$cd1/NearTables/ForTableGeneration/Schools_20062.dta" , clear

append using "$cd1/NearTables/ForTableGeneration/Schools_20072.dta" /// 
	"$cd1/NearTables/ForTableGeneration/Schools_20082.dta" ///
	"$cd1/NearTables/ForTableGeneration/Schools_20092.dta" ///
	"$cd1/NearTables/ForTableGeneration/Schools_20102.dta" ///
	"$cd1/NearTables/ForTableGeneration/Schools_20122.dta" ///
	"$cd1/NearTables/ForTableGeneration/Schools_20132.dta" , force

drop YEAR_SEMESTER
bys COLE_CODIGO_COLEGIO: keep if _n == 1

save "$cd2/ToMerge.dta" , replace

use "$cd2/ViolenceOnEducation.dta" , clear
drop _merge

merge m:1 COLE_CODIGO_COLEGIO using "$cd2/ToMerge.dta" , keep(3) nogenerate

*I checked to make sure the DANE codes in the schools database are the same as
*in the student ICFES database. Except for three schools, they are. For some years though,
*the student ICFES database doesn't have DANE codes, so we'll replace those missings.
replace CODIGO_DANE = DANE_sede if CODIGO_DANE == . & COLE_CODIGO_COLEGIO != 56796 /// 
& COLE_CODIGO_COLEGIO != 131540 & COLE_CODIGO_COLEGIO != 133678


*Convert Barrio shapefiles to dta
shp2dta using "$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014.shp" , ///
	database("$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_db.dta") ///
	coordinates("$cd1/Shapefiles/Barrios Reprojected/BarrioVereda_2014_coord.dta") ///
	replace genid(_ID)
*Get comuna numbers
geoinpoly y x using "$cd1/Shapefiles/Comunas/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$cd1/Shapefiles/Comunas/comunadb.dta" , gen(comuna_merge) ///
	keepus(NUMERO_COM)
destring NUMERO_COM , gen(comuna)
drop comunaid


***Control Variables
*Gender is string, make factor 
gen female = 1 if ESTU_GENERO == "F"
replace female = 0 if ESTU_GENERO == "M"
label var female "Female"

*Create reformatted rural dwelling variable
gen rural = 1 if ECON_AREA_VIVE == 2 & inrange(YEAR , 2008 , 2012)
replace rural = 0 if ECON_AREA_VIVE == 1 & inrange(YEAR , 2008 , 2012)
label var rural "Rural dwelling"

format %70s COLE_INST_NOMBRE

*Shift is string, make factor 
replace COLE_INST_JORNADA = "Morning" if strpos(COLE_INST_JORNADA , "M") == 1
replace COLE_INST_JORNADA = "Full Day" if COLE_INST_JORNADA == "C" | ///
	COLE_INST_JORNADA == "COMPLETA U ORDINARIA"
replace COLE_INST_JORNADA = "Afternoon" if COLE_INST_JORNADA == "T" | ///
	COLE_INST_JORNADA == "TARDE"
replace COLE_INST_JORNADA = "Night" if COLE_INST_JORNADA == "N" | ///
	COLE_INST_JORNADA == "N" | COLE_INST_JORNADA == "NOCHE"
replace COLE_INST_JORNADA = "Weekend" if COLE_INST_JORNADA == "S" | ///
	COLE_INST_JORNADA == "SABATINA - DOMINICAL"

*Shift
encode COLE_INST_JORNADA , gen(shift)
label var shift "Shift"

*Minority ethnic group
gen minority = 1 if (ESTU_ETNIA >=1 & ESTU_ETNIA <= 99) & ///
	!inlist(YEAR , 2004 , 2005 , 2014)
replace minority = 0 if ESTU_ETNIA == . & !inlist(YEAR , 2004 , 2005 , 2014)
label var minority "Ethnic Minority"

*Mother's and father's education. Data dictionary for 2013 shows different values for dropout status.
/* Code no longer correct for defining father's education, and we're not using this anyways.
gen father_dropout = 1 if FAMI_COD_EDUCA_PADRE < 12 & YEAR != 2013
replace father_dropout = 1 if FAMI_COD_EDUCA_PADRE < 5 & YEAR == 2013
replace father_dropout = 1 if inlist(FAMI_COD_EDUCA_PADRE2014 , "Ninguno" , "Primaria completa" , "Primaria incompleta") & YEAR == 2014
replace father_dropout = 0 if FAMI_COD_EDUCA_PADRE >= 12 & FAMI_COD_EDUCA_PADRE < 99 ///
	& YEAR != 2013
replace father_dropout = 0 if FAMI_COD_EDUCA_PADRE >= 5 & FAMI_COD_EDUCA_PADRE < 11 ///
	& YEAR == 2013
replace father_dropout = 0 if ///
	(strpos(FAMI_COD_EDUCA_PADRE2014 , "profesional") > 0 | ///
	FAMI_COD_EDUCA_PADRE2014 == "Postgrado" | ///
	strpos(FAMI_COD_EDUCA_PADRE2014 , "Secundaria") > 0 | ///
	strpos(FAMI_COD_EDUCA_PADRE2014 , "tecno") > 0) & YEAR == 2014
label var father_dropout "Father Highest Education is Primary or Less"*/

/*gen mother_dropout = 1 if FAMI_COD_EDUCA_MADRE < 12 & YEAR != 2013 & ///
	!inlist(YEAR , 2004 , 2005) //excluding 2004, 2005 b/c coded differently
replace mother_dropout = 1 if FAMI_COD_EDUCA_MADRE < 5 & YEAR == 2013
replace mother_dropout = 0 if FAMI_COD_EDUCA_MADRE >= 12 & FAMI_COD_EDUCA_MADRE < 99 ///
	& YEAR != 2013 & !inlist(YEAR , 2004 , 2005)
replace mother_dropout = 0 if FAMI_COD_EDUCA_MADRE >= 5 & FAMI_COD_EDUCA_MADRE < 11 ///
	& YEAR == 2013 & !inlist(YEAR , 2004 , 2005)
label var mother_dropout "Mother Low Education"*/
gen mother_dropout = 1 if FAMI_COD_EDUCA_MADRE < 11 & YEAR != 2013 & !inrange(YEAR , 2004 , 2007) 
replace mother_dropout = 1 if FAMI_COD_EDUCA_MADRE < 4 & YEAR == 2013
replace mother_dropout = 1 if inlist(FAMI_COD_EDUCA_MADRE2014 , "Ninguno" , "Primaria completa" , "Primaria incompleta") & YEAR == 2014
replace mother_dropout = 0 if FAMI_COD_EDUCA_MADRE >= 11 & FAMI_COD_EDUCA_MADRE < 99 ///
	& YEAR != 2013 & !inrange(YEAR , 2004 , 2007)
replace mother_dropout = 0 if FAMI_COD_EDUCA_MADRE >= 4 & FAMI_COD_EDUCA_MADRE < 11 ///
	& YEAR == 2013 & !inrange(YEAR , 2004 , 2007)
replace mother_dropout = 1 if FAMI_COD_EDUCA_MADRE == 1 & inrange(YEAR , 2004 , 2007)
replace mother_dropout = 0 if inrange(FAMI_COD_EDUCA_MADRE , 2 , 4) & inrange(YEAR , 2004 , 2007)
replace mother_dropout = 0 if ///
	(strpos(FAMI_COD_EDUCA_MADRE2014 , "profesional") > 0 | ///
	FAMI_COD_EDUCA_MADRE2014 == "Postgrado" | ///
	strpos(FAMI_COD_EDUCA_MADRE2014 , "Secundaria") > 0 | ///
	strpos(FAMI_COD_EDUCA_MADRE2014 , "tecno") > 0) & YEAR == 2014
label var mother_dropout "Mother Highest Education is Primary or Less"


*Strata
gen Strata = "Strata 1" if ESTU_ESTRATO == 1
replace Strata = "Strata 2" if ESTU_ESTRATO == 2 
replace Strata = "Strata 3" if ESTU_ESTRATO == 3  
replace Strata = "Strata 4" if ESTU_ESTRATO == 4  	
replace Strata = "Strata 5" if ESTU_ESTRATO == 5 
replace Strata = "Strata 6" if ESTU_ESTRATO == 6
replace Strata = "" if inlist(YEAR , 2006 , 2007)
encode Strata , gen(strata)
drop Strata
label var strata "Socioeconomic Strata"

*Age
label var ESTU_EDAD "Age"

*Dependent Variables
label var stnd_lenguaje "Language - Standardized Score"
label var stnd_matematicas "Math - Standardized Score" 
label var stnd_ciencias_sociales "Social Studies - Standardized Score"
label var stnd_filosofia "Philosophy - Standardized Score"
label var stnd_biologia "Biology - Standardized Score"
label var stnd_quimica "Chemistry - Standardized Score"
label var stnd_fisica "Physics - Standardized Score"
label var LENGUAJE_PUNT "Language - Raw Score"
label var MATEMATICAS_PUNT "Math - Raw Score"
label var CIENCIAS_SOCIALES_PUNT "Social Studies - Raw Score"
label var FILOSOFIA_PUNT "Philosophy - Raw Score"
label var BIOLOGIA_PUNT "Biology - Raw Score"
label var QUIMICA_PUNT "Chemistry - Raw Score"
label var FISICA_PUNT "Physics - Raw Score"

**Only students aged 16-19
keep if ESTU_EDAD >=16 & ESTU_EDAD <= 19

drop if mi(comuna)	
drop avg_* sd_* comuna_merge

order YEAR NUMERO_COM comuna COLE_INST_NOMBRE DANE_sede CODIGO_DANE ///
	COLE_CODIGO_COLEGIO female rural shift minority mother_dropout ///
	strata stnd_lenguaje stnd_matematicas stnd_ciencias_sociales stnd_filosofia ///
	stnd_biologia stnd_quimica stnd_fisica

save "$cd2/ViolenceOnEducation.dta" , replace