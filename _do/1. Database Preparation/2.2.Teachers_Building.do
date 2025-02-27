********************************************************************************
*********************			Teacher		Database		********************
********************************************************************************

global cd2 "H:/4. Personal/Violence_Education/_dta"
cap mkdir "$cd2"

global cd1 "H:/4. Personal/Violence_Education/_dta/RawData"
cap mkdir "$cd1"
cap mkdir "$cd1/Schools"

cd "$cd1"
set more off




use "$cd2/ToMerge.dta" , clear // ToMerge.dta is created in Student_Building.do

sort DANE_sede COLE_CODIGO_COLEGIO
bys DANE_sede: keep if _n == 1
rename DANE_sede codigo_dane_ee
save "$cd2/ToMerge2.dta" , replace

use "$cd1/docentes_2008-2013.dta" , clear

**The database uses outdated DANE codes for some schools. Update.
replace codigo_dane_ee = 105001025771 if codigo_dane_ee == 105001017400

merge m:1 codigo_dane_ee using "$cd2/ToMerge2.dta"

drop if (codigo_dane_ee == . )| (_merge != 3 & muni_carga != 5001)
drop fecha_amenazado amenazado _merge
rename codigo_dane_ee DANE_sede
rename anno_carga YEAR
sort COLE_CODIGO_COLEGIO YEAR

merge m:1 COLE_CODIGO_COLEGIO YEAR using "$cd1/NearTables/Violence_All.dta" , gen (m1)
*We only have teacher data starting 2008
keep if YEAR >= 2008
*Drop observations from Violence_All.dta that weren´t matched. These are schools with multiple jornadas, so they have one DANE_sede and multiple COLE_CODIGO_COLEGIO
drop if m1 == 2
*These are odd observations - special curriculum schools, not found in ICFES database for some reason,
*only very few observations in the entire database, or rural schools with extremely difficult addresses.
*most have a combination of the above circumstances.
drop if DANE_sede == 0 | DANE_sede == 105001002348 | DANE_sede == 105001002372 | ///
DANE_sede == 105001003395 | DANE_sede == 105001014869 | DANE_sede == 105001026131 | ///
DANE_sede == 205001001389 | DANE_sede == 205001001451 | DANE_sede == 205001001931 | ///
DANE_sede == 205001001940 | DANE_sede == 205001001966 | DANE_sede == 205001002245 | ///
DANE_sede == 205001002768 | DANE_sede == 205001003195 | DANE_sede == 205001003462 | ///
DANE_sede == 205001003322 | DANE_sede == 205001006119 | DANE_sede == 205001006216 | ///
DANE_sede == 205001006224 | DANE_sede == 205001007611 | DANE_sede == 205001008090 | ///
DANE_sede == 205001008391 | DANE_sede == 205001008405 | DANE_sede == 205001008464 | ///
DANE_sede == 205001009681 | DANE_sede == 205001008561 | DANE_sede == 205001009916 | ///
DANE_sede == 205001010248 | DANE_sede == 205001010272 | DANE_sede == 205001010841 | ///
DANE_sede == 205001011384 | DANE_sede == 205001011651 | DANE_sede == 205001011961 | ///
DANE_sede == 205001014189 | DANE_sede == 205001014308 | DANE_sede == 205001019253 | ///
DANE_sede == 205001019989 | DANE_sede == 205001020006 | DANE_sede == 205001021452 | ///
DANE_sede == 205001022360


*Get comuna numbers
geoinpoly y x using "$cd1/Shapefiles/Comunas/comunacoord.dta"
rename _ID comunaid
merge m:1 comunaid using "$cd1/Shapefiles/Comunas/comunadb.dta" , nogenerate ///
	keepus(NUMERO_COM) keep(3)
destring NUMERO_COM , gen(comuna)
drop comunaid

order YEAR NUMERO_COM comuna DANE_sede


save "$cd2/Teachers.dta" , replace
erase "$cd2/ToMerge.dta"
erase "$cd2/ToMerge2.dta"