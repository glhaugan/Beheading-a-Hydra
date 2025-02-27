# Beheading a Hydra: Kingpin Extradition, Homicides, Education Outcomes, and the End of Medellin’s Pax Mafiosa
#### Highlighted Skills: Causal inference, statistical programming in R and Stata, data cleaning/wrangling, data visualization, geospatial analysis

Some highlights from my work on a long-term project studying how the extradition of a major crime lord impacted gang dynamics in Medellin, Colombia. 


### Data

- **Comunas**: Shapefiles for Medellin's comuna administrative units.
- **Schools.csv**: Data on Medellin's public schools for 2004-2013. Schools are identified by the DANE_sede school id variable. MATEMATICAS_punt is a variable containing the raw average score for 11th grade students on an annual standardized exam. _Note: Due to privacy concerns, I cannot share the raw individual-level data used in the paper. This dataset collapses test scores to the school-level and removes all other information about students._ 
- **Medellin_crimes.csv**: Data on reported crimes in Medellin for 2004-2013. Schools are identified by the DANE_sede school id variable. MATEMATICAS_punt is a variable containing the raw average score for 11th grade students on an annual standardized exam. Includes month (mes) and year of occurrence.

### Scripts
- **Analysis.R**: R code for cleaning, analysis, and output. _Note: I originally programmed my work in Stata, but translated this bit of the work into R to demonstrate my R skills for this portfolio. It does not reproduce all results in the paper._
- **_do**: Folder with the original Stata do files for all work for this project. _Note: This code will not run since I cannot share the individual-level datasets it uses._


### Output
The analysis script in R produces the following output.

- **MedellinMap.html**: Interactive map showing schools' homicide exposure in 2007 and 2010.
- **plot.png**: Trendline plot for monthly homicides in Berna-controlled and non-controlled comunas.

