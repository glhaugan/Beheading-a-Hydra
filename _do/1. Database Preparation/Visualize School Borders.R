########################################################################
####          Visualizing School Borders and Homicides              ####
####                  Created by: Greg Haugan                       ####
####                          4/17/2023                             ####
########################################################################

#Using municipality level variables, our objective is to
#apply genetic matching to optimize balance.
packages <- c("tidyverse" , "dplyr" , "readxl" , "osmdata" , "sf" , "ggmap" ,  
              "RgoogleMaps" , "foreign" , "rgdal" , "ggplot2" , "lubridate" , 
              "data.table" , "ggrepel" , "sp" , "raster" , "geosphere", 
              "dismo" , "rgeos" , "blockTools" , "reshape2" , "maptools" , 
              "broom" , "RColorBrewer" , "haven" , "stringr" , "ggiraph" , 
              "mapview")

### Install packages if not already installed.
#install.packages(packages)
### Load packages
lapply(packages, require, character.only = TRUE)
rm(list=ls())

### Tell R where we are working (our working directory):
setwd("H:/Personal/Violence_Education")

### Read in School Locations
schools <- read_dta(paste(getwd() , 
          "/_dta/RawData/NearTables/ForTableGeneration/Schools_All.dta" , sep = "")) 
xy <- schools[,c(5,4)]
schools <- SpatialPointsDataFrame(coords = xy, data = schools,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#Draw different radius around schools
radius <- st_as_sf(schools)
radius25 <- st_buffer(radius, dist = 25)
radius50 <- st_buffer(radius, dist = 50)
radius100 <- st_buffer(radius, dist = 100)
radius250 <- st_buffer(radius, dist = 250)
radius500 <- st_buffer(radius, dist = 500)

#Read in Homicide data
homicides <- read_dta(paste(getwd() , 
                          "/_dta/RawData/Medellin_crimes.dta" , sep = "")) 
homicides <- homicides %>%
  subset(delito == "Homicidio Comun" & !is.na(y))
homicides2006 <- homicides %>%
  subset(year == 2006)

xy <- homicides2006[,c(2,3)]
homicides2006 <- SpatialPointsDataFrame(coords = xy, data = homicides2006,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
homicides2006 <- st_as_sf(homicides2006)
homicides2006 <- st_buffer(homicides2006, dist = 25)

school_radius_map <- mapview(radius25, layer.name = c("25m radius"), 
        map.types = c("Esri.WorldImagery", "OpenStreetMap")) + 
mapview(radius50, layer.name = c("50m radius"), 
        map.types = c("Esri.WorldImagery", "OpenStreetMap")) + 
mapview(radius100, layer.name = c("100m radius"), 
        map.types = c("Esri.WorldImagery", "OpenStreetMap")) + 
mapview(radius250, layer.name = c("250m radius"), 
          map.types = c("Esri.WorldImagery", "OpenStreetMap")) + 
mapview(radius500, layer.name = c("500m radius"), 
          map.types = c("Esri.WorldImagery", "OpenStreetMap")) +
mapview(homicides2006, layer.name = c("Homicides 2006"), 
          map.types = c("Esri.WorldImagery", "OpenStreetMap") , 
          color = 'red' , col.regions = 'red')  
mapshot(school_radius_map, url = "school_radius_map.html")
