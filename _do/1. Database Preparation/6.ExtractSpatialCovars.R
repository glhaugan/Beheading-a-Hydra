#####################################################################
####      R Script for Extracting Spatial Covariate Data         ####
####                  Created by: Greg Haugan                    ####
####                          3/14/2023                          ####
#####################################################################

#This program is to create a community-level dataset (i.e., at the level of vereda
#or centro poblado) using spatial data.

packages <- c("sp" , "raster" , "rgdal" , "geosphere", "dismo" , "rgeos" ,
              "blockTools" , "dplyr" , "reshape2" , "foreign" , "maptools" , 
              "broom" , "ggplot2" , "ggmap" , "RColorBrewer" , "haven" , 
              "stringr" , "ggiraph" , "tidyverse" , "sf" , "mapview" , 
              "data.table")

###Install packages if not already installed.
#install.packages(packages)
###Load packages
lapply(packages, require, character.only = TRUE)
rm(packages)
rm(list=ls())

###Tell R where we are working (our working directory):
setwd("H:/Personal/Violence_Education/_dta/RawData")

### Read in Comuna Shapefiles
comunas <- readOGR(dsn = paste(getwd(), "/Shapefiles/Barrios Reprojected" , 
                                   sep=""), 
                       layer="BarrioVereda_2014")
comunas <- subset(comunas, LIMITECOMU <= 16)
region <- gUnaryUnion(comunas, id = comunas@data$LIMITECOMU)
# extract zone Id's to make dataframe
Gid <- sapply(slot(region, "polygons"), function(x) slot(x, "ID"))

# Create dataframe with correct rownames
z.df <- data.frame( ID=1:length(region), row.names = Gid)  

# make Polygondataframe 
comunas <- SpatialPolygonsDataFrame(region, data=z.df)   


### Read in school GPS data
schools <- read_dta("H:/Personal/Violence_Education/_dta/RawData/NearTables/ForTableGeneration/Schools_All.dta")
schools <- st_as_sf(schools, coords = c("x", "y"), crs = crs(comunas)) %>%
    st_transform(crs = crs(comunas))

#create 500m buffers around each school
schools_circles <- st_buffer(schools, dist = 500)

#Read in raster data
extent_set_2004 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2004.tif") , extent(comunas)) 
extent_set_2005 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2005.tif") , extent(comunas)) 
extent_set_2006 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2006.tif") , extent(comunas)) 
extent_set_2007 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2007.tif") , extent(comunas)) 
extent_set_2008 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2008.tif") , extent(comunas)) 
extent_set_2009 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2009.tif") , extent(comunas)) 
extent_set_2010 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2010.tif") , extent(comunas)) 
extent_set_2011 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2011.tif") , extent(comunas)) 
extent_set_2013 <- crop(raster("H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/col_bsgmi_v0a_100m_2013.tif") , extent(comunas))


#Extract data for comunas
raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number

  temp <- raster::extract(extent_set_2004 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2004[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2004 <- lapply(comunas$ID , raster_fun)

raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number
  
  temp <- raster::extract(extent_set_2005 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2005[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2005 <- lapply(comunas$ID , raster_fun)


raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number
  
  temp <- raster::extract(extent_set_2006 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2006[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2006 <- lapply(comunas$ID , raster_fun)


raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number
  
  temp <- raster::extract(extent_set_2007 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2007[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2007 <- lapply(comunas$ID , raster_fun)


raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number
  
  temp <- raster::extract(extent_set_2008 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2008[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2008 <- lapply(comunas$ID , raster_fun)



raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number
  
  temp <- raster::extract(extent_set_2009 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2009[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2009 <- lapply(comunas$ID , raster_fun)



raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number
  
  temp <- raster::extract(extent_set_2010 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2010[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2010 <- lapply(comunas$ID , raster_fun)



raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number
  
  temp <- raster::extract(extent_set_2011 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2011[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2011 <- lapply(comunas$ID , raster_fun)



raster_fun <- function(i){ #for each comuna
  print(which(grepl(i, comunas$ID))) #print comuna number
  
  temp <- raster::extract(extent_set_2013 , #extract the raster values
                          subset(comunas, ID == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  comunas$extent_set_2013[comunas$ID == i] <- temp[1,2]
}
comunas$extent_set_2013 <- lapply(comunas$ID , raster_fun)




#Extract data for schools
raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2004 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2004[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2004 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)


raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2005 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2005[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2005 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)


raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2006 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2006[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2006 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)


raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2007 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2007[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2007 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)


raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2008 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2008[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2008 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)


raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2009 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2009[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2009 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)


raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2010 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2010[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2010 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)



raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2011 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2011[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2011 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)


raster_fun <- function(i){ #for each school
  print(which(grepl(i, schools_circles$COLE_CODIGO_COLEGIO))) #which loop number
  
  temp <- raster::extract(extent_set_2013 , #extract the raster values
                          subset(schools_circles, COLE_CODIGO_COLEGIO == i) , fun = mean , 
                          na.rm=TRUE , df=TRUE)
  schools$extent_set_2013[schools$COLE_CODIGO_COLEGIO == i] <- temp[1,2]
}
schools$extent_set_2013 <- lapply(schools$COLE_CODIGO_COLEGIO , raster_fun)

#Write datafile
schools = st_set_geometry(schools , NULL) # Remove geometry column
schools$extent_set_2004 <- as.numeric(as.character(schools$extent_set_2004)) #convert list to numeric
schools$extent_set_2005 <- as.numeric(as.character(schools$extent_set_2005))
schools$extent_set_2006 <- as.numeric(as.character(schools$extent_set_2006))
schools$extent_set_2007 <- as.numeric(as.character(schools$extent_set_2007))
schools$extent_set_2008 <- as.numeric(as.character(schools$extent_set_2008))
schools$extent_set_2009 <- as.numeric(as.character(schools$extent_set_2009))
schools$extent_set_2010 <- as.numeric(as.character(schools$extent_set_2010))
schools$extent_set_2011 <- as.numeric(as.character(schools$extent_set_2011))
schools$extent_set_2013 <- as.numeric(as.character(schools$extent_set_2013))
schools$extent_set_2012 = # impute values for 2012
  ((schools$extent_set_2013 - schools$extent_set_2011) / 2) + schools$extent_set_2011
write_dta(schools , "H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/schools_settlementextent.dta")

comunas = as.data.frame(comunas@data) # Remove geometry column
comunas$extent_set_2004 <- as.numeric(as.character(comunas$extent_set_2004)) #convert list to numeric
comunas$extent_set_2005 <- as.numeric(as.character(comunas$extent_set_2005))
comunas$extent_set_2006 <- as.numeric(as.character(comunas$extent_set_2006))
comunas$extent_set_2007 <- as.numeric(as.character(comunas$extent_set_2007))
comunas$extent_set_2008 <- as.numeric(as.character(comunas$extent_set_2008))
comunas$extent_set_2009 <- as.numeric(as.character(comunas$extent_set_2009))
comunas$extent_set_2010 <- as.numeric(as.character(comunas$extent_set_2010))
comunas$extent_set_2011 <- as.numeric(as.character(comunas$extent_set_2011))
comunas$extent_set_2013 <- as.numeric(as.character(comunas$extent_set_2013))
comunas$extent_set_2012 = # impute values for 2012
    ((comunas$extent_set_2013 - comunas$extent_set_2011) / 2) + comunas$extent_set_2011
write_dta(comunas , "H:/Personal/Violence_Education/_dta/RawData/SettlementGrowth/comunas_settlementextent.dta")
