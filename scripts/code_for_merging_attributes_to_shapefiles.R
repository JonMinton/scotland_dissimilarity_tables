# script to link 2 and 4 group ethnicity attribute data by 2011 datazones to shapefiles 


rm(list = ls())



require(spdep)
require(maptools)
require(rgdal)
require(CARBayes)
require(Rcpp)
require(MASS)

require(stringr)
require(plyr)
require(tidyr)
require(dplyr)


require(grid)
require(ggplot2)

require(shiny)


# Attribute data 

attributes_table <- read.csv("output_data/ethnicity_4group_categories_datazones_2001_and_2011.csv")

attributes_table_2001 <- attributes_table %>% filter(year == 2001)
attributes_table_2001 <- attributes_table_2001[!duplicated(attributes_table_2001$dz_2001),]

attributes_table_2011 <- attributes_table %>% filter(year == 2011)
attributes_table_2011 <- attributes_table_2011[!duplicated(attributes_table_2011$dz_2001),]


dz_2001_shp <- readOGR(
  dsn = "shapefiles/scotland_2001_datazones",
  layer = "scotland_dz_2001"                     
)      


dz_2001_shp_2001data <- dz_2001_shp 
dz_2001_shp_2011data <- dz_2001_shp 

dz_2001_shp_2001data@data <- dz_2001_shp_2001data@data %>% rename(dz_2001=zonecode)
dz_2001_shp_2001data_merged <- merge(x = dz_2001_shp_2001data, y = attributes_table_2001, by.x = "dz_2001", by.y = "dz_2001", all.x = TRUE)


dz_2001_shp_2011data@data <- dz_2001_shp_2011data@data %>% rename(dz_2001=zonecode)
dz_2001_shp_2011data_merged <- merge(x = dz_2001_shp_2011data, y = attributes_table_2011, by.x = "dz_2001", by.y = "dz_2001", all.x = TRUE)


writeOGR(dz_2001_shp_2001data_merged, dsn = "shapefiles_with_attributes", layer = "ethnicity_2001dzs_4cats_2001data", driver = "ESRI Shapefile")
writeOGR(dz_2001_shp_2011data_merged, dsn = "shapefiles_with_attributes", layer = "ethnicity_2001dzs_4cats_2011data", driver = "ESRI Shapefile")



# Now the same but the two category versions



# Attribute data 

attributes_table <- read.csv("output_data/ethnicity_datazones_2001_and_2011.csv")
attributes_table <- attributes_table  %>% mutate(white = total - nonwhite)

attributes_table_2001 <- attributes_table %>% filter(year == 2001)
attributes_table_2001 <- attributes_table_2001[!duplicated(attributes_table_2001$dz_2001),]

attributes_table_2011 <- attributes_table %>% filter(year == 2011)
attributes_table_2011 <- attributes_table_2011[!duplicated(attributes_table_2011$dz_2001),]


dz_2001_shp <- readOGR(
  dsn = "shapefiles/scotland_2001_datazones",
  layer = "scotland_dz_2001"                     
)      


dz_2001_shp_2001data <- dz_2001_shp 
dz_2001_shp_2011data <- dz_2001_shp 

dz_2001_shp_2001data@data <- dz_2001_shp_2001data@data %>% rename(dz_2001=zonecode)
dz_2001_shp_2001data_merged <- merge(x = dz_2001_shp_2001data, y = attributes_table_2001, by.x = "dz_2001", by.y = "dz_2001", all.x = TRUE)


dz_2001_shp_2011data@data <- dz_2001_shp_2011data@data %>% rename(dz_2001=zonecode)
dz_2001_shp_2011data_merged <- merge(x = dz_2001_shp_2011data, y = attributes_table_2011, by.x = "dz_2001", by.y = "dz_2001", all.x = TRUE)


writeOGR(dz_2001_shp_2001data_merged, dsn = "shapefiles_with_attributes", layer = "ethnicity_2001dzs_2cats_2001data", driver = "ESRI Shapefile")
writeOGR(dz_2001_shp_2011data_merged, dsn = "shapefiles_with_attributes", layer = "ethnicity_2001dzs_2cats_2011data", driver = "ESRI Shapefile")




