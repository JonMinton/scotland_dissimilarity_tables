# script to link 2 and 4 group ethnicity attribute data by 2011 datazones to shapefiles 


rm(list = ls())



require(spdep)
require(maptools)
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

attributes_table <- read.csv("output_data/2011_datazones/ethnicity_4group_categories_datazones_2001_and_2011.csv")

dz_2011_shp <- readShapeSpatial(
  "shapefiles/scotland_2011_datazones/DZ_2011_EoR_Scotland.shp"
)      


dz_2011_shp@data <- dz_2011_shp@data %>% rename(dz_2011=DZ_CODE)

dz_2011_shp@data <- dz_2011_shp@data %>% inner_join(attributes_table)


# Problem: the shapefile doesn't use 2011 datazone codes.. more recent version needed.