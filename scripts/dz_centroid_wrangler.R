# Script for looking at datazones and output areas 

rm(list = ls())


require(maptools)
require(rgeos)
require(rgdal)

require(ggmap)
require(ggplot2)


datazones <- readShapePoly(fn = "shapefiles/scotland_2001_datazones/scotland_dz_2001")

oa_01 <- readShapePoly(fn = "shapefiles/2001_census_output_areas/Scotland_oa_2001_area")
oa_11 <- readShapePoly(fn = "shapefiles/2011_census_output_areas/BoundaryData/scotland_oa_2011")

head(sapply(slot(oa_01, "polygons"), slot, "area"))


# Workflow

# For each datazone
  # for each OA year
    # Find all output area centroids 
    # Calculate which centroids are within the datazone
    # calculate area of union of oas whose centroids are in dz
    # calculate area of intersection between union of oas with centroids in dz, and datazone
    # return dataframe with oa linked to datazone, and measure of fit based on above 
  # 
# 


N <- length(datazones)

oa_01_centroids <- gCentroid(oa_01, byid = T)
oa_11_centroids <- gCentroid(oa_11, byid = T)
for (i in 1:N){
  this_dz_id <- datazones@polygons[[i]]@ID
  this_dz_poly <- datazones@polygons[[i]]@Polygons[[1]]
  this_dz_poly_area <- this_dz_poly@area
  this_dz_poly_centroids <- gCentroid(datazones, byid = T)
  
  
}
