# produce map of travel to work boundaries 


# script to link 2 and 4 group ethnicity attribute data by 2011 datazones to shapefiles 


rm(list = ls())


require(readr)

require(spdep)
require(maptools)
require(rgdal)

require(stringr)
require(plyr)
require(tidyr)
require(dplyr)

require(tmap)
require(ggmap)
require(ggplot2)




# two group versions; whole of Scotland -----------------------------------



# dz_2001_shp <- readOGR(
#   dsn = "shapefiles/scotland_2001_datazones",
#   layer = "scotland_dz_2001"                     
# )      

dz_2001_shp <- read_shape(file = "shapefiles/scotland_2001_datazones/scotland_dz_2001.shp")


# produce separate shapfiles for separate TTWAs ---------------------------

ttwas <- c("Aberdeen", "Glasgow", "Edinburgh", "Dundee")

# so, the aim is to produce smaller joins for each ttwa, and to label each year/attribute/ttwa shapefile appropriately
ttwa <- read_csv(file = "input_data/lookups/LSOA01_TTWA01_UK_LU.csv", col_types = "ccccccc")

# quick table showing the number of datazones by travel to work area

ttwa
ttwa_simple <- ttwa %>% 
  mutate(
    four_cities = TTWA01NM,
    four_cities = ifelse(four_cities %in% ttwas, four_cities, "Elsewhere")
  )


dz_joined <- append_data(
  shp = dz_2001_shp, data = ttwa_simple, 
  key.shp = "zonecode", key.data = "LSOA01CD",
  ignore.duplicates = T
                         ) 

# Present travel to work areas as a single figure 

png("maps/travel_to_work_areas.png", height = 15, width = 10, res = 300, units = "cm")

qtm(dz_joined, fill = "grey", borders = NULL) + 
  qtm(dz_joined[dz_joined$four_cities == "Glasgow",], fill = "red", borders = NULL) + 
  qtm(dz_joined[dz_joined$four_cities == "Edinburgh",], fill = "blue", borders = NULL) + 
  qtm(dz_joined[dz_joined$four_cities == "Dundee",], fill = "green", borders = NULL) + 
  qtm(dz_joined[dz_joined$four_cities == "Aberdeen",], fill = "purple", borders = NULL) 
  
dev.off()

png("maps/areas_with_largest_nonscottish_populations.png", height = 15, width = 10, res = 300, units = "cm")
qtm(dz_joined, fill = "grey", borders = NULL) + 
  qtm(dz_joined[dz_joined$TTWA01NM == "Carlisle",], fill = "red", borders = NULL) +  
  qtm(dz_joined[dz_joined$TTWA01NM == "Ullapool and Gairloch",], fill = "green", borders = NULL) +
  qtm(dz_joined[dz_joined$TTWA01NM == "Berwick",], fill = "blue", borders = NULL) +
  qtm(dz_joined[dz_joined$TTWA01NM == "Pitlochry",], fill = "yellow", borders = NULL) +  
  qtm(dz_joined[dz_joined$TTWA01NM == "St Andrews and Cupar",], fill = "purple", borders = NULL) + 
  qtm(dz_joined[dz_joined$TTWA01NM == "Badenoch",], fill = "orange", borders = NULL) 
  
dev.off()  

#shp_joined <- join_attribute_table(dz_2001_shp, ttwa, "zonecode", "LSOA01CD")
# Need to state centroids for each ttwa

# ACTION: Ask Gwilym for his defintions of centres for ttwas


# Definitions of centroids 

# Glasgow
#  - West End of George Square
# G1 3BU
# S01003358

# Edinburgh 
# - 31 Waverley Bridge
# EH1 1BQ
# S01002131


# Aberdeen
# - Shoe Lane
# AB10 1AN
# S01000126

# Dundee 
# - Commercial Street
# DD1 2AJ
# S01001101

ttwa_centroids <- c(
  Aberdeen = "S01000125",
  Glasgow = "S01003358",
  Edinburgh = "S01002131", 
  Dundee = "S01001101"
)


# Maps highlighting datazone city centres for each ttwa -------------------



ttwas <- c("Aberdeen", "Glasgow", "Edinburgh", "Dundee")

# so, the aim is to produce smaller joins for each ttwa, and to label each year/attribute/ttwa shapefile appropriately
ttwa <- read_csv(file = "input_data/lookups/LSOA01_TTWA01_UK_LU.csv", col_types = "ccccccc")

# quick table showing the number of datazones by travel to work area

ttwa
ttwa_simple <- ttwa %>% 
  mutate(
    four_cities = TTWA01NM,
    four_cities = ifelse(four_cities %in% ttwas, four_cities, "Elsewhere"),
    centre = as.factor(ifelse(LSOA01CD %in% ttwa_centroids, "yes", "no"))
  )


dz_joined <- append_data(
  shp = dz_2001_shp, data = ttwa_simple, 
  key.shp = "zonecode", key.data = "LSOA01CD",
  ignore.duplicates = T
) 


png("maps/ttwa_centre_glasgow.png", width = 15, height = 15, units = "cm", res = 300)
dz_joined[dz_joined$four_cities == "Glasgow",] %>% 
  tm_shape(.) + 
  tm_polygons("centre", palette = c("lightgrey", "red"))

dev.off()


png("maps/ttwa_centre_edinburgh.png", width = 15, height = 15, units = "cm", res = 300)
dz_joined[dz_joined$four_cities == "Edinburgh",] %>% 
  tm_shape(.) + 
  tm_polygons("centre", palette = c("lightgrey", "red"))

dev.off()

png("maps/ttwa_centre_aberdeen.png", width = 20, height = 20, units = "cm", res = 300)
dz_joined[dz_joined$four_cities == "Aberdeen",] %>% 
  tm_shape(.) + 
  tm_polygons("centre",palette = c("lightgrey", "red"))

dev.off()

png("maps/ttwa_centre_dundee.png", width = 15, height = 15, units = "cm", res = 300)
dz_joined[dz_joined$four_cities == "Dundee",] %>% 
  tm_shape(.) + 
  tm_polygons("centre",palette = c("lightgrey", "red"))

dev.off()


# quick map of Carlisle
png("maps/ttwa_centre_dundee.png", width = 15, height = 15, units = "cm", res = 300)
dz_joined[dz_joined$TTWA01NM == "Carlisle",] %>% 
  qtm(.) 

dev.off()



