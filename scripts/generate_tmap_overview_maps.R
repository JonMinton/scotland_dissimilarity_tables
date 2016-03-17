# produce map of travel to work boundaries 


# script to link 2 and 4 group ethnicity attribute data by 2011 datazones to shapefiles 


rm(list = ls())


require(readr)

require(spdep)
require(maptools)
require(rgdal)
require(rgeos)

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

ttwas <- c("Aberdeen", "Glasgow", "Edinburgh", "Dundee", 
           "Inverness and Dingwall", "Perth and Blairgowrie", "Stirling and Alloa")

# so, the aim is to produce smaller joins for each ttwa, and to label each year/attribute/ttwa shapefile appropriately
ttwa <- read_csv(file = "input_data/lookups/LSOA01_TTWA01_UK_LU.csv", col_types = "ccccccc")

# quick table showing the number of datazones by travel to work area

ttwa
ttwa_simple <- ttwa %>% 
  mutate(
    seven_cities = TTWA01NM,
    seven_cities = ifelse(seven_cities %in% ttwas, seven_cities, "Elsewhere")
  )


dz_joined <- append_data(
  shp = dz_2001_shp, data = ttwa_simple, 
  key.shp = "zonecode", key.data = "LSOA01CD",
  ignore.duplicates = T
                         ) 

# Attempt to produce dissolved version of above using ttwas 

ttwa_joined <- gUnaryUnion(dz_joined, id = dz_joined@data$TTWA01NM)
# horrible hacking (from https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/)
df <- as.data.frame(names(ttwa_joined))
colnames(df) <- "ttwa_name"
row.names(ttwa_joined) <- as.character(1:length(ttwa_joined))
ttwa_joined <- SpatialPolygonsDataFrame(ttwa_joined, df)



# Present travel to work areas as a single figure 

png("maps/travel_to_work_areas.png", height = 15, width = 10, res = 300, units = "cm")


qtm(ttwa_joined, fill = "grey", borders = NULL) + 
  qtm(ttwa_joined[ttwa_joined$seven_cities != "Elsewhere",], 
    fill = "seven_cities", borders = NULL, fill.title = "TTWA Name")

dev.off()


# now to try the same with the cartogram

dz_cart_shp <- read_shape(file = "shapefiles/Scotland_2001_population_cartogram.shp")
dz_cart_shp <- set_projection(
  dz_cart_shp, 
  projection = get_proj4("longlat"), 
  current.projection = get_projection(dz_2001_shp), 
  overwrite.current.projection = T
  )

cart_joined <- append_data(
  shp = dz_cart_shp, data = ttwa_simple, 
  key.shp = "zonecode", key.data = "LSOA01CD",
  ignore.duplicates = T
) 

# Present travel to work areas as a single figure 

png("maps/travel_to_work_areas_cartogram.png", height = 15, width = 10, res = 300, units = "cm")

qtm(cart_joined, fill = "grey", borders = NULL) + 
  qtm(cart_joined[cart_joined$seven_cities != "Elsewhere",], 
      fill = "seven_cities", borders = NULL, fill.title = "TTWA Name")

dev.off()

cart_joined <- gUnaryUnion(cart_joined, id = cart_joined@data$TTWA01NM)
# horrible hacking (from https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/)
df <- as.data.frame(names(ttwa_joined))
colnames(df) <- "ttwa_name"
row.names(ttwa_joined) <- as.character(1:length(ttwa_joined))
ttwa_joined <- SpatialPolygonsDataFrame(ttwa_joined, df)



# for completeness let's do the cartogram with all TTWAs 
png("maps/all_travel_to_work_areas_cartogram.png", height = 30, width = 20, res = 300, units = "cm")
tm_shape(cart_joined) + 
  tm_fill(col = "TTWA01NM", borders = NULL, legend.show = T, style = "cat", max.categories = 48)
dev.off()  

# TO DO: produce another version of the shapefile with only TTWAs as polygons, then use
# this to display names of TTWAs on top of the areas using tm_text



png("maps/areas_with_largest_nonscottish_populations.png", height = 15, width = 10, res = 300, units = "cm")
qtm(dz_joined, fill = "grey", borders = NULL) + 
  qtm(dz_joined[dz_joined$TTWA01NM == "Carlisle",], fill = "red", borders = NULL) +  
  qtm(dz_joined[dz_joined$TTWA01NM == "Ullapool and Gairloch",], fill = "green", borders = NULL) +
  qtm(dz_joined[dz_joined$TTWA01NM == "Berwick",], fill = "blue", borders = NULL) +
  qtm(dz_joined[dz_joined$TTWA01NM == "Pitlochry",], fill = "yellow", borders = NULL) +  
  qtm(dz_joined[dz_joined$TTWA01NM == "St Andrews and Cupar",], fill = "purple", borders = NULL) + 
  qtm(dz_joined[dz_joined$TTWA01NM == "Badenoch",], fill = "orange", borders = NULL) 
  
dev.off()  


dz_joined <- append_data(
  shp = dz_2001_shp, data = ttwa, 
  key.shp = "zonecode", key.data = "LSOA01CD",
  ignore.duplicates = T
) 

png("maps/areas_with_consistently_high_lltis.png", height = 15, width = 10, res = 300, units = "cm")
qtm(dz_joined, fill = "grey", borders = NULL) + 
  qtm(dz_joined[
    dz_joined$TTWA01NM %in% 
      c("Dunoon and Bute", "Newton Stewart and Wigtown", "Stranraer", 
        "Greenock", "Kirkcaldy and Glenrothes", "Irvine and Arran","Ayr and Kilmarnock")
    ,], 
    fill = "TTWA01NM", borders = NULL, fill.title = "TTWA Name")
dev.off()

png("maps/areas_with_consistently_high_higher_sec.png", height = 15, width = 10, res = 300, units = "cm")

qtm(dz_joined, fill = "grey", borders = NULL) + 
  qtm(dz_joined[
    dz_joined$TTWA01NM %in% 
      c("Edinburgh", "St Andrews and Cupar", "Aberdeen", "Glasgow", "Stirling and Alloa", "Perth and Blairgowrie", "Galashiels and Peebles", "Dundee")
    ,], 
    fill = "TTWA01NM", borders = NULL, fill.title = "TTWA Name")
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




