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


# Using a join-attribute table function from the following link

#http://permalink.gmane.org/gmane.comp.lang.r.geo/20914#

join_attribute_table <- function(x, y, xcol, ycol) {
  # Merges data frame to SpatialPolygonsDataFrame, keeping the correct
  # order. Code from suggestions at:
  # https://stat.ethz.ch/pipermail/r-sig-geo/2008-January/003064.html
  # Args:
  #   x: SpatialPolygonsDataFrame
  #   y: Name of data.frame to merge
  #   xcol: Merge column name
  #   ycol: Merge column name
  # Returns: Shapefile with merged attribute table
  
  x$sort_id <- 1:nrow(as(x, "data.frame"))  # Column containing
  #original row order for later sorting
  
  x.dat <- as(x, "data.frame")  # Create new data.frame object
  x.dat2 <- merge(x.dat, y, by.x = xcol, by.y = ycol)  # Merge
  x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  # Reorder back to original
  x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  # Make new set of
  #polygons, dropping those which aren't in merge
  x2.dat <- as(x2, "data.frame")  # Make update x2 into a data.frame
  row.names(x.dat2.ord) <- row.names(x2.dat)  # Reassign row.names
  # from original data.frame
  x2@data <- x.dat2.ord  # Assign to shapefile the new data.frame
  return(x2)
}



# two group versions; whole of Scotland -----------------------------------



dz_2011_shp <- readOGR(
  dsn = "shapefiles/SG_DataZoneBdry_2011",
  layer = "SG_DataZone_Bdry_2011"                     
)      


# csv locations 
att_files <- dir("output_data/dz_2011/binary")



fn <- function(x){
  infile <- read_csv(paste0("output_data/dz_2011/binary/", x))
  
  infile <- infile[!duplicated(infile),]
  
  shp_joined <- dz_2011_shp
  
  shp_joined <- join_attribute_table(dz_2011_shp, infile, "DataZone", "dz_2011")
  
  
  
  
  outname <- x %>% str_replace("\\.csv$", "")
  
  writeOGR(shp_joined, dsn = "shapefiles_with_attributes/2grp_2011", layer = outname,  driver = "ESRI Shapefile")
  
  
  return(NULL)
}


l_ply(att_files, fn, .progress = "text")


# This produces MANY warning messages, examples below:

# Warning 1: Value 218966759.866 of field Shape_Area of feature 1149 not successfully written. Possibly due to too larger number with respect to field width
# Warning 1: Value 201699500.39399999 of field Shape_Area of feature 1364 not successfully written. Possibly due to too larger number with respect to field width
#

# However, the shapefiles appear to work. Shape_Area fields are visible. 


# produce separate shapfiles for separate TTWAs ---------------------------

ttwas <- c("Aberdeen", "Glasgow", "Edinburgh", "Dundee", 
           "Inverness", "Perth", "Falkirk and Stirling")

att_files <- dir("output_data/dz_2011/binary")
combinations <- expand.grid(attributes = att_files, ttwa = ttwas)

# so, the aim is to produce smaller joins for each ttwa, and to label each year/attribute/ttwa shapefile appropriately
ttwa <- read_csv(file = "input_data/lookups/LSOA DZ SOA to TTWA Lookup_V2.csv", col_types = "ccccc")

# Need to state centroids for each ttwa
# postcode_to_oa <- read_csv(file = "input_data/lookups/POSTCODE_TO_OA.csv")
# norman <- read_csv("input_data/lookups/paul_norman_dz2011_table.csv")
# postcode_to_oa %>% 
#   select(postcode = Postcode, oa_2011 = OutputArea2011Code) %>% 
#   inner_join(norman) %>% 
#   filter(postcode %in% c("G1 3BU", "EH1 1BQ", "AB10 1AN", "DD1 2AJ", "PH2 8PA", "IV1 1HY", "FK8 2LJ"))

# ACTION: Ask Gwilym for his defintions of centres for ttwas


# Definitions of centroids 

# Glasgow
#  - West End of George Square
# G1 3BU
# dz_2001 : S01003358
# dz_2011 : S01010265

# Edinburgh 
# - 31 Waverley Bridge
# EH1 1BQ
# dz_2001 : S01002131
# dz_2011 : S01008677

# Aberdeen
# - Shoe Lane
# AB10 1AN
# dz_2001 : S01000126
# dz_2011 : S01006646

# Dundee 
# - Commercial Street
# DD1 2AJ
# dz_2001 : S01001101
# dz_2001 : S01007705

# Perth 
# - South Street
# PH2 8PA
# dz_2001 : S01005037
# dz_2011 : S01011939 (option A)
# dz_2011 : S01011940 (option B)


# Inverness 
# - High Street/Castle Street
# IV1 1HY
# dz_2001 : S01003853
# dz_2011 : S01010620

# Stirling
# Port Street
# FK8 2LJ
# dz_2001 : S01006120
# dz_2011 : S01013067

# 2001 centroids
# ttwa_centroids <- c(
#   Aberdeen = "S01000125",
#   Glasgow = "S01003358",
#   Edinburgh = "S01002131", 
#   Dundee = "S01001101",
#   `Inverness and Dingwall` = "S01003853",
#   `Perth and Blairgowrie` = "S01005037",
#   `Stirling and Alloa` = "S01006120"
#   )

# 2011 centroids
ttwa_centroids <- c(
  Aberdeen = "S01006646",
  Glasgow = "S01010265",
  Edinburgh = "S01008677",
  Dundee = "S01007705",
  Inverness = "S01010620",
  Perth = "S01011939",
  `Falkirk and Stirling` = "S01013067"
  )


fn <- function(x){
  # x now contains 
  # x$attributes 
  # x$ttwa 
  infile <- read_csv(paste0("output_data/dz_2011/binary/", x$attributes))
  infile <- infile[!duplicated(infile),]
  # Now need to add one more column: centre
  # 1 if city centre; 0 otherwise
  infile <- infile %>% mutate(
    centre = ifelse(dz_2011 == ttwa_centroids[x$ttwa], 1, 0)
  )
  
  dzs_in_selection <- ttwa %>% filter(TTWA11NM == x$ttwa) %>% .$LSOA11CD %>% unique

  shp_joined <- join_attribute_table(dz_2011_shp, infile, "DataZone", "dz_2011")

  shp_joined <- shp_joined[shp_joined$DataZone %in% dzs_in_selection,]
  
  outname <- paste0(x$ttwa, "_", str_replace(x$attributes, "\\.csv$", ""))
  plot(shp_joined, main = outname)

  writeOGR(shp_joined, dsn = "shapefiles_with_attributes/2grp_2011/ttwa", layer = outname,  driver = "ESRI Shapefile")
  
  
  return(NULL)
}



a_ply(combinations, 1, fn, .progress = "text")


