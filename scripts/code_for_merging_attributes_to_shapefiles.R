# script to link 2 and 4 group ethnicity attribute data by 2011 datazones to shapefiles 


rm(list = ls())



require(spdep)
require(maptools)
require(rgdal)

require(stringr)
require(plyr)
require(tidyr)
require(dplyr)







dz_2001_shp <- readOGR(
  dsn = "shapefiles/scotland_2001_datazones",
  layer = "scotland_dz_2001"                     
)      


# csv locations 
dir("output_data/dz_2001/")

att_files <- dir("output_data/dz_2001/")



fn <- function(x){
  infile <- read_csv(paste0("output_data/dz_2001/", x))
  
  infile <- infile[!duplicated(infile),]
  
  shp_joined <- dz_2001_shp
  
  shp_joined@data <- merge(
    x = dz_2001_shp@data, 
    y= infile,
    by.x = "zonecode",
    by.y = "dz_2001",
    all.x =TRUE
  )
  
  outname <- x %>% str_replace("\\.csv$", "")
  
  writeOGR(shp_joined, dsn = "shapefiles_with_attributes", layer = outname,  driver = "ESRI Shapefile")
  
  
  return(NULL)
}


l_ply(att_files, fn, .progress = "text")


# do this for binary (two mutually exclusive category) versions of this 




dz_2001_shp <- readOGR(
  dsn = "shapefiles/scotland_2001_datazones",
  layer = "scotland_dz_2001"                     
)      


# csv locations 
dir("output_data/dz_2001/binary/")

att_files <- dir("output_data/dz_2001/binary")



fn <- function(x){
  infile <- read_csv(paste0("output_data/dz_2001/binary/", x))
  
  infile <- infile[!duplicated(infile),]
  
  shp_joined <- dz_2001_shp
  
  shp_joined@data <- merge(
    x = dz_2001_shp@data, 
    y= infile,
    by.x = "zonecode",
    by.y = "dz_2001",
    all.x =TRUE
  )
  
  outname <- x %>% str_replace("\\.csv$", "")
  
  writeOGR(shp_joined, dsn = "shapefiles_with_attributes/2grp_2001", layer = outname,  driver = "ESRI Shapefile")
  
  
  return(NULL)
}


l_ply(att_files, fn, .progress = "text")

# Code exploring GIDs and datazones from 2001 dz shapefile.

# There are more GIDs than datazones. I think this is because some datazones include more than 
# one polygon. This would make sense for island areas in particular, as the minimum size of datazones 
# means they are likely to include more than one non-contiguous polygon. 



# produce separate shapfiles for separate TTWAs ---------------------------

ttwas <- c("Aberdeen", "Glasgow", "Edinburgh", "Dundee")

att_files <- dir("output_data/dz_2001/binary")
combinations <- expand.grid(attributes = att_files, ttwa = ttwas)

# so, the aim is to produce smaller joins for each ttwa, and to label each year/attribute/ttwa shapefile appropriately
ttwa <- read_csv(file = "input_data/lookups/LSOA01_TTWA01_UK_LU.csv", col_types = "ccccccc")


fn <- function(x){
  # x now contains 
  # x$attributes 
  # x$ttwa 
  infile <- read_csv(paste0("output_data/dz_2001/binary/", x$attributes))
  infile <- infile[!duplicated(infile),]

  dzs_in_selection <- ttwa %>% filter(TTWA01NM == x$ttwa) %>% .$LSOA01CD %>% unique

  shp_joined <- dz_2001_shp
  
  shp_joined <- shp_joined[shp_joined$zonecode %in% dzs_in_selection,]
  

  outname <- paste0(x$ttwa, "_", str_replace(x$attributes, "\\.csv$", ""))
  plot(shp_joined, main = outname)

  writeOGR(shp_joined, dsn = "shapefiles_with_attributes/2grp_2001/ttwa", layer = outname,  driver = "ESRI Shapefile")
  
  
  return(NULL)
}


a_ply(combinations, 1, fn, .progress = "text")


# This seems to have worked programmatically, but the TTWA areas are non-contiguous and so look like they aren't correct

# to check this more carefully I'm now going to plot the shapefiles, highlighting particular ttwas


plot(dz_2001_shp)




shp_joined <- dz_2001_shp

# Rather than try to merge, instead try to filter 



plot(shp_joined)

these_dzs <- ttwa  %>% filter(TTWA01NM == "Aberdeen")  %>% .$LSOA01CD  %>% unique
sel <- shp_joined$zonecode %in% these_dzs
plot(shp_joined[sel,], col = "red", add =T)

these_dzs <- ttwa  %>% filter(TTWA01NM == "Glasgow")  %>% .$LSOA01CD  %>% unique
sel <- shp_joined$zonecode %in% these_dzs
plot(shp_joined[sel,], col = "blue", add =T)



# 
# > require(spdep)
# Loading required package: spdep
# Loading required package: sp
# Loading required package: Matrix
# 
# Attaching package: ‘Matrix’
# 
# The following object is masked from ‘package:tidyr’:
#   
#   expand
# 
# Warning message:
#   package ‘spdep’ was built under R version 3.1.3 
# > require(maptools)
# Loading required package: maptools
# Checking rgeos availability: TRUE
# > require(rgdal)
# Loading required package: rgdal
# rgdal: version: 0.9-1, (SVN revision 518)
# Geospatial Data Abstraction Library extensions to R successfully loaded
# Loaded GDAL runtime: GDAL 1.11.1, released 2014/09/24
# Path to GDAL shared files: \\cfsk18.campus.gla.ac.uk/SSD_Home_Data_X/jm383x/My Documents/R/win-library/3.1/rgdal/gdal
# GDAL does not use iconv for recoding strings.
# Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012, [PJ_VERSION: 480]
# Path to PROJ.4 shared files: \\cfsk18.campus.gla.ac.uk/SSD_Home_Data_X/jm383x/My Documents/R/win-library/3.1/rgdal/proj
# > 
#   > require(stringr)
# > require(plyr)
# > require(tidyr)
# > require(dplyr)
# > dz_2001_shp <- readOGR(
#   +   dsn = "shapefiles/scotland_2001_datazones",
#   +   layer = "scotland_dz_2001"                     
#   + )
# OGR data source with driver: ESRI Shapefile 
# Source: "shapefiles/scotland_2001_datazones", layer: "scotland_dz_2001"
# with 6610 features and 5 fields
# Feature type: wkbPolygon with 2 dimensions
# > tmp <- dz_2001_shp
# > tmp <- tmp  %>% tbl_df
# Error: data is not a data frame
# > tmp <- dz_2001_shp@data
# > tmp <- tmp  %>% tbl_df
# > tmp
# Source: local data frame [6,610 x 5]
# 
# gid  zonecode ons_code       label name
# 1    1 S01006490       RH 31S01006490   NA
# 2    2 S01006505       RH 31S01006505   NA
# 3    3 S01006499       RH 31S01006499   NA
# 4    4 S01006494       RH 31S01006494   NA
# 5    5 S01006397       RH 31S01006397   NA
# 6    6 S01006456       RH 31S01006456   NA
# 7    7 S01006422       RH 31S01006422   NA
# 8    8 S01006418       RH 31S01006418   NA
# 9    9 S01006429       RH 31S01006429   NA
# 10  10 S01006419       RH 31S01006419   NA
# .. ...       ...      ...         ...  ...
# > length(unique(tmp$zonecode))
# [1] 6505
# > View(tmp)
# > tmp  %>% arrange(zonecode)
# Source: local data frame [6,610 x 5]
# 
# gid  zonecode ons_code       label name
# 1  6089 S01000001       QA 01S01000001   NA
# 2  6090 S01000002       QA 01S01000002   NA
# 3  6091 S01000003       QA 01S01000003   NA
# 4  6092 S01000004       QA 01S01000004   NA
# 5  6093 S01000005       QA 01S01000005   NA
# 6  6094 S01000006       QA 01S01000006   NA
# 7  6095 S01000007       QA 01S01000007   NA
# 8  6096 S01000008       QA 01S01000008   NA
# 9  6097 S01000009       QA 01S01000009   NA
# 10 6098 S01000010       QA 01S01000010   NA
# ..  ...       ...      ...         ...  ...
# > tmp  %>% arrange(zonecode)  %>% mutate(x = NA, x = lag(zonecode))
# Source: local data frame [6,610 x 6]
# 
# gid  zonecode ons_code       label name  x
# 1  6089 S01000001       QA 01S01000001   NA NA
# 2  6090 S01000002       QA 01S01000002   NA  1
# 3  6091 S01000003       QA 01S01000003   NA  2
# 4  6092 S01000004       QA 01S01000004   NA  3
# 5  6093 S01000005       QA 01S01000005   NA  4
# 6  6094 S01000006       QA 01S01000006   NA  5
# 7  6095 S01000007       QA 01S01000007   NA  6
# 8  6096 S01000008       QA 01S01000008   NA  7
# 9  6097 S01000009       QA 01S01000009   NA  8
# 10 6098 S01000010       QA 01S01000010   NA  9
# ..  ...       ...      ...         ...  ... ..
# > tmp  %>% arrange(zonecode)  %>% mutate(x = lag(zonecode))
# Source: local data frame [6,610 x 6]
# 
# gid  zonecode ons_code       label name  x
# 1  6089 S01000001       QA 01S01000001   NA NA
# 2  6090 S01000002       QA 01S01000002   NA  1
# 3  6091 S01000003       QA 01S01000003   NA  2
# 4  6092 S01000004       QA 01S01000004   NA  3
# 5  6093 S01000005       QA 01S01000005   NA  4
# 6  6094 S01000006       QA 01S01000006   NA  5
# 7  6095 S01000007       QA 01S01000007   NA  6
# 8  6096 S01000008       QA 01S01000008   NA  7
# 9  6097 S01000009       QA 01S01000009   NA  8
# 10 6098 S01000010       QA 01S01000010   NA  9
# ..  ...       ...      ...         ...  ... ..
# > ?lag
# > ?dplyr::lag.default
# > lapply(tmp, class)
# $gid
# [1] "integer"
# 
# $zonecode
# [1] "factor"
# 
# $ons_code
# [1] "factor"
# 
# $label
# [1] "factor"
# 
# $name
# [1] "factor"
# 

# > tmp  %>% arrange(zonecode)  %>% mutate_each(funs(as.character), -gid)  %>% 
# mutate(x = lag(zonecode), y = x == zonecode)
# Source: local data frame [6,610 x 7]
# 
# gid  zonecode ons_code       label name         x     y
# 1  6089 S01000001       QA 01S01000001   NA        NA    NA
# 2  6090 S01000002       QA 01S01000002   NA S01000001 FALSE
# 3  6091 S01000003       QA 01S01000003   NA S01000002 FALSE
# 4  6092 S01000004       QA 01S01000004   NA S01000003 FALSE
# 5  6093 S01000005       QA 01S01000005   NA S01000004 FALSE
# 6  6094 S01000006       QA 01S01000006   NA S01000005 FALSE
# 7  6095 S01000007       QA 01S01000007   NA S01000006 FALSE
# 8  6096 S01000008       QA 01S01000008   NA S01000007 FALSE
# 9  6097 S01000009       QA 01S01000009   NA S01000008 FALSE
# 10 6098 S01000010       QA 01S01000010   NA S01000009 FALSE
# ..  ...       ...      ...         ...  ...       ...   ...

# > tmp  %>% arrange(zonecode)  %>% mutate_each(funs(as.character), -gid)  %>% mutate(x = lag(zonecode), y = x == zonecode)   %>% xtabs( ~ y, data = .)
# y
# FALSE  TRUE 
# 6504   105 


# > tmp  %>% arrange(zonecode)  %>% 
# mutate_each(funs(as.character), -gid)  %>% 
#   mutate(x = lag(zonecode), y = x == zonecode)  %>% 
#   filter(y==T)
# Source: local data frame [105 x 7]
# 
# gid  zonecode ons_code       label name         x    y
# 1  6610 S01000105       QA 01S01000105   NA S01000105 TRUE
# 2  6604 S01000481       QB 02S01000481   NA S01000481 TRUE
# 3  6576 S01000711       QD 04S01000711   NA S01000711 TRUE
# 4  6577 S01000711       QD 04S01000711   NA S01000711 TRUE
# 5  6574 S01000721       QD 04S01000721   NA S01000721 TRUE
# 6  6570 S01000755       QD 04S01000755   NA S01000755 TRUE
# 7  6571 S01000755       QD 04S01000755   NA S01000755 TRUE
# 8  6572 S01000755       QD 04S01000755   NA S01000755 TRUE
# 9  6568 S01000763       QD 04S01000763   NA S01000763 TRUE
# 10 6564 S01000796       QD 04S01000796   NA S01000796 TRUE
# ..  ...       ...      ...         ...  ...       ...  ...

