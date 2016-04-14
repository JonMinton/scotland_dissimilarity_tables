# Scoping and/or implementation of measures of segregation


rm(list = ls())

require(readxl)
require(xlsx)
require(readr)

require(spdep)
require(maptools)
require(rgdal)

require(car)
require(stringr)
require(plyr)
require(tidyr)
require(dplyr)

require(seg)
require(OasisR)
# This looks a more clearly laid out and comprehensive alternative 
# to seg

#https://cran.r-project.org/web/packages/OasisR/OasisR.pdf

require(ggplot2)

# Load some kind of shapefile containing the attributes 

dta <- readOGR(
  dsn = "shapefiles_with_attributes/2grp_2001",
  layer = "accom_2001"                     
)    

# nhd_matrix <- nb2mat(
#   poly2nb(
#     dta, queen = FALSE), 
#   style = "B",
#   zero.policy = T
# )


# unevenness 

dissim_oasis <- DI(dta@data[,c("house", "nonhouse")])

# dissim_scores <- dissim(
#   x = dta, 
#   data = dta@data[,c("house", "nonhouse")],
#   nb = nhd_matrix,
#   adjust = T
#   )

# Fails for spatially adjusted measures 
# because cannot handle empty neighbours 

dissim_scores <- dissim(
  x = dta, 
  data = dta@data[,c("house", "nonhouse")],
  adjust = F
)

# Values match

# isolation

iso_scores <- seg::spseg(
  x = dta,
  data = dta@data[, c("house", "nonhouse")]
  )

iso_scores2 <- OasisR::xPx(dta@data[,c("house", "nonhouse")])

# Numbers match 

# Adjusted isolation (Eta2)

eta2 <- OasisR::Eta2(dta@data[,c("house", "nonhouse")])



# centralisation

# start with Glasgow accommodation

dta <- readOGR(
  dsn = "shapefiles_with_attributes/2grp_2001/ttwa",
  layer = "Glasgow_accom_2001"                     
)   

distc <- distcenter(dta, center = 573)

rce_glasgow <- RCE(
  x = dta@data[,c("house", "nonhouse")],
  dc = distc,
  center = 573
)

# concentration 
# trying out for glasgow at the moment

areas <- area(dta)

rco_glasgow <- RCO(
  x = dta@data[,c("house", "nonhouse")],
  a = areas
) 

# produced a value of -2.22,
# when the limits are reported as -1 to 1

# The other value is 0.713
# Which is close to the value calculated 
# in geoseg analyser (0.729)

# This could be an issue with the RCO index itself:
#The following paper seems to suggest that the index can become 
# unbounded for some types of data:
# http://sf.oxfordjournals.org/content/76/3/1115.long

# The results (0.71 and -2.21) are identical when the areas are calculated 
# directly from the shapefile
rco_glasgow <- RCO(
  x = dta@data[,c("house", "nonhouse")],
  folder = "shapefiles_with_attributes/2grp_2001/ttwa",
  shape = "Glasgow_accom_2001"    
) 


# 