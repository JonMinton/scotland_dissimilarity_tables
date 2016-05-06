# Scoping and/or implementation of measures of segregation

rm(list = ls())

# trying out pacman package

#install.packages("pacman")
require(pacman)

pacman::p_load(
  readxl, xlsx, readr, spdep, 
  maptools, rgdal, car, stringr, 
  purrr, tidyr, dplyr, seg, OasisR,
  ggplot2
)

#https://cran.r-project.org/web/packages/OasisR/OasisR.pdf

# Load some kind of shapefile containing the attributes 
# 
# dta <- readOGR(
#   dsn = "shapefiles_with_attributes/2grp_2001",
#   layer = "accom_2001"                     
# )    
# 
# nhd_matrix <- nb2mat(
#   poly2nb(
#     dta, queen = FALSE),
#   style = "B",
#   zero.policy = T
# ) # NOTE: Takes a while to calculate!
# 
# 
# 
# # unevenness 
# 
# dissim_oasis <- DI(dta@data[,c("house", "nonhouse")])
# 
# d_adj_oasis <- Morill(dta@data[,c("house", "nonhouse")], nhd_matrix) 
# # THIS TAKES AGES!!!
# adj <- dissim_oasis - d_adj_oasis
# 

# dissim_scores <- dissim(
#   x = dta, 
#   data = dta@data[,c("house", "nonhouse")],
#   nb = nhd_matrix,
#   adjust = T
#   )

# Fails for spatially adjusted measures 
# because cannot handle empty neighbours 

# 
# # isolation
# 
# iso_scores2 <- OasisR::xPx(dta@data[,c("house", "nonhouse")])
# 
# # Numbers match 
# 
# # Adjusted isolation (Eta2)
# 
# eta2 <- OasisR::Eta2(dta@data[,c("house", "nonhouse")])
# 
# # centralisation
# 
# # start with Glasgow accommodation
# 
# dta <- readOGR(
#   dsn = "shapefiles_with_attributes/2grp_2001/ttwa",
#   layer = "Glasgow_accom_2001"                     
# )   
# 
# distc <- distcenter(dta, center = 573)
# 
# rce_glasgow <- RCE(
#   x = dta@data[,c("house", "nonhouse")],
#   dc = distc,
#   center = 573
# )


# Automating/standardising the above 

fls <- list.files("shapefiles_with_attributes/2grp_2001/ttwa", "\\.shp$")
task_list <- data_frame(filename =fls) %>% 
  mutate(tmp = str_replace(filename, "\\.shp$", "")) %>% 
  separate(tmp, into = c("place", "attribute", "year"), sep = "_")


# First, create a function which saves each shapefile into an element of a not-quite dataframe


get_shapefiles <- function(filename){
  filename_short <- str_replace(filename, "\\.shp$", "")
  this_shp <- readOGR(
    dsn = "shapefiles_with_attributes/2grp_2001/ttwa",
    layer = filename_short
  )
  
  return(this_shp)
}

task_list <- task_list %>% 
  mutate(shp = map(filename, get_shapefiles)) 

# extract data portion of each object
task_list <- task_list %>% 
  mutate(dta = map(shp, ~ slot(., "data"))) 

# Now neighbourhood matrix

get_nhd <- function(x){
  
  output <- x %>% 
    poly2nb(., queen = FALSE) %>%
    nb2mat(., style = "B", zero.policy = T)
  return(output)
}

task_list <- task_list %>% 
  mutate(nhd = map(shp, get_nhd)) 

# Now to add the measures one by one 

get_simple_d <- function(x){
  counts_matrix <- x[,c(8, 9)]
  dissim <- OasisR::DI(counts_matrix)
  output <- dissim[2,1]
  return(output)
}
# Calculate indices for each separately 

get_adj_d <- function(x, y){
  counts_matrix <- x[,c(8, 9)]
  output <- OasisR::Morill(counts_matrix, y) %>% .[1]   
  return(output)
}
# Morill(dta@data[,c("house", "nonhouse")], nhd_matrix) 


task_list <- task_list %>% 
  mutate(
    d_simple = map_dbl(dta, get_simple_d),
    d_adj = map2_dbl(dta, nhd, get_adj_d)
  )

# fn <- function(x){
#   dta <- readOGR(
#     dsn = "shapefiles_with_attributes/2grp_2001",
#     layer = "accom_2001"                     
#   )    
#   
#   nhd_matrix <- nb2mat(
#     poly2nb(
#       dta, queen = FALSE),
#     style = "B",
#     zero.policy = T
#   ) # NOTE: Takes a while to calculate!
#   
#   
#   
#   # unevenness 
#   
#   dissim_oasis <- DI(dta@data[,c("house", "nonhouse")])
#   
#   d_adj_oasis <- Morill(dta@data[,c("house", "nonhouse")], nhd_matrix) 
#   
#   adj <- dissim_oasis - d_adj_oasis
#   
#   
#   # dissim_scores <- dissim(
#   #   x = dta, 
#   #   data = dta@data[,c("house", "nonhouse")],
#   #   nb = nhd_matrix,
#   #   adjust = T
#   #   )
#   
#   # Fails for spatially adjusted measures 
#   # because cannot handle empty neighbours 
#   
#   
#   # isolation
#   
#   iso_scores2 <- OasisR::xPx(dta@data[,c("house", "nonhouse")])
#   
#   # Numbers match 
#   
#   # Adjusted isolation (Eta2)
#   
#   eta2 <- OasisR::Eta2(dta@data[,c("house", "nonhouse")])
#   
#   # centralisation
#   
#   # start with Glasgow accommodation
#   
#   dta <- readOGR(
#     dsn = "shapefiles_with_attributes/2grp_2001/ttwa",
#     layer = "Glasgow_accom_2001"                     
#   )   
#   
#   distc <- distcenter(dta, center = 573)
#   
#   rce_glasgow <- RCE(
#     x = dta@data[,c("house", "nonhouse")],
#     dc = distc,
#     center = 573
#   )
#   
#   
#   
# }
# 
#   
