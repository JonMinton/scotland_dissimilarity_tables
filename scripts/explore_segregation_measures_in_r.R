# Scoping and/or implementation of measures of segregation

rm(list = ls())

# trying out pacman package

#install.packages("pacman")
require(pacman)

pacman::p_load(
  readxl, xlsx, readr, spdep, 
  maptools, rgdal, car, stringr, 
  purrr, tidyr, dplyr, seg, OasisR,
  ggplot2, tmap
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

get_simple_d_and_names <- function(x){
  attribute_names <- names(x)[c(8, 9)]
  counts_matrix <- x[,c(8, 9)]
  dissim <- OasisR::DI(counts_matrix)
  output <- list(
    attribute_names = attribute_names, 
    simple_d_1 = dissim[2,1],
    simple_d_2 = dissim[1,2]
    )
  return(output)
}
# Calculate indices for each separately 

get_adj_d <- function(x, y){
  counts_matrix <- x[,c(8, 9)]
  outputs <- OasisR::Morill(counts_matrix, y)   
  return(outputs)
}
# Morill(dta@data[,c("house", "nonhouse")], nhd_matrix) 



task_list <- task_list %>% 
  mutate(
    d_simple_and_att_names = map(dta, get_simple_d_and_names),
    d_adj_both = map2(dta, nhd, get_adj_d),
    d_adj_1 = map_dbl(d_adj_both, ~ .[1]),
    d_adj_2 = map_dbl(d_adj_both, ~ .[2]),
    d_simple_1 = map_dbl(d_simple_and_att_names, ~ .[["simple_d_1"]]),
    d_simple_2 = map_dbl(d_simple_and_att_names, ~ .[["simple_d_2"]]),
    att_label_1 = map_chr(d_simple_and_att_names, ~ .[["attribute_names"]][1]), 
    att_label_2 = map_chr(d_simple_and_att_names, ~ .[["attribute_names"]][2])
    ) %>% 
  select(-d_simple_and_att_names, d_adj_both)


task_list <- task_list %>% 
  mutate(
    adj_1 = d_simple_1 - d_adj_1,
    adj_2 = d_simple_2 - d_adj_2
         
         )

get_xPx <- function(x){
  counts_matrix <- x[,c(8, 9)]
  output <- OasisR::xPx(counts_matrix) 
  return(output)
}


task_list <- task_list %>% 
  mutate(xPx = map(dta, get_xPx)) %>% 
  mutate(
    xPx_1 = map_dbl(xPx, ~ .[1]), 
    xPx_2 = map_dbl(xPx, ~ .[2])
    ) %>% 
  select(-xPx)


get_Eta2 <- function(x){
  counts_matrix <- x[,c(8, 9)]
  output <- OasisR::Eta2(counts_matrix) %>% .[1]
  return(output)
}

task_list <- task_list %>% 
  mutate(Eta2 = map_dbl(dta, get_Eta2)) 

get_rce <- function(dta, shp){
  counts_matrix <- dta[,c(8, 9)]
  cntr <- which(dta$centre == 1)
  distc <- distcenter(shp, center = cntr)
  rce <- RCE(
      x = counts_matrix,
      dc = distc,
      center = cntr
    )
  output <- rce[2,1]
  return(output)
}

task_list <- task_list %>% 
  mutate(rce = map2_dbl(dta, shp, get_rce)) 


# Bespoke concentration measure using same approach as RCE

get_rcon <- function(dta, shp){
  # rcon (borrows code from OasisR::RCE)
  rcon <- function(x, dens){
    x <- as.matrix(x)
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    varTotal <- colSums(x)
    xprovi <- cbind(x, dens)
    xprovi <- xprovi[order(xprovi[, ncol(xprovi)], decreasing = T), ] 
    # highest to lowest density, so should be decreasing
    xprovi <- as.data.frame(xprovi)
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
      XI1 <- cumsum(xprovi[, k1])[1:(nrow(xprovi) - 1)]/varTotal[k1]
      XI <- cumsum(xprovi[, k1])[2:nrow(xprovi)]/varTotal[k1]
      YI1 <- cumsum(xprovi[, k2])[1:(nrow(xprovi) - 1)]/varTotal[k2]
      YI <- cumsum(xprovi[, k2])[2:nrow(xprovi)]/varTotal[k2]
      result[k1, k2] <- XI1 %*% YI - XI %*% YI1
    }
    return(result)
  }

  dens <- tmap::calc_densities(shp, var = "total")
  
  counts_matrix <- dta[,c(8, 9)]
  output <- rcon(counts_matrix, dens) %>% .[2, 1]
  return(output)
}

task_list <- task_list %>% 
  mutate(rcon = map2_dbl(dta, shp, get_rcon))





simple_results <- task_list %>% 
  select(place, attribute, year, 
         attribute_label = att_label_1,
         evenness = d_simple_1, 
         clustering = adj_1, 
         isolation = Eta2, 
         centralisation = rce, 
         concentration = rcon
         ) %>% 
  gather(key = "dimension", value = "value", evenness:concentration)



simple_results <- simple_results %>% 
  mutate(dimension = factor(
    dimension, 
    levels = c(
      "evenness", "isolation", "clustering", 
      "centralisation", "concentration")
    )
    )

# Now how best to represent this? 

# simple_results %>% 
#   ggplot(., 
#          aes(
#            y = value, x = place, 
#            group = factor(year), colour = NULL, fill = factor(year)
#            )
#          ) +
#   facet_grid(pretty_label ~ dimension, scale = "free") + 
#   geom_bar(stat = "identity", position = "dodge")



# Change from 2001 to 2011

simple_results %>% 
  spread(year, value) %>% 
  mutate(
    point_change = `2011` - `2001`,
    prop_change = (`2011` - `2001`) / `2001`
    ) %>% 
  ggplot(.,
         aes( y = point_change, x = dimension, group = place, fill = place)
         ) + 
  facet_wrap( ~ attribute_label) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(aes(yintercept = 0)) +
  coord_cartesian(ylim = c(-0.2, 0.2)) + 
  labs(
    title = "Point change in segregations from 2001 to 2011",
    x = "Dimension", y = "Point change in segregation\nscore from 2001 to 2011") + 
  theme(axis.text.x = element_text(angle = 90))


ggsave("figures/point_change_segregations.png", width = 20, height = 20, dpi = 300, units = "cm")


simple_results %>% 
  spread(year, value) %>% 
  mutate(
    point_change = `2011` - `2001`,
    prop_change = (`2011` - `2001`) / `2001`
  ) %>% 
  qplot(
    data = ., 
    x = `2001`, y = point_change, colour = dimension, group = dimension
    ) +
  stat_smooth(method = "lm", se = F)

  
## TO DO: scatterplot of each attribute against each other attribute










simple_results %>% 
  spread(year, value) %>% 
  mutate(
    point_change = `2011` - `2001`,
    prop_change = (`2011` - `2001`) / `2001`
  ) %>% 
  qplot(
    data = ., 
    x = `2001`, y = point_change, colour = place, group = place
  ) +
  stat_smooth(method = "lm", se = F)


simple_results %>% 
  spread(year, value) %>% 
  mutate(
    point_change = `2011` - `2001`,
    prop_change = (`2011` - `2001`) / `2001`
  ) %>% 
  qplot(
    data = ., 
    x = `2001`, y = point_change, colour = attribute, group = attribute
  ) +
  stat_smooth(method = "lm", se = F)




# clusters of attributes  -------------------------------------------------


# socioeconomic

simple_results %>% 
  filter(attribute %in% c("employed", "car", "nssec", "homeowners")) %>% 
  spread(year, value) %>% 
  mutate(
    point_change = `2011` - `2001`,
    prop_change = (`2011` - `2001`) / `2001`
  ) %>% 
  ggplot(.,
         aes( y = point_change, x = dimension, group = place, fill = place)
  ) + 
  facet_wrap( ~ pretty_label) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(aes(yintercept = 0)) +
  coord_cartesian(ylim = c(-0.10, 0.15)) + 
  labs(
    title = "Point change in segregations from 2001 to 2011",
    x = "Dimension", y = "Point change in segregation\nscore from 2001 to 2011") + 
  theme(axis.text.x = element_text(angle = 90))


# ethnnosomethingism

simple_results %>% 
  filter(attribute %in% c("eth", "cob", "religion")) %>% 
  spread(year, value) %>% 
  mutate(
    point_change = `2011` - `2001`,
    prop_change = (`2011` - `2001`) / `2001`
  ) %>% 
  ggplot(.,
         aes( y = point_change, x = dimension, group = place, fill = place)
  ) + 
  facet_wrap( ~ pretty_label) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(aes(yintercept = 0)) +
  coord_cartesian(ylim = c(-0.10, 0.15)) + 
  labs(
    title = "Point change in segregations from 2001 to 2011",
    x = "Dimension", y = "Point change in segregation\nscore from 2001 to 2011") + 
  theme(axis.text.x = element_text(angle = 90))
