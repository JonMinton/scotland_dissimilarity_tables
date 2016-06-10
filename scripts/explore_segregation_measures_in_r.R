# Some to dos

# 1) redo 2 group categories so that first and second group are 'consistent'
# 2) remerge shapefiles
# 3) rerun analyses below


# Scoping and/or implementation of measures of segregation

rm(list = ls())

# trying out pacman package

#install.packages("pacman")
require(pacman)

pacman::p_load(
  readxl, xlsx, readr, spdep, 
  maptools, rgdal, car, stringr, 
  purrr, tidyr, dplyr, seg, OasisR,
  ggplot2, lattice, tmap
)




load("rdata/shapefiles_in_dataframe.RData")


# fls <- list.files("shapefiles_with_attributes/2grp_2011/ttwa", "\\.shp$")
# 
# task_list <- data_frame(filename =fls) %>%
#   mutate(tmp = str_replace(filename, "\\.shp$", "")) %>%
#   separate(tmp, into = c("place", "attribute", "year"), sep = "_")
# 
# 
# # First, create a function which saves each shapefile into an element of a not-quite dataframe
# 
# 
# get_shapefiles <- function(filename){
#   filename_short <- str_replace(filename, "\\.shp$", "")
#   this_shp <- readOGR(
#     dsn = "shapefiles_with_attributes/2grp_2011/ttwa",
#     layer = filename_short
#   )
# 
#   return(this_shp)
# }
# 
# task_list <- task_list %>%
#   mutate(shp = map(filename, get_shapefiles))
# 
# 
# # extract data portion of each object
# task_list <- task_list %>%
#   mutate(dta = map(shp, ~ slot(., "data")))
# 
# # Now neighbourhood matrix
# 
# get_nhd <- function(x){
# 
#   output <- x %>%
#     poly2nb(., queen = FALSE) %>%
#     nb2mat(., style = "B", zero.policy = T)
#   return(output)
# }
# 
# task_list <- task_list %>%
#   mutate(nhd = map(shp, get_nhd))
# 
# # Now to add the measures one by one
# 
# get_simple_d_and_names <- function(x){
#   attribute_names <- names(x)[c(12, 13)]
#   counts_matrix <- x[,c(12, 13)]
#   dissim <- OasisR::DI(counts_matrix)
#   output <- list(
#     attribute_names = attribute_names,
#     simple_d_1 = dissim[2,1],
#     simple_d_2 = dissim[1,2]
#   )
#   return(output)
# }
# # Calculate indices for each separately
# 
# get_adj_d <- function(x, y){
#   counts_matrix <- x[,c(12, 13)]
#   outputs <- OasisR::Morill(counts_matrix, y)
#   return(outputs)
# }
# # Morill(dta@data[,c("house", "nonhouse")], nhd_matrix)
# 
# 
# 
# 
# task_list <- task_list %>%
#   mutate(
#     d_simple_and_att_names = map(dta, get_simple_d_and_names),
#     d_adj_both = map2(dta, nhd, get_adj_d),
#     d_adj_1 = map_dbl(d_adj_both, ~ .[1]),
#     d_adj_2 = map_dbl(d_adj_both, ~ .[2]),
#     d_simple_1 = map_dbl(d_simple_and_att_names, ~ .[["simple_d_1"]]),
#     d_simple_2 = map_dbl(d_simple_and_att_names, ~ .[["simple_d_2"]]),
#     att_label_1 = map_chr(d_simple_and_att_names, ~ .[["attribute_names"]][1]),
#     att_label_2 = map_chr(d_simple_and_att_names, ~ .[["attribute_names"]][2])
#   ) %>%
#   select(-d_simple_and_att_names, d_adj_both)
# 
# task_list <- task_list %>%
#   mutate(
#     d_simple_and_att_names = map(dta, get_simple_d_and_names),
#     d_adj_both = map2(dta, nhd, get_adj_d),
#     d_adj_1 = map_dbl(d_adj_both, ~ .[1]),
#     d_adj_2 = map_dbl(d_adj_both, ~ .[2]),
#     d_simple_1 = map_dbl(d_simple_and_att_names, ~ .[["simple_d_1"]]),
#     d_simple_2 = map_dbl(d_simple_and_att_names, ~ .[["simple_d_2"]]),
#     att_label_1 = map_chr(d_simple_and_att_names, ~ .[["attribute_names"]][1]),
#     att_label_2 = map_chr(d_simple_and_att_names, ~ .[["attribute_names"]][2])
#   ) %>%
#   select(-d_simple_and_att_names, d_adj_both)
# 
# 
# task_list <- task_list %>%
#   mutate(
#     adj_1 = d_simple_1 - d_adj_1,
#     adj_2 = d_simple_2 - d_adj_2
# 
#   )
# 
# get_xPx <- function(x){
#   counts_matrix <- x[,c(12, 13)]
#   output <- OasisR::xPx(counts_matrix)
#   return(output)
# }
# 
# 
# task_list <- task_list %>%
#   mutate(xPx = map(dta, get_xPx)) %>%
#   mutate(
#     xPx_1 = map_dbl(xPx, ~ .[1]),
#     xPx_2 = map_dbl(xPx, ~ .[2])
#   ) %>%
#   select(-xPx)
# 
# 
# get_Eta2 <- function(x){
#   counts_matrix <- x[,c(12, 13)]
#   output <- OasisR::Eta2(counts_matrix) %>% .[1]
#   return(output)
# }
# 
# task_list <- task_list %>%
#   mutate(Eta2 = map_dbl(dta, get_Eta2))
# 
# get_rce <- function(dta, shp){
#   counts_matrix <- dta[,c(12, 13)]
#   cntr <- which(dta$centre == 1)
#   distc <- distcenter(shp, center = cntr)
#   rce <- RCE(
#     x = counts_matrix,
#     dc = distc,
#     center = cntr
#   )
#   output <- rce[2,1]
#   return(output)
# }
# 
# task_list <- task_list %>%
#   mutate(rce = map2_dbl(dta, shp, get_rce))
# 
# 
# # Bespoke concentration measure using same approach as RCE
# 
# get_rcon <- function(dta, shp){
#   # rcon (borrows code from OasisR::RCE)
#   rcon <- function(x, dens){
#     x <- as.matrix(x)
#     result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
#     varTotal <- colSums(x)
#     xprovi <- cbind(x, dens)
#     xprovi <- xprovi[order(xprovi[, ncol(xprovi)], decreasing = T), ]
#     # highest to lowest density, so should be decreasing
#     xprovi <- as.data.frame(xprovi)
#     for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
#       XI1 <- cumsum(xprovi[, k1])[1:(nrow(xprovi) - 1)]/varTotal[k1]
#       XI <- cumsum(xprovi[, k1])[2:nrow(xprovi)]/varTotal[k1]
#       YI1 <- cumsum(xprovi[, k2])[1:(nrow(xprovi) - 1)]/varTotal[k2]
#       YI <- cumsum(xprovi[, k2])[2:nrow(xprovi)]/varTotal[k2]
#       result[k1, k2] <- XI1 %*% YI - XI %*% YI1
#     }
#     return(result)
#   }
# 
#   dens <- tmap::calc_densities(shp, var = "total")
# 
#   counts_matrix <- dta[,c(12, 13)]
#   output <- rcon(counts_matrix, dens) %>% .[2, 1]
#   return(output)
# }
# 
# task_list <- task_list %>%
#   mutate(rcon = map2_dbl(dta, shp, get_rcon))
# 
# 
# save(task_list, file = "rdata/shapefiles_in_dataframe.RData")


# Create alternative attribute labels 

pretty_att_label_1 <- c(
  house = "Lives in a house",
  some = "Owns at least one car",
  scotland = "Born in Scotland",
  employd = "Is in employment",
  white = "Is White",
  good = "Reports good general health",
  owned = "Owns home",
  active = "Is economically active",
  no_llti = "Does not report LLTI",
  married = "Is married",
  higher = "Higher managerial/professional occupation",
  pensinr = "Is a pensioner",
  religis = "Has a religion",
  student = "Is a student"
)

pretty_att_label_2 <- c(
  nonhouse = "Does not live in a house",
  none = "Has no car in household",
  nonscot = "Is not Scottish born",
  nnmployed = "Is not employed",
  nonwhite = "Is not White",
  not_good = "Does not report good health",
  nonowned = "Does not own accommodation",
  inactive = "Is economically inactive",
  llti = "Reports a limiting long term illness",
  single = "Is not married",
  lower = "Has less than professional/managerial occupation",
  nnpnsnr = "Is not a pensioner",
  nonrlgs = "Does not report a religion",
  nonstudent = "Is not a student"
)


simple_results <- task_list %>% 
  select(place, attribute, year, 
         attribute_label = att_label_1,
         unevenness = d_simple_1, 
         clustering = adj_1, 
         isolation = Eta2, 
         centralisation = rce, 
         concentration = rcon
  ) %>% 
  mutate(centralisation = -1 * centralisation, concentration = -1 * concentration) %>% 
  gather(key = "dimension", value = "value", unevenness:concentration)

simple_results <- simple_results %>% 
  mutate(
    dimension = factor(
      dimension, 
      levels = c(
        "unevenness", "isolation", "clustering", 
        "centralisation", "concentration")
    ),
  pretty_attribute_label = pretty_att_label_1[attribute_label]
  )


# Using splot within lattice
#http://www.statmethods.net/graphs/scatterplot.html

simple_results  %>% spread(dimension, value)  -> tmp


# By year

png(filename = "figures/scatterplot_year.png", width= 30, height = 30, units = "cm", res = 300)

trellis.par.set("superpose.symbol", list(pch = 0:1))
super.sym <- trellis.par.get("superpose.symbol")
p <- splom(~tmp[6:10], groups = year, data = tmp,
      panel =  function(x, y, ...) { 
        panel.xyplot(x, y,  ...)
        panel.loess(x, y, ...)
        panel.lmline(x, y, ...)
      },
      key = list(title = "Values in 2001 and 2011",
                 columns = 2, 
                 points = list(pch = super.sym$pch[1:2],
                               col = super.sym$col[1:2]),
                 text = list(c("2001", "2011")))
      
      
      )

print(p)
dev.off()
p


# Now differences from 2001 to 2011

simple_results  %>% 
  spread(year, value)  %>% 
  mutate(change = `2011` - `2001`)  %>% 
  select(-`2001`, -`2011`)  %>% 
  spread(dimension, change)  -> tmp2

# By place - 

png(filename = "figures/scatterplot_change_ttwa.png", width= 30, height = 30, units = "cm", res = 300)

trellis.par.set("superpose.symbol", list(pch = 0:6))
super.sym <- trellis.par.get("superpose.symbol")

p <- splom(~tmp2[5:9], groups = place, data = tmp2,
      panel = function(x, y, ...) { 
        panel.xyplot(x, y,  ...)
        panel.loess(x, y, ...)
        panel.lmline(x, y, ...)
      },
      key = list(title = "Values by TTWA",
                 columns = 4, rows = 2,
                 points = list(pch = super.sym$pch[1:7],
                               col = super.sym$col[1:7]),
                 text = list(unique(tmp2$place)))
      
)

print(p)
dev.off()
p


# Show all on one very complex figure

png("figures/scatter_all_attributes.png", width = 60, height = 60, units = "cm", res = 300)

trellis.par.set("superpose.symbol", list(pch = 0:6))
super.sym <- trellis.par.get("superpose.symbol")

splom(~tmp2[5:9] | attribute, groups = place, data = tmp2,
      key = list(title = "Values by TTWA",
                 columns = 4, rows = 3,
                 points = list(pch = super.sym$pch[1:7],
                               col = super.sym$col[1:7]),
                 text = list(unique(tmp2$place)))
      
      )
dev.off()




# by attribute - socioeconomic 

# socioeconomic

tmp3 <- tmp2 %>% 
  filter(attribute %in% c("employed", "car", "nssec", "homeowners"))

trellis.par.set("superpose.symbol", list(pch = 0:3))
super.sym <- trellis.par.get("superpose.symbol")

png("figures/scatter_socioeconomic.png", width = 30, height = 30, res = 300, units = "cm")
splom(~tmp3[5:9], groups = pretty_attribute_label, data = tmp3,
      panel = function(x, y, ...) { 
        panel.xyplot(x, y,  ...)
        panel.loess(x, y, ...)
        panel.lmline(x, y, ...)
      },
      key = list(title = "Socioeconomic attribute",
                 columns = 2, rows = 2,
                 points = list(pch = super.sym$pch[1:4],
                               col = super.sym$col[1:4]),
                 text = list(unique(tmp3$pretty_attribute_label))))

dev.off()


# ethnosreligious 

tmp3 <- tmp2 %>% 
  filter(attribute %in% c("eth", "cob", "religion"))

trellis.par.set("superpose.symbol", list(pch = 0:2))
super.sym <- trellis.par.get("superpose.symbol")

png("figures/scatter_ethnoreligious.png", width = 30, height = 30, res = 300, units = "cm")

splom(~tmp3[5:9], groups = pretty_attribute_label, data = tmp3,
      panel = function(x, y, ...) { 
        panel.xyplot(x, y,  ...)
        panel.loess(x, y, ...)
        panel.lmline(x, y, ...)
      },
      key = list(title = "Ethnoreligious",
                 columns = 3,
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3]),
                 text = list(unique(tmp3$pretty_attribute_label)))
      )
dev.off()


# tenure and place 

tmp3 <- tmp2 %>% 
  filter(attribute %in% c("accom", "car", "homeowners"))

trellis.par.set("superpose.symbol", list(pch = 0:2))
super.sym <- trellis.par.get("superpose.symbol")

png("figures/scatter_tenureplace.png", width = 30, height = 30, res = 300, units = "cm")

splom(~tmp3[5:9], groups = pretty_attribute_label, data = tmp3,
      panel = function(x, y, ...) { 
        panel.xyplot(x, y,  ...)
        panel.loess(x, y, ...)
        panel.lmline(x, y, ...)
      },
      key = list(title = "Tenure and place",
                 columns = 3,
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3]),
                 text = list(unique(tmp3$pretty_attribute_label))))
dev.off()


# health and status

tmp3 <- tmp2 %>% 
  filter(attribute %in% c("employed", "generalhealth", "inactive", "nssec"))

trellis.par.set("superpose.symbol", list(pch = 0:3))
super.sym <- trellis.par.get("superpose.symbol")

png("figures/scatter_healthstatus.png", width = 30, height = 30, res = 300, units = "cm")

splom(~tmp3[5:9], groups = pretty_attribute_label, data = tmp3,
      panel = function(x, y, ...) { 
        panel.xyplot(x, y,  ...)
        panel.loess(x, y, ...)
        panel.lmline(x, y, ...)
      },
      key = list(title = "Health and status",
                 columns = 2,
                 points = list(pch = super.sym$pch[1:4],
                               col = super.sym$col[1:4]),
                 text = list(unique(tmp3$pretty_attribute_label))))
dev.off()


# Demographic

tmp3 <- tmp2 %>% 
  filter(attribute %in% c("inactive", "maritalstatus", "pensioners", "students"))

trellis.par.set("superpose.symbol", list(pch = 0:3))
super.sym <- trellis.par.get("superpose.symbol")

png("figures/scatter_demographic.png", width = 30, height = 30, res = 300, units = "cm")

splom(~tmp3[5:9], groups = pretty_attribute_label, data = tmp3,
      panel = function(x, y, ...) { 
        panel.xyplot(x, y,  ...)
        panel.loess(x, y, ...)
        panel.lmline(x, y, ...)
      },
      key = list(title = "Demographic",
                 columns = 2,
                 points = list(pch = super.sym$pch[1:4],
                               col = super.sym$col[1:4]),
                 text = list(unique(tmp3$pretty_attribute_label))))
dev.off()



# Now how best to rep



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





