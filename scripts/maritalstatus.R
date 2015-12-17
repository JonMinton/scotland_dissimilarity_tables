# Marital Status


rm(list=ls())


require(readr)
require(readxl)
require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

#2001 KS04

dta_2001 <- read_csv(
  file="E:/Dropbox/Data/Census/Scotland 2001 Census/from_scrol_dvd/data/oa/KS04.csv",
  skip = 4,
  col_types = "cccccccc"
)


names(dta_2001)[1] <- "output_area"
dta_2001 <- dta_2001 %>% filter(str_detect(output_area, "^60"))

dta_2001[,-1] <- lapply(dta_2001[,-1], function(x) as.numeric(str_replace(x, "-", "0")))

dta_2001_count <- dta_2001


names(dta_2001_count) <- str_replace(names(dta_2001_count), "Percentage of ", "Number of ")

dta_2001_count <- dta_2001_count %>% mutate_each(funs(
  out = round((. /100)  * `All people aged 16 and over`, 0)
),
-output_area, -`All people aged 16 and over`)

write_csv(dta_2001_count, path="output_data/maritalstatus_2001.csv")


#2011: a - KS103SC:

dta_2011 <- read_csv(
  file="E:/Dropbox/Data/Census/Scotland 2011 Census/unzipped/2ablk/Output Area/KS103SC.csv"
)

names(dta_2011)[1] <- "output_area"

dta_2011 <- dta_2011 %>% filter(str_detect(output_area, "^S00"))
dta_2011[,-1] <- lapply(dta_2011[,-1], function(x) as.numeric(str_replace(str_replace(str_trim(x), ",", ""), "-", "0")))
dta_2011[,-1] <- lapply(dta_2011[,-1], function(x) {tmp <- is.na(x); x[tmp] <- 0; return(x)})

write_csv(dta_2011, path="output_data/maritalstatus_2011.csv")
