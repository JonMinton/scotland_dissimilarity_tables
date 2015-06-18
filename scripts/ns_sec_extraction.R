

# Socioeconomic class

rm(list=ls())


require(readr)
require(readxl)
require(plyr)
require(stringr)
require(tidyr)
require(dplyr)


dta_2001 <- read_csv(
  file="E:/Dropbox/Data/Census/Scotland 2001 Census/from_scrol_dvd/data/oa/KS14A.csv",
  skip = 3
                     )
dta_2001 <- dta_2001 %>% slice(-c(1:2)) 
names(dta_2001)[1] <- "output_area"
dta_2001 <- dta_2001 %>% filter(str_detect(output_area, "^60"))

dta_2001[,-1] <- lapply(dta_2001[,-1], function(x) as.numeric(str_replace(x, "-", "0")))
  
dta_2001_count <- dta_2001

names(dta_2001_count) <- str_replace(names(dta_2001_count), "Percentage of ", "Number of ")

dta_2001_count %>% mutate_each(funs(
  out = round((. /100)  * `ALL PEOPLE AGED 16 - 74`, 0)
  ),
                               -output_area, -`ALL PEOPLE AGED 16 - 74`)


write_csv(dta_2001_count, path="output_data/ns_sec_2001.csv")


2011: KS611SC: NS-Sec
