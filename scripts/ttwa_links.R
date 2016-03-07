# Link existing lookup tables to travel to work areas:


# For 2011 datazones
# The source of the travel to work area lookup is: 

#https://data.gov.uk/dataset/travel-to-work-areas-2011-to-travel-to-work-areas-2011_revised-uk-aug-2015-lookup
# Accessed 7/3/2016

# 2011 travel to work areas for whole of UK



# for 2001 datazones 
# the source of the travel to work area look up is: 

#https://data.gov.uk/dataset/local-authority-districts-2013-to-travel-to-work-areas-2001-uk-lookup
# accessed 7/3/2016



rm(list = ls())

require(repmis)
require(readr)
require(readxl)


require(car)

require(tidyr)
require(stringr)
require(plyr)
require(dplyr)

require(ggplot2)
require(ggmap)



# # proof of concept for 2011 datazone lookup -------------------------------
# 
# 
# # read lookup from excel workbook 
# 
# # within sheet LSOA-DZ_SOA_to_TTWA_allocations
# 
# # note this uses dz_2011 codes (starting 6506 rather than 0001)
# 
# ttwa <- read_excel(
#   path = "input_data/lookups/LSOA DZ SOA to TTWA Lookup_V2.xls",
#   sheet = "LSOA_DZ_SOA_to_TTWA_allocations"
#   )
# 
# # Now to look at a single cleaned table, e.g. ethnicity
# 
# dta_eth <- read_csv("output_data/dz_2011/eth_2001.csv")
# 
# 
# dta_eth %>% 
#   inner_join(ttwa, by = c("dz_2011" = "LSOA11CD")) %>% 
#   filter(TTWA11NM %in% c("Aberdeen", "Glasgow", "Edinburgh", "Dundee")) # these are the top four cities. Their TTWAs include around half of Scotland's DZs
# 
# 

# proof of concept for 2001 dzs -------------------------------------------

rm(list = ls())


ttwa <- read_excel(
  path = "input_data/lookups/2001_TTWA_Allocations.xlsx", 
  sheet = "TTWA allocation"
)

dta_eth <- read_csv("output_data/dz_2001/eth_2001.csv")

dta_eth %>% 
  inner_join(ttwa, by = c("dz_2001" = "Area Code")) %>% 
  filter(TTWA_Name %in% c("Aberdeen", "Glasgow", "Edinburgh", "Dundee")) # these are the top four cities. Their TTWAs include around half of Scotland's DZs
# For 2001 the proportion of datazones in these four areas is smaller than for 2011 
# 2001: 2891 dzs 
# 2011: 3223 dzs




