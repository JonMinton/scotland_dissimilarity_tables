# Construct Glasgow and Edinburgh datazone ethnicity tables


rm(list = ls())

require(repmis)
require(readr)


require(car)

require(tidyr)
require(stringr)
require(plyr)
require(dplyr)




# Start by constructing ethnicity counts for the whole of Scotland, then use LA 
# lookup to match to the cities


eth_2001 <- read_csv("output_data/ethnicity_2001.csv")
eth_2011 <- read_csv("output_data/ethnicity_2011.csv")


# For 2001, need NRS to standard OA lookup, 
# For both, need to divide into simple white/nonwhite

eth_simple_2001 <- eth_2001 %>% 
  select(
    oa_nrs = output_area,
    total = `ALL PEOPLE`,
    contains("White")
  ) %>% 
  mutate(
    white = `Number White Scottish` + `Number Other White British` + `Number White Irish` + `Number Other White` ,
    nonwhite = total - white
  ) %>% 
  select(oa_nrs, total, nonwhite)

eth_simple_2011 <- eth_2011 %>% 
  select(
    oa_2011= output_area,
    total = `All people`,
    white = `White`
    ) %>% 
  mutate(
    nonwhite = total - white
  ) %>% 
  select(
    oa_2011,
    total,
    nonwhite
  )

# Now I need to link NRS to standard OA code using the lookup table used previously

nrs_oa_link <- source_DropboxData(
  file = "OUTPUT_AREA_2001_LOOKUP.csv",
  key = "39wszvlpxy4qvpf"
  ) %>% tbl_df

nrs_oa_link <- nrs_oa_link %>% 
  rename(oa_2001 = OutputArea2001Code, oa_nrs = NRSoldOutputArea2001Code)

eth_simple_2001 <- eth_simple_2001 %>% 
  inner_join(nrs_oa_link) %>% 
  select(
    oa_2001, 
    total, 
    nonwhite
  )

# link to 2001 datazones

big_link <- source_DropboxData(
  file =  "Census_2011_Lookup__OA_TO_HIGHER_AREAS.csv",
  key =   "95x5ozuw0c6xgxk"
) %>% tbl_df

oa_dz_link <- big_link %>% 
  select(
    oa_2001 = OutputArea2001Code,
    oa_2011 = OutputArea2011Code,
    dz_2001 = Datazone2001Code, 
    la_2011 = CouncilArea2011Code # this will allow links to specific cities later 
  )


la_2011_codes <- source_DropboxData(
  file = "LAD_2012_UK_NC.csv",
  key = "86em2kfrq0xxmpk"
) %>% tbl_df

la_2011_codes <- la_2011_codes %>% 
  select(
    la_2011 = LAD12CD, 
    la_name = LAD12NM
  )

oa_dz_link <- oa_dz_link %>% inner_join(la_2011_codes)
  

# Final form should be: 

# dz_2001
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- eth_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2001, 
    la_name,
    year,
    total,
    nonwhite
  ) %>% 
  group_by(
    dz_2001, 
    la_name, 
    year
    ) %>%
  summarise(
    total = sum(total),
    nonwhite = sum(nonwhite)
  )

  

dz_simple_2011 <- eth_simple_2011 %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2001, 
    la_name,
    year, 
    total,
    nonwhite
  ) %>% 
  group_by(
    dz_2001,
    la_name,
    year
  ) %>% 
  summarise(
    total = sum(total),
    nonwhite = sum(nonwhite)
  )
    

dz_eth <-
  dz_simple_2001 %>% 
  bind_rows(dz_simple_2011)

# Write this out and send to Gavin, Duncan & Jing

write_csv(
  dz_eth,
  path = "output_data/ethnicity_datazones_2001_and_2011.csv"        
          )



# English data ------------------------------------------------------------


# I now need to do something similar with English data. 

# I have extracted 2001 census data by LSOA for England & Wales 
# using infuse, which seems relatively painless. 


eth_enw_2001_nms <- source_DropboxData(
  file = "Data_ETHGEW_UNIT.csv",
  key = "xco1zn814824brp"
)
nms <- names(eth_enw_2001_nms)[1:5]
rm(eth_enw_2001_nms)

eth_enw_2001 <- source_DropboxData(
  file = "Data_ETHGEW_UNIT.csv",
  key = "xco1zn814824brp",
  skip = 1
) %>% tbl_df

eth_enw_2001
names(eth_enw_2001)[1:5] <- nms

rm(nms)

    
# Using a definition of Greater London from the website
#http://data.london.gov.uk/dataset/lsoa-atlas

# 2001 lsoa definitions

lsoas_in_greater_london <- source_DropboxData(
  file = "lsoas_in_greater_london_2001.csv",
  key = "hwrz9qloiotdnev"  
) %>% tbl_df


eth_enw_2001 <- eth_enw_2001 %>% 
  select(
    -CDU_ID,
    -GEO_TYPE,
    -GEO_TYP2,
    -V12
  ) %>% 
  rename(
    lsoa_2001 = GEO_CODE,
    label = GEO_LABEL,
    total = `Ethnic group (England and Wales) : All people - Unit : People`,
    white = `Ethnic group (England and Wales) : White - Unit : People`
  )  %>% 
  select(
    lsoa_2001,
    label,
    total,
    white
  ) %>% 
  mutate(
    nonwhite = total - white
  )


eth_enw_2001$city <- NA

tmp <- lsoas_in_greater_london$Codes
eth_enw_2001$city[eth_enw_2001$lsoa_2001 %in% tmp] <- "greater_london"
rm(tmp)

tmp <- str_detect(eth_enw_2001$label, "Sheffield")
eth_enw_2001$city[tmp] <- "sheffield"
rm(tmp)

tmp <- str_detect(eth_enw_2001$label, "York")
eth_enw_2001$city[tmp] <- "york"
rm(tmp)

tmp <- str_detect(eth_enw_2001$label, "Manchester")
eth_enw_2001$city[tmp] <- "manchester"
rm(tmp)


# Now to do the same kind of thing for 2011


eth_enw_2011_nms <- source_DropboxData(
  file = "Data_ETHGRP_UNIT.csv",
  key = "d73y4ck863vnekh"
) %>% tbl_df

nms <- names(eth_enw_2011_nms)[1:5]
rm(eth_enw_2011_nms)

eth_enw_2011 <- source_DropboxData(
  file = "Data_ETHGRP_UNIT.csv",
  key = "d73y4ck863vnekh",
  skip = 1
) %>% tbl_df

names(eth_enw_2011)[1:5] <- nms

rm(nms)


eth_enw_2011 <- eth_enw_2011 %>% 
  select(
    -CDU_ID,
    -GEO_TYPE,
    -GEO_TYP2,
    -V25
  ) %>% 
  rename(
    lsoa_2011 = GEO_CODE,
    label = GEO_LABEL
  )  %>% 
  select(
    lsoa_2011,
    label,
    contains("Ethnic group : All"),
    contains("Ethnic group : White")
  ) %>%
  rename(
    total = `Ethnic group : All categories: Ethnic group - Unit : Persons`,
    white_uk = `Ethnic group : White\\ English/Welsh/Scottish/Northern Irish/British - Unit : Persons`,
    white_irish = `Ethnic group : White\\ Irish - Unit : Persons`,
    white_gypsy = `Ethnic group : White\\ Gypsy or Irish Traveller - Unit : Persons`,
    white_other = `Ethnic group : White\\ Other White - Unit : Persons`
  ) %>% 
   mutate(
    white = white_uk + white_irish + white_gypsy + white_other, 
    nonwhite = total - white
  ) %>% 
  select(
    lsoa_2011,
    label,
    white, 
    nonwhite
  )


eth_enw_2011$city <- NA

tmp <- lsoas_in_greater_london$Codes
eth_enw_2011$city[eth_enw_2011$lsoa_2011 %in% tmp] <- "greater_london"
rm(tmp)

tmp <- str_detect(eth_enw_2011$label, "Sheffield")
eth_enw_2011$city[tmp] <- "sheffield"
rm(tmp)

tmp <- str_detect(eth_enw_2011$label, "York")
eth_enw_2011$city[tmp] <- "york"
rm(tmp)

tmp <- str_detect(eth_enw_2011$label, "Manchester")
eth_enw_2011$city[tmp] <- "manchester"
rm(tmp)

# Assuming LSOAs are consistent as they used the same prefix, let's combine

tmp <- eth_enw_2001 %>% 
  mutate(year = 2001) %>% 
  select(
    lsoa = lsoa_2001, 
    year,
    city,
    label, 
    white,
    nonwhite
  )

tmp2 <- eth_enw_2011 %>% 
  mutate(year = 2011) %>% 
  select(
    lsoa = lsoa_2011,
    year,
    city,
    label,
    white,
    nonwhite
  )

eth_enw <- tmp %>% bind_rows(tmp2)

# Now to write this out

write_csv(eth_enw, path = "output_data/ethnicity_lsoa_2001_and_2011.csv")
