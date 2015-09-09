# Contruct Country of Origin data for English cities


rm(list = ls())

require(repmis)
require(readr)


require(car)

require(tidyr)
require(stringr)
require(plyr)
require(dplyr)






# English data ------------------------------------------------------------


# I now need to do something similar with English data. 

# I have extracted 2001 census data by LSOA for England & Wales 
# using infuse, which seems relatively painless. 


cob_enw_2001_nms <- source_DropboxData(
  file = "Data_CTRBIR_UNIT.csv",
  key = "dofvm29nruxgd0n/"
)
nms <- names(cob_enw_2001_nms)[1:5]
rm(cob_enw_2001_nms)

cob_enw_2001 <- source_DropboxData(
  file = "Data_CTRBIR_UNIT.csv",
  key = "dofvm29nruxgd0n/",
  skip = 1
) %>% tbl_df

cob_enw_2001
names(cob_enw_2001)[1:5] <- nms

rm(nms)

    
# Using a definition of Greater London from the website
#http://data.london.gov.uk/dataset/lsoa-atlas

# 2001 lsoa definitions

lsoas_in_greater_london <- source_DropboxData(
  file = "lsoas_in_greater_london_2001.csv",
  key = "hwrz9qloiotdnev"  
) %>% tbl_df


cob_enw_2001 <- cob_enw_2001 %>% 
  select(
    -CDU_ID,
    -GEO_TYPE,
    -GEO_TYP2,
    -V11
  ) %>% 
  rename(
    lsoa_2001 = GEO_CODE,
    label = GEO_LABEL,
    total = `Country of birth : All people - Unit : People`,
    uk = `Country of birth : Europe \\ United Kingdom  - Unit : People`
  )  %>% 
  select(
    lsoa_2001,
    label,
    total,
    uk
  ) %>% 
  mutate(
    nonuk = total - uk
  )


cob_enw_2001$city <- NA

tmp <- lsoas_in_greater_london$Codes
cob_enw_2001$city[cob_enw_2001$lsoa_2001 %in% tmp] <- "greater_london"
rm(tmp)

tmp <- str_detect(cob_enw_2001$label, "Sheffield")
cob_enw_2001$city[tmp] <- "sheffield"
rm(tmp)

tmp <- str_detect(cob_enw_2001$label, "York")
cob_enw_2001$city[tmp] <- "york"
rm(tmp)

tmp <- str_detect(cob_enw_2001$label, "Manchester")
cob_enw_2001$city[tmp] <- "manchester"
rm(tmp)



# Assuming LSOAs are consistent as they used the same prefix, let's combine

tmp <- cob_enw_2001 %>% 
  mutate(year = 2001) %>% 
  select(
    lsoa = lsoa_2001, 
    year,
    city,
    label, 
    total,
    nonuk
  )

cob_enw <- tmp 

# Now to write this out

write_csv(cob_enw, path = "output_data/countryofbirth_lsoa_2001_only.csv")
