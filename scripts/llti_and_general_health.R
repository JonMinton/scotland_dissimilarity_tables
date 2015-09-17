

# 2001, general health

rm(list=ls())


require(readr)
require(readxl)
require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(repmis)
# 2001, general health, oa

# General health QS302SC
# Long term health QS303SC

dta_2001 <- read_csv(
  file="E:/Dropbox/Data/Census/Scotland 2001 Census/from_scrol_dvd/data/oa/uv20.csv",
  skip = 4,
  col_types = list(
    col_character(),     col_character(),     col_character(),
    col_character(),    col_character()
  )
)


names(dta_2001)[1] <- "output_area"
dta_2001 <- dta_2001 %>% filter(str_detect(output_area, "^60"))

dta_2001[,-1] <- lapply(dta_2001[,-1], function(x) as.numeric(str_replace(x, "-", "0")))

dta_2001_count <- dta_2001


write_csv(dta_2001_count, path="output_data/generalhealth_2001.csv")


#2011 General health QS302SC, oa

dta_2011 <- read_csv(
  file="E:/Dropbox/Data/Census/Scotland 2011 Census/unzipped/2ablk/Output Area/QS302SC.csv",
  col_types = list(
    col_character(),     col_character(),     col_character(),
    col_character(),    col_character(), col_character(),
    col_character()
  )
)

names(dta_2011)[1] <- "output_area"

dta_2011 <- dta_2011 %>% filter(str_detect(output_area, "^S00"))
dta_2011[,-1] <- lapply(dta_2011[,-1], function(x) as.numeric(str_replace(str_trim(x), "-", "0")))
dta_2011[,-1] <- lapply(dta_2011[,-1], function(x) {tmp <- is.na(x); x[tmp] <- 0; return(x)})

write_csv(dta_2011, path="output_data/generalhealth_2011.csv")


# General health QS302SC

# llti 2001

dta_2001 <- read_csv(
  file="E:/Dropbox/Data/Census/Scotland 2001 Census/from_scrol_dvd/data/oa/uv22.csv",
  skip = 4,
  col_types = list(
    col_character(),     col_character(),     
    col_character(),    col_character()
  )
)


names(dta_2001)[1] <- "output_area"
dta_2001 <- dta_2001 %>% filter(str_detect(output_area, "^60"))

dta_2001[,-1] <- lapply(dta_2001[,-1], function(x) as.numeric(str_replace(x, "-", "0")))

dta_2001_count <- dta_2001


write_csv(dta_2001_count, path="output_data/llti_2001.csv")


# Long term health QS303SC


dta_2011 <- read_csv(
  file="E:/Dropbox/Data/Census/Scotland 2011 Census/unzipped/2ablk/Output Area/QS303SC.csv",
  col_types = list(
    col_character(),     col_character(),     col_character(),
    col_character(),    col_character()
  )
)

names(dta_2011)[1] <- "output_area"

dta_2011 <- dta_2011 %>% filter(str_detect(output_area, "^S00"))
dta_2011[,-1] <- lapply(dta_2011[,-1], function(x) as.numeric(str_replace(str_trim(x), "-", "0")))
dta_2011[,-1] <- lapply(dta_2011[,-1], function(x) {tmp <- is.na(x); x[tmp] <- 0; return(x)})

write_csv(dta_2011, path="output_data/llti_2011.csv")


# Linking tables ----------------------------------------------------------


nrs_oa_link <- source_DropboxData(
  file = "OUTPUT_AREA_2001_LOOKUP.csv",
  key = "39wszvlpxy4qvpf"
) %>% tbl_df

nrs_oa_link <- nrs_oa_link %>% 
  rename(oa_2001 = OutputArea2001Code, oa_nrs = NRSoldOutputArea2001Code)

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


# general health - link 2001 oa to 2001 dz -------------------------------------------------

gh_2001 <- read_csv("output_data/generalhealth_2001.csv")
gh_2011 <- read_csv("output_data/generalhealth_2011.csv")
# For 2001, need NRS to standard OA lookup, 
# For both, need to divide into simple white/nonwhite

gh_simple_2001 <- gh_2001 %>% 
  select(
    oa_nrs = output_area,
    total = `ALL PEOPLE`,
    not_good = `Not Good Health`
  ) %>% 
  select(oa_nrs, total, not_good)

gh_simple_2011 <- gh_2011 %>% 
  select(
    oa_2011= output_area,
    total = `All people`,
    fair = `Fair health`,
    bad = `Bad health`,
    very_bad = `Very bad health`
  )  %>%  
  mutate(
    not_good = fair + bad + very_bad
  ) %>% 
  select(
    oa_2011, 
    total,
    not_good
  )





gh_simple_2001 <- gh_simple_2001 %>% 
  inner_join(nrs_oa_link) %>% 
  select(
    oa_2001, 
    total, 
    not_good
  )



# Final form should be: 

# dz_2001
# la_name
# year
# total
# not_good


dz_simple_2001 <- gh_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2001, 
    la_name,
    year,
    total,
    not_good
  ) %>% 
  group_by(
    dz_2001, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    not_good = sum(not_good)
  )



dz_simple_2011 <- gh_simple_2011 %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2001, 
    la_name,
    year, 
    total,
    not_good
  ) %>% 
  group_by(
    dz_2001,
    la_name,
    year
  ) %>% 
  summarise(
    total = sum(total),
    not_good = sum(not_good)
  )


dz_gh <-
  dz_simple_2001 %>% 
  bind_rows(dz_simple_2011)

# Write this out and send to Gavin, Duncan & Jing

write_csv(
  dz_gh,
  path = "output_data/generalhealth_datazones_2001_and_2011.csv"        
)



# LLTI at dz level

# general health - link 2001 oa to 2001 dz -------------------------------------------------

llti_2001 <- read_csv("output_data/llti_2001.csv")
llti_2011 <- read_csv("output_data/llti_2011.csv")
# For 2001, need NRS to standard OA lookup, 
# For both, need to divide into simple white/nonwhite

llti_simple_2001 <- llti_2001 %>% 
  select(
    oa_nrs = output_area,
    total = `ALL PEOPLE`,
    llti = `With a limiting long-term illness`
  ) %>% 
  select(oa_nrs, total, llti)

llti_simple_2011 <- llti_2011 %>% 
  select(
    oa_2011= output_area,
    total = `All people`,
    llti_lot = `Day-to-day activities limited a lot`,
    llti_little = `Day-to-day activities limited a little`
  )  %>%  
  mutate(
    llti = llti_lot + llti_little
  ) %>% 
  select(
    oa_2011, 
    total,
    llti
  )



llti_simple_2001 <- llti_simple_2001 %>% 
  inner_join(nrs_oa_link) %>% 
  select(
    oa_2001, 
    total, 
    llti
  )



# Final form should be: 

# dz_2001
# la_name
# year
# total
# llti


dz_simple_2001 <- llti_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2001, 
    la_name,
    year,
    total,
    llti
  ) %>% 
  group_by(
    dz_2001, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    llti = sum(llti)
  )



dz_simple_2011 <- llti_simple_2011 %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2001, 
    la_name,
    year, 
    total,
    llti
  ) %>% 
  group_by(
    dz_2001,
    la_name,
    year
  ) %>% 
  summarise(
    total = sum(total),
    llti = sum(llti)
  )


dz_llti <-
  dz_simple_2001 %>% 
  bind_rows(dz_simple_2011)

write_csv(
  dz_llti,
  path = "output_data/llti_datazones_2001_and_2011.csv"        
)
