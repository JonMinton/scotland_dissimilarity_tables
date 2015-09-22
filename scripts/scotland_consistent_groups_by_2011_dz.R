# Construct simplified dissimilarity scores by 2001 datazone for the following:

# car ownership
# SEC
# country of origin
# education
# employment status


rm(list = ls())

require(repmis)
require(readr)


require(car)

require(tidyr)
require(stringr)
require(plyr)
require(dplyr)



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

dz2011_link <- source_DropboxData(
  file = "00462936.csv",
  key = "20ost7i79pq52c4"
) %>% tbl_df

dz2011_link <- dz2011_link %>% 
  rename(
    oa_2011 = OutputArea,
    dz_2011 = DataZone,
    ig_2011 = InterZone, 
    la_2011 = Council
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

oa_dz_link <- oa_dz_link %>% inner_join(dz2011_link)

# car attribute tables ----------------------------------------------------




car_2001 <- read_csv("output_data/carvanownership_2001.csv")
car_2011 <- read_csv("output_data/carvanownership_2011.csv")


# car ownership
# SEC
# country of origin
# education
# employment status


# For 2001, need NRS to standard OA lookup, 
# For both, need to divide into simple white/nonwhite

car_simple_2001 <- car_2001 %>% 
  select(
    oa_nrs = output_area,
    total = `All households`,
    none = `Number of households with cars or vans None`
  ) %>% 
  select(oa_nrs, total, none)

car_simple_2011 <- car_2011 %>% 
  select(
    oa_2011= output_area,
    total = `All households`,
    none = `Number of cars or vans in household: No cars or vans`
  )  %>%  
  select(
    oa_2011,
    total,
    none
  )


car_simple_2001 <- car_simple_2001 %>% 
  inner_join(nrs_oa_link) %>% 
  select(
    oa_2001, 
    total, 
    none
  )



# Final form should be: 

# dz_2011
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- car_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2011, 
    la_name,
    year,
    total,
    none
  ) %>% 
  group_by(
    dz_2011, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    none = sum(none)
  )



dz_simple_2011 <- car_simple_2011 %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2011, 
    la_name,
    year, 
    total,
    none
  ) %>% 
  group_by(
    dz_2011,
    la_name,
    year
  ) %>% 
  summarise(
    total = sum(total),
    none = sum(none)
  )


dz_car <-
  dz_simple_2001 %>% 
  bind_rows(dz_simple_2011)

# Write this out and send to Gavin, Duncan & Jing

write_csv(
  dz_car,
  path = "output_data/2011_datazones/carandvanownership_datazones_2001_and_2011.csv"        
)



# NS SEC tables --------------------------------------------------------


sec_2001 <- read_csv("output_data/ns_sec_2001.csv")

sec_simple_2001 <- sec_2001 %>% 
  select(
    oa_nrs = output_area,
    total = `ALL PEOPLE AGED 16 - 74`,
    student = `Number of people aged 16 - 74: Full-time students`,
    higher_manager = `Number of people aged 16 - 74: Large employers and higher managers`,
    higher_professional = `Number of people aged 16 - 74: Higher professional occupations`,
    lower_manager = `Number of people aged 16 - 74: Lower managerial and professional occupations`,
    semi_routine = `Number of people aged 16 - 74: Semi-routine occupations`,
    routine = `Number of people aged 16 - 74: Routine occupations`
    ) %>% 
  transmute(
    oa_nrs,
    total = total, 
    student = student,
    white_collar = higher_manager + higher_professional + lower_manager,
    blue_collar = semi_routine + routine
  )

sec_simple_2001 <- sec_simple_2001 %>% 
  inner_join(nrs_oa_link) %>% 
  select(
    oa_2001, 
    total, 
    student, 
    white_collar,
    blue_collar
  )




sec_2011 <- read_csv("output_data/ns_sec_2011.csv")

sec_simple_2011 <- sec_2011 %>% 
  select(
    output_area,
    total = `All people aged 16 to 74`,
    student = `L15 Full-time students`,
    higher_manager = `1. Higher managerial, administrative and professional occupations: Total`,
    lower_manager = `2. Lower managerial and professional occupations`,
    semi_routine = `6. Semi-routine occupations`,
    routine = `7. Routine occupations`
  ) %>% 
  transmute(
    oa_2011 = output_area,
    total = total,
    student = student,
    white_collar = higher_manager + lower_manager,
    blue_collar = semi_routine + routine
  )







# Final form should be: 

# dz_2001
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- sec_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2011, 
    la_name,
    year,
    total,
    student,
    white_collar,
    blue_collar
  ) %>% 
  group_by(
    dz_2011, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    student = sum(student),
    white_collar = sum(white_collar),
    blue_collar = sum(blue_collar)
  )



dz_simple_2011 <- sec_simple_2011 %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2011, 
    la_name,
    year, 
    total,
    student,
    white_collar,
    blue_collar
  ) %>% 
  group_by(
    dz_2011,
    la_name,
    year
  ) %>% 
  summarise(
    total = sum(total),
    student = sum(student),
    white_collar = sum(white_collar),
    blue_collar = sum(blue_collar)
  )


dz_sec <-
  dz_simple_2001 %>% 
  bind_rows(dz_simple_2011)

write_csv(
  dz_sec,
  path = "output_data/2011_datazones/socioeconomicclass_datazones_2001_and_2011.csv"        
)



# country of birth --------------------------------------------------------


cob_2001 <- read_csv("output_data/countryofbirth_2001.csv")

cob_simple_2001 <- cob_2001 %>% 
  select(
    oa_nrs = output_area,
    total = `All people`,
    wales = `Number of people born in: Wales`,
    scotland = `Number of people born in: Scotland`,
    england = `Number of people born in: England`,
    ni = `Number of people born in: Northern Ireland`,
    irl = `Number of people born in: Republic of Ireland`,
    other_eu = `Number of people born in: Other EU countries`,
    elsewhere = `Number of people born: Elsewhere`
  ) %>% 
  transmute(
    oa_nrs,
    total, 
    rest_uk = wales + england + ni, 
    rest_eu = irl + other_eu, 
    outside_europe = elsewhere
  )

cob_simple_2001 <- cob_simple_2001 %>% 
  inner_join(nrs_oa_link) %>% 
  select(
    oa_2001,
    total, 
    rest_uk,
    rest_eu,
    outside_europe
  )



cob_2011 <- read_csv("output_data/countryofbirth_2011.csv")

cob_simple_2011 <- cob_2011 %>% 
  select(
    output_area,
    total = `All people`,
    wales = `Wales`,
    scotland = `Scotland`,
    england = `England`,
    ni = `Northern Ireland`,
    irl = `Republic of Ireland`,
    other_eu_old = `Other EU: Member countries in March 2001 (1)`,
    other_eu_new = `Other EU: Accession countries April 2001 to March 2011`,
    elsewhere = `Other countries`
  ) %>% 
  transmute(
    oa_2011 = output_area,
    total, 
    rest_uk = wales + england + ni,
    rest_eu = irl + other_eu_old + other_eu_new,
    outside_europe = elsewhere
  )







# Final form should be: 

# dz_2011
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- cob_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2011, 
    la_name,
    year,
    total,
    rest_uk,
    rest_eu,
    outside_europe
  ) %>% 
  group_by(
    dz_2011, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    rest_uk = sum(rest_uk),
    rest_eu = sum(rest_eu),
    outside_europe = sum(outside_europe)
  )



dz_simple_2011 <- cob_simple_2011 %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2011, 
    la_name,
    year, 
    total,
    rest_uk,
    rest_eu,
    outside_europe
  ) %>% 
  group_by(
    dz_2011,
    la_name,
    year
  ) %>% 
  summarise(
    total = sum(total),
    rest_uk = sum(rest_uk),
    rest_eu = sum(rest_eu),
    outside_europe = sum(outside_europe)
  )


dz_cob <-
  dz_simple_2001 %>% 
  bind_rows(dz_simple_2011)

write_csv(
  dz_cob,
  path = "output_data/2011_datazones/countryofbirth_datazones_2001_and_2011.csv"        
)



# Qualifications --------------------------------------------------------


qual_2001 <- read_csv("output_data/qualifications_2001.csv")

qual_simple_2001 <- qual_2001 %>% 
  select(
    oa_nrs = output_area,
    total = `ALL PEOPLE`,
    noqual = `No qualifications or qualifications outwith these groups`
  ) 


qual_simple_2001 <- qual_simple_2001 %>% 
  inner_join(nrs_oa_link) %>% 
  select(
    oa_2001,
    total, 
    noqual
  )



qual_2011 <- read_csv("output_data/qualifications_2011.csv")

qual_simple_2011 <- qual_2011 %>% 
  select(
    oa_2011 = output_area,
    total = `All people aged 16 and over`,
    noqual = `All people aged 16 and over: No qualifications`
  ) 







# Final form should be: 

# dz_2011
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- qual_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2011, 
    la_name,
    year,
    total,
    noqual
  ) %>% 
  group_by(
    dz_2011, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    noqual = sum(noqual)
  )



dz_simple_2011 <- qual_simple_2011 %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2011, 
    la_name,
    year, 
    total,
    noqual

  ) %>% 
  group_by(
    dz_2011,
    la_name,
    year
  ) %>% 
  summarise(
    total = sum(total),
    noqual = sum(noqual)
    )


dz_qual <-
  dz_simple_2001 %>% 
  bind_rows(dz_simple_2011)

write_csv(
  dz_qual,
  path = "output_data/2011_datazones/noqualifications_datazones_2001_and_2011.csv"        
)




# Employment status --------------------------------------------------------


eact_2001 <- read_csv("output_data/economicactivity_2001.csv")

eact_simple_2001 <- eact_2001 %>% 
  select(
    oa_nrs = output_area,
    total = `All people aged 16 - 74`,
    emp_pt = `Number of people aged 16 - 74 Economically active Employees Part-time`,
    emp_ft = `Number of people aged 16 - 74 Economically active Employees Full-time` ,
    emp_self = `Number of people aged 16 - 74 Economically active Self employed` ,
    unemp = `Number of people aged 16 - 74 Economically active Unemployed` ,
    eact_student = `Number of people aged 16 - 74 Economically active Full-time student` ,
    retired = `Number of people aged 16 - 74 Economically inactive Retired`  ,
    einact_student = `Number of people aged 16 - 74 Economically inactive Student` ,
    inact_home = `Number of people aged 16 - 74 Economically inactive Looking after home/family`,
    inact_sick = `Number of people aged 16 - 74 Economically inactive Permanently sick/disabled`,
    inact_other = `Number of people aged 16 - 74 Economically inactive Other`
  ) %>% 
  transmute(
    oa_nrs,
    total,
    nonretired = total - retired,
    employed = emp_pt + emp_ft + emp_self,
    unemployed = unemp,
    student = eact_student + einact_student,
    sick_disabled = inact_sick
  )


eact_simple_2001 <- eact_simple_2001 %>% 
  inner_join(nrs_oa_link) %>% 
  select(
    oa_2001,
    total,
    nonretired,
    employed ,
    unemployed ,
    student ,
    sick_disabled
  )



eact_2011 <- read_csv("output_data/economicactivity_2011.csv")

eact_simple_2011 <- eact_2011 %>% 
  select(
    oa_2011 = output_area,
    total = `All people aged 16 to 74`,
    emp_pt = `Economically active: Employee: Part-time`,
    emp_ft = `Economically active: Employee: Full-time` ,
    emp_self = `Economically active: Self-employed` ,
    unemp = `Economically active: Unemployed` ,
    eact_student = `Economically active: Full-time student` ,
    retired = `Economically inactive: Retired`  ,
    einact_student = `Economically inactive: Student` ,
    inact_home = `Economically inactive: Looking after home or family`,
    inact_sick = `Economically inactive: Long-term sick or disabled`,
    inact_other = `Economically inactive: Other`
  ) %>% 
  transmute(
    oa_2011,
    total,
    nonretired = total - retired,
    employed = emp_pt + emp_ft + emp_self,
    unemployed = unemp,
    student = eact_student + einact_student,
    sick_disabled = inact_sick
  )






# Final form should be: 

# dz_2011
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- eact_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2011, 
    la_name,
    year,
    total,
    nonretired ,
    employed ,
    unemployed ,
    student ,
    sick_disabled 
  ) %>% 
  group_by(
    dz_2011, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    nonretired =sum(nonretired),
    employed = sum(employed),
    unemployed = sum(unemployed),
    student =sum(student),
    sick_disabled =sum(sick_disabled)
  )



dz_simple_2011 <- eact_simple_2011 %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2011, 
    la_name,
    year,
    total,
    nonretired ,
    employed ,
    unemployed ,
    student ,
    sick_disabled 
  ) %>% 
  group_by(
    dz_2011, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    nonretired =sum(nonretired),
    employed = sum(employed),
    unemployed = sum(unemployed),
    student =sum(student),
    sick_disabled =sum(sick_disabled)
  )


dz_eact <-
  dz_simple_2001 %>% 
  bind_rows(dz_simple_2011)

write_csv(
  dz_eact,
  path = "output_data/2011_datazones/economicactivity_datazones_2001_and_2011.csv"        
)




# Ethnicity ---------------------------------------------------------------



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



# Final form should be: 

# dz_2011
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- eth_simple_2001 %>% 
  inner_join(nrs_oa_link) %>%
  inner_join(oa_dz_link) %>% 
  mutate(year = 2001) %>% 
  select(
    dz_2011, 
    la_name,
    year,
    total,
    nonwhite
  ) %>% 
  group_by(
    dz_2011, 
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
    dz_2011, 
    la_name,
    year, 
    total,
    nonwhite
  ) %>% 
  group_by(
    dz_2011,
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
  path = "output_data/2011_datazones/ethnicity_datazones_2001_and_2011.csv"        
)

