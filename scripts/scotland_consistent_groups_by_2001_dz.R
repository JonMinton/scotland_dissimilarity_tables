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

# dz_2001
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- car_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2001, 
    la_name,
    year,
    total,
    none
  ) %>% 
  group_by(
    dz_2001, 
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
    dz_2001, 
    la_name,
    year, 
    total,
    none
  ) %>% 
  group_by(
    dz_2001,
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
  path = "output_data/carandvanownership_datazones_2001_and_2011.csv"        
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
    dz_2001, 
    la_name,
    year,
    total,
    student,
    white_collar,
    blue_collar
  ) %>% 
  group_by(
    dz_2001, 
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
    dz_2001, 
    la_name,
    year, 
    total,
    student,
    white_collar,
    blue_collar
  ) %>% 
  group_by(
    dz_2001,
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
  path = "output_data/socioeconomicclass_datazones_2001_and_2011.csv"        
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

# dz_2001
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- cob_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2001, 
    la_name,
    year,
    total,
    rest_uk,
    rest_eu,
    outside_europe
  ) %>% 
  group_by(
    dz_2001, 
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
    dz_2001, 
    la_name,
    year, 
    total,
    rest_uk,
    rest_eu,
    outside_europe
  ) %>% 
  group_by(
    dz_2001,
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
  path = "output_data/countryofbirth_datazones_2001_and_2011.csv"        
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

# dz_2001
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- qual_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2001, 
    la_name,
    year,
    total,
    noqual
  ) %>% 
  group_by(
    dz_2001, 
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
    dz_2001, 
    la_name,
    year, 
    total,
    noqual

  ) %>% 
  group_by(
    dz_2001,
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
  path = "output_data/noqualifications_datazones_2001_and_2011.csv"        
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

# dz_2001
# la_name
# year
# total
# nonwhite


dz_simple_2001 <- eact_simple_2001 %>% 
  inner_join(oa_dz_link) %>%
  mutate(year = 2001) %>% 
  select(
    dz_2001, 
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
    dz_2001, 
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
    dz_2001, 
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
    dz_2001, 
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
  path = "output_data/economicactivity_datazones_2001_and_2011.csv"        
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




# Four group ethnicity classification -------------------------------------
# requested by Nick & Jin November 2015

# The desired categories are: 
# 1) White
# 2) Chinese
# 3) Pakistani
# 4) Other

# > names(eth_2001)
# [1] "output_area"                                                                                                    
# [2] "ALL PEOPLE"                                                                                                     
# [WHITE] [3] "Number White Scottish"                                                                                          
# [WHITE] [4] "Number Other White British"                                                                                     
# [WHITE] [5] "Number White Irish"                                                                                             
# [WHITE] [6] "Number Other White"                                                                                             
# [OTHER] [7] "Number Indian"                                                                                                  
# [PAKISTANI] [8] "Number Pakistani"                                                                                               
# [OTHER] [9] "Number Bangladeshi"                                                                                             
# [OTHER] [10] "Number Other South Asian"                                                                                       
# [CHINESE] [11] "Number Chinese"                                                                                                 
# [OTHER] [12] "Number Caribbean"                                                                                               
# [OTHER] [13] "Number African"                                                                                                 
# [OTHER] [14] "Number Black Scottish or Other Black"                                                                           
# [OTHER] [15] "Number Any Mixed Background"                                                                                    
# [OTHER] [16] "Number Other Ethnic Group"                                                                                      
# [NA] [17] "Number of people aged 3 and over, who understand, speak, read or write Gaelic and who were born in Scotland"    
# [NA] [18] "Number of people aged 3 and over, who understand, speak, read or write Gaelic and who were not born in Scotland"

eth_2001_4grp <- eth_2001 %>% transmute(
  oa_nrs = output_area, 
  all_check = `ALL PEOPLE`,
  white = `Number White Scottish` + `Number Other White British` + 
    `Number White Irish` + `Number Other White`, 
  pakistani = `Number Pakistani`, 
  chinese = `Number Chinese`, 
  other = `Number Indian` + `Number Bangladeshi` + `Number Other South Asian` + 
    `Number Caribbean` + `Number African` + `Number Black Scottish or Other Black` +
    `Number Any Mixed Background` + `Number Other Ethnic Group`
) %>% mutate(total = white + pakistani + chinese + other) %>% 
  select(oa_nrs, total, white, pakistani, chinese, other)

# evaluates TRUE

# > names(eth_2011)
# [1] "output_area"                                                                                     
# [2] "All people"                                                                                      
# [3] [WHITE] "White"                                                                                           
# [4] [NA] "White: Scottish"                                                                                 
# [5] [NA] "White: Other British"                                                                            
# [6] [NA] "White: Irish"                                                                                    
# [7] [NA] "White: Gypsy/Traveller"                                                                          
# [8] [NA] "White: Polish"                                                                                   
# [9] [NA] "White: Other White"                                                                              
# [10] [OTHER] "Mixed or multiple ethnic groups"                                                                 
# [11] [NA] "Asian, Asian Scottish or Asian British"                                                          
# [12] [PAKISTANI] "Asian, Asian Scottish or Asian British: Pakistani, Pakistani Scottish or Pakistani British"      
# [13] [OTHER] "Asian, Asian Scottish or Asian British: Indian, Indian Scottish or Indian British"               
# [14] [OTHER] "Asian, Asian Scottish or Asian British: Bangladeshi, Bangladeshi Scottish or Bangladeshi British"
# [15] [CHINESE] "Asian, Asian Scottish or Asian British: Chinese, Chinese Scottish or Chinese British"            
# [16] [OTHER] "Asian, Asian Scottish or Asian British: Other Asian"                                             
# [17] [OTHER] "African"                                                                                         
# [18] [NA] "African: African, African Scottish or African British"                                           
# [19] [NA] "African: Other African"                                                                          
# [20] [OTHER] "Caribbean or Black"                                                                              
# [21] [NA] "Caribbean or Black: Caribbean, Caribbean Scottish or Caribbean British"                          
# [22] [NA] "Caribbean or Black: Black, Black Scottish or Black British"                                      
# [23] [NA] "Caribbean or Black: Other Caribbean or Black"                                                    
# [24] [OTHER] "Other ethnic groups"                                                                             
# [25] [NA] "Other ethnic groups: Arab, Arab Scottish or Arab British"                                        
# [26] [NA] "Other ethnic groups: Other ethnic group" 


eth_2011_4grp <- eth_2011 %>% transmute(
  oa_2011= output_area,
  all_check = `All people`,
  white = White, 
  pakistani = `Asian, Asian Scottish or Asian British: Pakistani, Pakistani Scottish or Pakistani British`, 
  chinese = `Asian, Asian Scottish or Asian British: Chinese, Chinese Scottish or Chinese British`, 
  other = `Mixed or multiple ethnic groups` + `Asian, Asian Scottish or Asian British: Indian, Indian Scottish or Indian British` + 
    `Asian, Asian Scottish or Asian British: Bangladeshi, Bangladeshi Scottish or Bangladeshi British` + 
    `Asian, Asian Scottish or Asian British: Other Asian` + `African` + `Caribbean or Black` +
    `Other ethnic groups` 
) %>%  mutate(total = white + pakistani + chinese + other) %>% 
  select(oa_2011, total, white, pakistani, chinese, other)



dz_4grp_2001 <- eth_2001_4grp %>% 
  inner_join(nrs_oa_link) %>%
  inner_join(oa_dz_link) %>% 
  mutate(year = 2001) %>% 
  select(
    dz_2001, 
    la_name,
    year,
    total,
    white,
    pakistani,
    chinese,
    other
  ) %>% 
  group_by(
    dz_2001, 
    la_name, 
    year
  ) %>%
  summarise(
    total = sum(total),
    white = sum(white),
    pakistani = sum(pakistani),
    chinese = sum(chinese),
    other = sum(other)
  )



dz_4grp_2011 <- eth_2011_4grp %>% 
  inner_join(oa_dz_link) %>% 
  mutate(year = 2011) %>% 
  select(
    dz_2001, 
    la_name,
    year, 
    total,
    white,
    pakistani,
    chinese,
    other
  ) %>% 
  group_by(
    dz_2001,
    la_name,
    year
  ) %>% 
  summarise(
    total = sum(total),
    white = sum(white),
    pakistani = sum(pakistani),
    chinese = sum(chinese),
    other = sum(other)
  )



dz_eth_4grp <-
  dz_4grp_2001 %>% 
  bind_rows(dz_4grp_2011)

# Write this out and send to Gavin, Duncan & Jing

write_csv(
  dz_eth_4grp,
  path = "output_data/ethnicity_4group_categories_datazones_2001_and_2011.csv"        
)



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

