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

# Jing nrs_oa_2001 to dz_2011 link

jing_link <- source_DropboxData(
    file = "nrs_oa_2001_to_dz_2011.csv",
    key = "kxtxts7y1x92fgq"
  ) %>% tbl_df


# > jing_link
# Source: local data frame [42,604 x 2]
# 
# OA_2001   DZ_2011
# 1  60QA000008 S01006506
# 2  60QA001254 S01006506
# 3  60QA001256 S01006506
# 4  60QA001258 S01006506
# 5  60QA001343 S01006506
# 6  60QA001805 S01006506
# 7  60QA001806 S01006506
# 8  60QA001807 S01006506
# 9  60QA001255 S01006507
# 10 60QA001261 S01006507
# ..        ...       ...


nrs_oa_link <- source_DropboxData(
  file = "OUTPUT_AREA_2001_LOOKUP.csv",
  key = "39wszvlpxy4qvpf"
) %>% tbl_df

# First few lines of the above table

# > nrs_oa_link
# Source: local data frame [42,604 x 2]
# 
# OutputArea2001Code NRSoldOutputArea2001Code
# 1           S00000001               60QA000001
# 2           S00000002               60QA000002
# 3           S00000003               60QA000003
# 4           S00000004               60QA000004
# 5           S00000005               60QA000005
# 6           S00000006               60QA000006
# 7           S00000007               60QA000007
# 8           S00000008               60QA000008
# 9           S00000009               60QA000009
# 10          S00000010               60QA000010
# ..                ...                      ...

# > length(unique(nrs_oa_link$OutputArea2001Code))
# [1] 42604
# > length(unique(nrs_oa_link$NRSoldOutputArea2001Code))
# [1] 42604

# It therefore seems that each nrs_oa code has maps 1:1 to standard codes, with 60QA in place of S01
# To check this: 

# tmp1 <- nrs_oa_link$NRSoldOutputArea2001Code %>% str_replace("60QA", "")
# tmp2 <- nrs_oa_link$OutputArea2001Code  %>% str_replace("^S00", "")
# tmp3 <- cbind(tmp1, tmp2)

# head(apply(tmp3, 1, function(x) x[1] == x[2]))
#[1] TRUE TRUE TRUE TRUE TRUE TRUE

# all(apply(tmp3, 1, function(x) x[1] == x[2]))
#[1] FALSE

# table(apply(tmp3, 1, function(x) x[1] == x[2]))
# FALSE  TRUE 
# 40743  1861 

# > head(tmp3[apply(tmp3, 1, function(x) x[1] == x[2]) ==FALSE,])
# tmp1         tmp2    
# [1,] "60QB000001" "001862"
# [2,] "60QB000002" "001863"
# [3,] "60QB000003" "001864"
# [4,] "60QB000004" "001865"
# [5,] "60QB000005" "001866"
# [6,] "60QB000006" "001867"

# So, the codes are almost but not quite the same apart from the prefixes, but they are 1:1 matches
#rm(tmp1, tmp2, tmp3)

nrs_oa_link <- nrs_oa_link %>% 
  rename(oa_2001 = OutputArea2001Code, oa_nrs = NRSoldOutputArea2001Code)



big_link <- source_DropboxData(
  file =  "Census_2011_Lookup__OA_TO_HIGHER_AREAS.csv",
  key =   "95x5ozuw0c6xgxk",
  colClasses = "character"
) %>% tbl_df

oa_dz_link <- big_link %>% 
  select(
    oa_2001 = OutputArea2001Code,
    oa_2011 = OutputArea2011Code,
    dz_2001 = Datazone2001Code, 
    la_2011 = CouncilArea2011Code # this will allow links to specific cities later 
  )

# > oa_dz_link
# Source: local data frame [46,351 x 4]
# 
# oa_2001   oa_2011   dz_2001   la_2011
# 1  S00004507 S00093944 S01000675 S12000041
# 2  S00040457 S00132898 S01006149 S12000030
# 3  S00005737 S00095247 S01005406 S12000026
# 4  S00027843 S00119131 S01003962 S12000017
# 5  S00008211 S00097920 S01001005 S12000006
# 6  S00006223 S00095782 S01005441 S12000026
# 7  S00029197 S00120583 S01004183 S12000019
# 8  S00007822 S00097516 S01000915 S12000006
# 9  S00027793 S00119061 S01003870 S12000017
# 10 S00036870 S00128942 S01005501 S12000027
# ..       ...       ...       ...       ...


la_2011_codes <- source_DropboxData(
  file = "LAD_2012_UK_NC.csv",
  key = "86em2kfrq0xxmpk",
  colClasses = "character"
) %>% tbl_df

# > la_2011_codes
# Source: local data frame [406 x 3]
# 
# LAD12CD LAD12CDO                     LAD12NM
# 1  E06000001     00EB                  Hartlepool
# 2  E06000002     00EC               Middlesbrough
# 3  E06000003     00EE        Redcar and Cleveland
# 4  E06000004     00EF            Stockton-on-Tees
# 5  E06000005     00EH                  Darlington
# 6  E06000006     00ET                      Halton
# 7  E06000007     00EU                  Warrington
# 8  E06000008     00EX       Blackburn with Darwen
# 9  E06000009     00EY                   Blackpool
# 10 E06000010     00FA Kingston upon Hull, City of
# ..       ...      ...                         ...


la_2011_codes <- la_2011_codes %>% 
  select(
    la_2011 = LAD12CD, 
    la_name = LAD12NM
  )

# > la_2011_codes
# Source: local data frame [406 x 2]
# 
# la_2011                     la_name
# 1  E06000001                  Hartlepool
# 2  E06000002               Middlesbrough
# 3  E06000003        Redcar and Cleveland
# 4  E06000004            Stockton-on-Tees
# 5  E06000005                  Darlington
# 6  E06000006                      Halton
# 7  E06000007                  Warrington
# 8  E06000008       Blackburn with Darwen
# 9  E06000009                   Blackpool
# 10 E06000010 Kingston upon Hull, City of
# ..       ...                         ...

oa_dz_link <- oa_dz_link %>% inner_join(la_2011_codes) # inner_join so just Scotland

jing_link <- jing_link %>% rename(oa_nrs = OA_2001, dz_2011 = DZ_2011)

jing_link

# > jing_link
# Source: local data frame [42,604 x 2]
# 
# oa_nrs   dz_2011
# 1  60QA000008 S01006506
# 2  60QA001254 S01006506
# 3  60QA001256 S01006506
# 4  60QA001258 S01006506
# 5  60QA001343 S01006506
# 6  60QA001805 S01006506
# 7  60QA001806 S01006506
# 8  60QA001807 S01006506
# 9  60QA001255 S01006507
# 10 60QA001261 S01006507
# ..        ...       ...

jing_link  %>% inner_join(nrs_oa_link)
# Joining by: "oa_nrs"
# Source: local data frame [42,604 x 3]
# 
# oa_nrs   dz_2011   oa_2001
# 1  60QA000008 S01006506 S00000008
# 2  60QA001254 S01006506 S00001254
# 3  60QA001256 S01006506 S00001256
# 4  60QA001258 S01006506 S00001258
# 5  60QA001343 S01006506 S00001343
# 6  60QA001805 S01006506 S00001805
# 7  60QA001806 S01006506 S00001806
# 8  60QA001807 S01006506 S00001807
# 9  60QA001255 S01006507 S00001255
# 10 60QA001261 S01006507 S00001261
# ..        ...       ...       ...

jing_link  %>% inner_join(nrs_oa_link) %>% inner_join(oa_dz_link)
# > jing_link  %>% inner_join(nrs_oa_link) %>% inner_join(oa_dz_link)
# Joining by: "oa_nrs"
# Joining by: "oa_2001"
# Source: local data frame [46,351 x 7]
# 
# oa_nrs   dz_2011   oa_2001   oa_2011   dz_2001   la_2011       la_name
# 1  60QA000008 S01006506 S00000008 S00088963 S01000001 S12000033 Aberdeen City
# 2  60QA000008 S01006506 S00000008 S00088964 S01000001 S12000033 Aberdeen City
# 3  60QA001254 S01006506 S00001254 S00090287 S01000001 S12000033 Aberdeen City
# 4  60QA001254 S01006506 S00001254 S00090286 S01000001 S12000033 Aberdeen City
# 5  60QA001256 S01006506 S00001256 S00090289 S01000001 S12000033 Aberdeen City
# 6  60QA001258 S01006506 S00001258 S00090291 S01000001 S12000033 Aberdeen City
# 7  60QA001343 S01006506 S00001343 S00090377 S01000001 S12000033 Aberdeen City
# 8  60QA001805 S01006506 S00001805 S00090888 S01000001 S12000033 Aberdeen City
# 9  60QA001807 S01006506 S00001807 S00090889 S01000001 S12000033 Aberdeen City
# 10 60QA001255 S01006507 S00001255 S00090288 S01000013 S12000033 Aberdeen City
# ..        ...       ...       ...       ...       ...       ...           ...

final_link <- jing_link  %>% inner_join(nrs_oa_link) %>% inner_join(oa_dz_link)





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

