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




# Saving to nrs oa; 2001 dz; 2011 dz --------------------------------------


# Accommodation tables

accom_2001 <- read_csv("output_data/unharmonised/accommodationtype_2001.csv")
accom_2011 <- read_csv("output_data/unharmonised/accommodationtype_2011.csv")


# > names(accom_2001)
# [1] "output_area"                                      
#"ALL PEOPLE" [total]                                      
# [3] "In an unshared dwelling"                          
#"House or bungalow"  [house]                             
# [5] "     Detached"                                    
#"Semi-detached"                              
# [7] "     Terraced (including end-terrace)"            
#"  Flat, maisonette or apartment" [flat]                 
# [9] "     In a purpose-built block of flats"           
#"     Part of a converted or shared house"        
# [11] "     In a commercial building"                    
#"  Caravan or other mobile or temporary structure" [caravan]
# [13] "In a shared dwelling"     [shared]

# > names(accom_2011)
# [1] "output_area"                                                                 
# [2] "All people" [total]                                                                  
# [3] "Unshared dwelling total"                                                     
# [4] "Unshared dwelling: Whole house or bungalow" [house]                                  
# [5] "Unshared dwelling: Whole house or bungalow: Detached"                        
# [6] "Unshared dwelling: Whole house or bungalow: Semi-detached"                   
# [7] "Unshared dwelling: Whole house or bungalow: Terraced (including end terrace)"
# [8] "Unshared dwelling: Flat maisonette or apartment" [flat]                            
# [9] "Unshared dwelling: Purpose-built block of flats or tenement"                 
# [10] "Unshared dwelling: Part of a converted or shared house (including bed-sits)" 
# [11] "Unshared dwelling: In a commercial building"                                 
# [12] "Unshared dwelling: Caravan or other mobile or temporary structure" [caravan]           
# [13] "Shared dwelling" [shared]            

# going with: 
# shared
# flat
# house
# caravan

names(accom_2001)[c(2, 4, 8, 12, 13)] <- c("total", "house", "flat", "caravan", "shared")

accom_2001 <- accom_2001 %>% select(output_area, total, house, flat, caravan, shared)

# Check these are mutually exclusive and exhaustive
# > accom_2001  %>% mutate(t2 = house + flat + caravan + shared, check = total == t2)  %>% xtabs(~ check, data = .)
# check
# TRUE 
# 42604 



names(accom_2011)[c(2, 4, 8, 12, 13)] <- c("total", "house", "flat", "caravan", "shared")
accom_2011 <- accom_2011 %>% select(output_area, total, house, flat, caravan, shared)

# check these are mutually exclusive and exhaustive
# > accom_2011 %>% mutate(t2 = house + flat + caravan + shared, check = total == t2) %>% xtabs( ~ check , data = .)
# check
# TRUE 
# 46351 

write_csv(accom_2001, path = "output_data/oa_harmonised/accom_2001.csv")
write_csv(accom_2011, path = "output_data/oa_harmonised/accom_2011.csv")


# car attribute tables ----------------------------------------------------


car_2001 <- read_csv("output_data/unharmonised/carvanownership_2001.csv")
car_2011 <- read_csv("output_data/unharmonised/carvanownership_2011.csv")


names(car_2001) <- c("output_area", "total", "none", "one", "two", "three", "four", "totnumcar")

# car_2001 %>% mutate(t2 = none + one + two + three + four, check = total == t2) %>% xtabs( ~ check, data = .)
# > car_2001 %>% mutate(t2 = none + one + two + three + four, check = total == t2) %>% xtabs( ~ check, data = .)
# check
# TRUE 
# 42604 

car_2001 <- car_2001 %>% transmute(output_area = output_area, total = total, none = none, some = one + two + three + four)
# Source: local data frame [42,604 x 4]
# 
# output_area total none some
# 1   60QA000001    24    1   23
# 2   60QA000002    64    1   63
# 3   60QA000003    30    0   30
# 4   60QA000004    22    0   22
# 5   60QA000005    25    0   25
# 6   60QA000006    41    6   35
# 7   60QA000007    25    0   25
# 8   60QA000008    36    0   36
# 9   60QA000009    57    5   52
# 10  60QA000010    28    3   25
# ..         ...   ...  ...  ...

names(car_2011) <- c("output_area", "total", "none" ,"one", "two", "three", "four", "numcar")

# > car_2011 %>% mutate(t2 = none + one + two + three + four, check = total == t2) %>% xtabs(~ check, data = .)
# check
# TRUE 
# 46351 

write_csv(car_2001, path = "output_data/oa_harmonised/car_2001.csv")
write_csv(car_2011, path = "output_data/oa_harmonised/car_2011.csv")



# Country of birth


cob_2001 <- read_csv("output_data/unharmonised/countryofbirth_2001.csv")
cob_2011 <- read_csv("output_data/unharmonised/countryofbirth_2011.csv")

# > names(cob_2001)
# [1] "output_area"                                   "All people"                                   
# [3] "Number of people born in: England"             "Number of people born in: Scotland"           
# [5] "Number of people born in: Wales"               "Number of people born in: Northern Ireland"   
# [7] "Number of people born in: Republic of Ireland" "Number of people born in: Other EU countries" 
# [9] "Number of people born: Elsewhere"             

# > names(cob_2011)
# [1] "output_area"                                            "All people"                                            
# [3] "England"                                                "Northern Ireland"                                      
# [5] "Scotland"                                               "Wales"                                                 
# [7] "Republic of Ireland"                                    "Other EU: Member countries in March 2001 (1)"          
# [9] "Other EU: Accession countries April 2001 to March 2011" "Other countries"    


names(cob_2001) <- c("output_area", "total", "england", "scotland", "wales", "nir", "repirl", "other_eu", "elsewhere")
names(cob_2011) <- c("output_area", "total", "england", "nir", "scotland", "wales", "repirl", "other_eu_old", "other_eu_new", "elsewhere")

cob_2001 <- cob_2001 %>% transmute(output_area, total, scotland, ruk = england + wales + nir, elsewhere = repirl + other_eu + elsewhere)

# > cob_2001  %>% mutate(t2 = scotland + ruk + elsewhere, check = total == t2)  %>% xtabs(~ check, data = .)
# check
# TRUE 
# 42604 

cob_2011 <- cob_2011 %>% transmute(output_area, total, scotland, ruk = england + wales + nir, elsewhere = repirl + other_eu_old + other_eu_new + elsewhere)

# > cob_2011  %>% mutate(t2 = scotland + ruk + elsewhere, check = total == t2)  %>% xtabs(~ check, data = .)
# check
# TRUE 
# 46351 
 

write_csv(cob_2001, path = "output_data/oa_harmonised/cob_2001.csv")
write_csv(cob_2011, path = "output_data/oa_harmonised/cob_2011.csv")



# economic activity

ecact_2001 <- read_csv("output_data/unharmonised/economicactivity_2001.csv")
ecact_2011 <- read_csv("output_data/unharmonised/economicactivity_2011.csv")


# > names(ecact_2001)
# [1] "output_area"                                                                  
# [2] "All people aged 16 - 74"                                                      
# [3] "Number of people aged 16 - 74 Economically active Employees Part-time"        
# [4] "Number of people aged 16 - 74 Economically active Employees Full-time"        
# [5] "Number of people aged 16 - 74 Economically active Self employed"              
# [6] "Number of people aged 16 - 74 Economically active Unemployed"                 
# [7] "Number of people aged 16 - 74 Economically active Full-time student"          
# [8] "Number of people aged 16 - 74 Economically inactive Retired"                  
# [9] "Number of people aged 16 - 74 Economically inactive Student"                  
# [10] "Number of people aged 16 - 74 Economically inactive Looking after home/family"
# [11] "Number of people aged 16 - 74 Economically inactive Permanently sick/disabled"
# [12] "Number of people aged 16 - 74 Economically inactive Other"                    
# [13] "Number of unemployed people aged 16-74: Aged 16-24 years"                     
# [14] "Number of unemployed people aged 16-74: Aged 50 years and over"               
# [15] "Number of unemployed people aged 16-74 who have never worked"                 
# [16] "Number of unemployed people aged 16-74 who are long-term unemployed"          

names(ecact_2001) <- c("output_area", "total", "ft", "pt", "se", "unemp", "ftstud", "retired", "student", "home", "sick", "other", "16_24", "50_plus", "never_worked", "ltunemp")

ecact_2001 <- ecact_2001 %>% 
  transmute(output_area, total, 
            employed = ft + pt + se, 
            unemployed = unemp, 
            other = ftstud + retired + student + home + sick + other
            )

# > ecact_2001 %>% mutate(t2 = employed + unemployed + other, check = total == t2)  %>% xtabs(~check, data = .)
# check
# TRUE 
# 42604


#> names(ecact_2011)
# [1] "output_area"                                           "All people aged 16 to 74"                             
# [3] "Economically active: Employee: Part-time"              "Economically active: Employee: Full-time"             
# [5] "Economically active: Self-employed"                    "Economically active: Unemployed"                      
# [7] "Economically active: Full-time student"                "Economically inactive: Retired"                       
# [9] "Economically inactive: Student"                        "Economically inactive: Looking after home or family"  
# [11] "Economically inactive: Long-term sick or disabled"     "Economically inactive: Other"                         
# [13] "Unemployed people aged 16 to 74: Aged 16 to 24"        "Unemployed people aged 16 to 74: Aged 50 to 74"       
# [15] "Unemployed people aged 16 to 74: Never worked"         "Unemployed people aged 16 to 74: Long-term unemployed"

names(ecact_2011) <- c("output_area", "total", "pt", "ft", "se", "unemp", "ftstud", "retired", "student", "home", "sick", "other", "young", "old", "neverworked","ltunemp")

ecact_2011 <- ecact_2011 %>% 
  transmute(output_area, total, 
            employed = ft + pt + se, 
            unemployed = unemp, 
            other = ftstud + retired + student + home + sick + other
  )
# > ecact_2011 %>% 
#   mutate(t2 = employed + unemployed + other, check = total == t2)  %>% xtabs(~check, data = .)
# check
# TRUE 
# 46351 

write_csv(ecact_2001, path = "output_data/oa_harmonised/ecact_2001.csv")
write_csv(ecact_2011, path = "output_data/oa_harmonised/ecact_2011.csv")



# ethnicity ---------------------------------------------------------------



eth_2001 <- read_csv("output_data/unharmonised/ethnicity_2001.csv")
eth_2011 <- read_csv("output_data/unharmonised/ethnicity_2011.csv")

# > names(eth_2001)
# [1] "output_area"                                                                                                    
# [2] "ALL PEOPLE"                                                                                                     
# [3] "Number White Scottish"                                                                                          
# [4] "Number Other White British"                                                                                     
# [5] "Number White Irish"                                                                                             
# [6] "Number Other White"                                                                                             
# [7] "Number Indian"                                                                                                  
# [8] "Number Pakistani"                                                                                               
# [9] "Number Bangladeshi"                                                                                             
# [10] "Number Other South Asian"                                                                                       
# [11] "Number Chinese"                                                                                                 
# [12] "Number Caribbean"                                                                                               
# [13] "Number African"                                                                                                 
# [14] "Number Black Scottish or Other Black"                                                                           
# [15] "Number Any Mixed Background"                                                                                    
# [16] "Number Other Ethnic Group"                                                                                      
# [17] "Number of people aged 3 and over, who understand, speak, read or write Gaelic and who were born in Scotland"    
# [18] "Number of people aged 3 and over, who understand, speak, read or write Gaelic and who were not born in Scotland"
# 

names(eth_2001) <- c("output_area", "total", "wht_scot", "wht_othbrit", "wht_irish", "wht_other",
                     "indian", "pakistani", "bangladeshi", "other_south_asian", "chinese", 
                     "caribbean", "african", "black_other", "mixed", "other", "num_gaelic_brnscot", "num_gaelic_notbrnscot"
                     )
# 
# names(eth_2011)
# 
# > names(eth_2011)
# [1] "output_area"                                                                                     
# [2] "All people"                                                                                      
# [3] "White"                                                                                           
# [4] "White: Scottish"                                                                                 
# [5] "White: Other British"                                                                            
# [6] "White: Irish"                                                                                    
# [7] "White: Gypsy/Traveller"                                                                          
# [8] "White: Polish"                                                                                   
# [9] "White: Other White"                                                                              
# [10] "Mixed or multiple ethnic groups"                                                                 
# [11] "Asian, Asian Scottish or Asian British"                                                          
# [12] "Asian, Asian Scottish or Asian British: Pakistani, Pakistani Scottish or Pakistani British"      
# [13] "Asian, Asian Scottish or Asian British: Indian, Indian Scottish or Indian British"               
# [14] "Asian, Asian Scottish or Asian British: Bangladeshi, Bangladeshi Scottish or Bangladeshi British"
# [15] "Asian, Asian Scottish or Asian British: Chinese, Chinese Scottish or Chinese British"            
# [16] "Asian, Asian Scottish or Asian British: Other Asian"                                             
# [17] "African"                                                                                         
# [18] "African: African, African Scottish or African British"                                           
# [19] "African: Other African"                                                                          
# [20] "Caribbean or Black"                                                                              
# [21] "Caribbean or Black: Caribbean, Caribbean Scottish or Caribbean British"                          
# [22] "Caribbean or Black: Black, Black Scottish or Black British"                                      
# [23] "Caribbean or Black: Other Caribbean or Black"                                                    
# [24] "Other ethnic groups"                                                                             
# [25] "Other ethnic groups: Arab, Arab Scottish or Arab British"                                        
# [26] "Other ethnic groups: Other ethnic group"   
# 
names(eth_2011) <- c(
  "output_area", "total", "white", "white_scottish", "white_othbrit", "white_irish", "white_gypsy",
  "white_polish", "white_other", "mixed", "asian", "asian_pakistani", "asian_indian", "asian_bangladeshi", 
  "asian_chinese", "asian_other", "african", "african_african", "african_other", "carib",
  "carib_carib", "carib_black","carib_other", "other", "other_arab", "other_other"
  )



# > eth_2001 %>% 
# transmute(output_area, total, 
#           white = wht_scot + wht_othbrit + wht_irish + wht_other, 
#           pakistani, chinese, 
#           other = indian + bangladeshi + other_south_asian + caribbean + african + black_other + mixed + other) %>% mutate(t2 = white + pakistani + chinese + other, check = total == t2) %>% xtabs( ~ check , data = .)
# check
# TRUE 
# 42604 

eth_2001 <-  eth_2001 %>% 
  transmute(output_area, total, 
          white = wht_scot + wht_othbrit + wht_irish + wht_other, 
          pakistani, chinese, 
          other = indian + bangladeshi + other_south_asian + caribbean + 
            african + black_other + mixed + other) 

names(eth_2011) <- c(
  "output_area", "total", "white", "white_scottish", "white_othbrit", "white_irish", "white_gypsy",
  "white_polish", "white_other", "mixed", "asian", "asian_pakistani", "asian_indian", "asian_bangladeshi", 
  "asian_chinese", "asian_other", "african", "african_african", "african_other", "carib",
  "carib_carib", "carib_black","carib_other", "other", "other_arab", "other_other"
)

# > eth_2011 %>% 
# + transmute(output_area, total, white = white, 
#             +           pakistani = asian_pakistani, chinese = asian_chinese, 
#             +           other = asian_indian + asian_bangladeshi + asian_other + african + carib + other + mixed) %>% mutate(t2 = white + pakistani + chinese + other, check = total == t2) %>% xtabs( ~ check, data  = .)
# check
# TRUE 
# 46351

eth_2011 <- eth_2011 %>% 
transmute(output_area, total, white = white, 
          pakistani = asian_pakistani, chinese = asian_chinese, 
          other = asian_indian + asian_bangladeshi + asian_other + african + carib + other + mixed) 

write_csv(eth_2001, path = "output_data/oa_harmonised/eth_2001.csv")
write_csv(eth_2011, path = "output_data/oa_harmonised/eth_2011.csv")



#LLTI

llti_2001 <- read_csv("output_data/unharmonised/llti_2001.csv")
llti_2011 <- read_csv("output_data/unharmonised/llti_2011.csv")

names(llti_2001) <- c("output_area", "total", "with_llti", "without_llti")
names(llti_2011) <- c("output_area", "total", "lot", "little", "not")

llti_2001 <- llti_2001 %>% transmute(output_area, total, llti = with_llti, no_llti = without_llti)
llti_2011 <- llti_2011 %>% transmute(output_area, total, llti = lot + little, no_llti = not)
# > llti_2001 %>% mutate(t2 = llti + no_llti, check = total == t2) %>% xtabs( ~ check, data = .)
# check
# TRUE 
# 42604 
# > llti_2011 %>% mutate(t2 = llti + no_llti, check = total == t2) %>% xtabs( ~ check, data = .)
# check
# TRUE 
# 46351 

write_csv(llti_2001, path = "output_data/oa_harmonised/llti_2001.csv")
write_csv(llti_2011, path = "output_data/oa_harmonised/llti_2011.csv")

##### General health

gh_2001 <- read_csv("output_data/unharmonised/generalhealth_2001.csv")
gh_2011 <- read_csv("output_data/unharmonised/generalhealth_2011.csv")

names(gh_2001) <- c("output_area", "total", "good", "fairly_good", "not_good")
names(gh_2011) <- c("output_area", "total", "very_good", "good", "fair", "bad", "very_bad")

gh_2001 <- gh_2001 %>% transmute(output_area, total, good = good + fairly_good, not_good)
gh_2011 <- gh_2011 %>% transmute(output_area, total, good = good + very_good, not_good = fair + bad + very_bad)


# > gh_2001 %>% mutate(t2 = good + not_good, check = total == t2) %>% xtabs(~check, data = .)
# check
# TRUE 
# 42604 
# > gh_2011 %>% mutate(t2 = good + not_good, check = total == t2) %>% xtabs(~check, data = .)
# check
# TRUE 
# 46351 

write_csv(gh_2001, path = "output_data/oa_harmonised/general_health_2001.csv")
write_csv(gh_2011, path = "output_data/oa_harmonised/general_health_2011.csv")


# Marital status

ms_2001 <- read_csv("output_data/unharmonised/maritalstatus_2001.csv")
ms_2011 <- read_csv("output_data/unharmonised/maritalstatus_2011.csv")

names(ms_2001)
names(ms_2011)

# > names(ms_2001)
# [1] "output_area"                                                            
# [2] "All people aged 16 and over"                                            
# [3] "Number of people aged 16 and over Single (never married)"               
# [4] "Number of people aged 16 and over Married"                              
# [5] "Number of people aged 16 and over Re-married"                           
# [6] "Number of people aged 16 and over Separated (but still legally married)"
# [7] "Number of people aged 16 and over Divorced"                             
# [8] "Number of people aged 16 and over Widowed" 
names(ms_2001) <- c("output_area", "total", "single", "married", "remarried", "separated", "divorced", "widowed")

# > names(ms_2011)
# [1] "output_area"                                                                           
# [2] "All people aged 16 and over"                                                           
# [3] "Single (never married or never registered a same-sex civil partnership)"               
# [4] "Married"                                                                               
# [5] "In a registered same-sex civil partnership"                                            
# [6] "Separated (but still legally married or still legally in a same-sex civil partnership)"
# [7] "Divorced or formerly in a same-sex civil partnership which is now legally dissolved"   
# [8] "Widowed or surviving partner from a same-sex civil partnership"  
names(ms_2011) <- c("output_area", "total", "single", "married", 
                    "civil_partner", "separated", "divorced", "widowed")


# > ms_2001 %>% transmute(output_area, total, single = single + separated + divorced + widowed, married = married + remarried) %>% mutate(t2 = single + married, check = total == t2) %>% xtabs(~check, data = .)
# check
# TRUE 
# 42604 

ms_2001 <- ms_2001 %>% 
transmute(output_area, total, 
          single = single + separated + divorced + widowed, 
          married = married + remarried)

# > ms_2011 %>% transmute(output_area, total, single = single + separated + divorced + widowed, married = married + civil_partner) %>% mutate(t2 = single + married, check = t2 == total) %>% xtabs(~check, data = .)
# check
# TRUE 
# 46351 

ms_2011 <- ms_2011 %>% 
transmute(output_area, total, 
          single = single + separated + divorced + widowed, 
          married = married + civil_partner) 

write_csv(ms_2001, path = "output_data/oa_harmonised/marital_status_2001.csv")
write_csv(ms_2011, path = "output_data/oa_harmonised/marital_status_2011.csv")



# ns - sec ----------------------------------------------------------------


nssec_2001 <- read_csv("output_data/unharmonised/ns_sec_2001.csv")
nssec_2011 <- read_csv("output_data/unharmonised/ns_sec_2011.csv")

# names(nssec_2001)
# names(nssec_2011)
# 
# > names(nssec_2001)
# [1] "output_area"                                                                 
# [2] "ALL PEOPLE AGED 16 - 74"                                                     
# [3] "Number of people aged 16 - 74: Large employers and higher managers"          
# [4] "Number of people aged 16 - 74: Higher professional occupations"              
# [5] "Number of people aged 16 - 74: Lower managerial and professional occupations"
# [6] "Number of people aged 16 - 74: Intermediate occupations"                     
# [7] "Number of people aged 16 - 74: Small employers and own account workers"      
# [8] "Number of people aged 16 - 74: Lower supervisory and technical occupations"  
# [9] "Number of people aged 16 - 74: Semi-routine occupations"                     
# [10] "Number of people aged 16 - 74: Routine occupations"                          
# [11] "Number of people aged 16 - 74: Never worked"                                 
# [12] "Number of people aged 16 - 74: Long-term unemployed"                         
# [13] "Number of people aged 16 - 74: Full-time students"                           
# [14] "Number of people aged 16 - 74: Not classifiable for other reasons"     
names(nssec_2001) <- c(
  "output_area", "total", "lrgemp", "higher_prof_occupations", "lower_managerial_prof", "intermed", 
  "small_emp", "lower_sup", "semi_routine", "routine", "never_worked", "ltunemp", "ftstud", "other")

# > names(nssec_2011)
# [1] "output_area"                                                                                                             
# [2] "All people aged 16 to 74"                                                                                                
# [3] "1. Higher managerial, administrative and professional occupations: Total"                                                
# [4] "1. Higher managerial, administrative and professional occupations: 1.1 Large employers and higher managerial occupations"
# [5] "1. Higher managerial, administrative and professional occupations: 1.2 Higher professional occupations"                  
# [6] "2. Lower managerial and professional occupations"                                                                        
# [7] "3. Intermediate occupations"                                                                                             
# [8] "4. Small employers and own account workers"                                                                              
# [9] "5.  Lower supervisory and technical occupations"                                                                         
# [10] "6. Semi-routine occupations"                                                                                             
# [11] "7. Routine occupations"                                                                                                  
# [12] "8. Never worked and long-term unemployed: Total"                                                                         
# [13] "8. Never worked and long-term unemployed: L14.1 Never worked"                                                            
# [14] "8. Never worked and long-term unemployed: L14.2 Long-term unemployed"                                                    
# [15] "L15 Full-time students"   
names(nssec_2011) <- c("output_area", "total", "higher_total", "higher_large", 
                       "higher_professional", "lower_managerial", "intermed",
                       "small_emp", "lower_supervis", "semi_routine", "routine", 
                       "never_total", "never_never", "never_ltunemp", "ftstud")

#groups 
# higher managerial, admin and professional occupations
# lower managerial
# intermediate
# small employers and own account
# lower supervisory , semi routine and routine
# students
# never worked/long term unemployed

nssec_2001 <- nssec_2001 %>% transmute(
  output_area, total, 
  higher_man = lrgemp + higher_prof_occupations,
  lower_man = lower_managerial_prof,
  intermed,
  small_self = small_emp, 
  routine = lower_sup + semi_routine + routine,
  students = ftstud, 
  other = ltunemp + never_worked + other
                         ) 
# check
# TRUE 
# 42604 

nssec_2011 <- nssec_2011 %>% 
  transmute(output_area, total, 
          higher_man = higher_total, lower_man = lower_managerial, 
          intermed, small_self = small_emp, 
          routine = lower_supervis + semi_routine + routine, 
          students = ftstud, other = never_total)

# check
# TRUE 
# 46351 

write_csv(nssec_2001, "output_data/oa_harmonised/nssec_2001.csv")
write_csv(nssec_2011, "output_data/oa_harmonised/nssec_2011.csv")


#####################################################################
# religion


rel_2001 <- read_csv("output_data/unharmonised/religion_2001.csv")
rel_2011 <- read_csv("output_data/unharmonised/religion_2011.csv")

names(rel_2001) <- c("output_area", "total", "cos", "rom_cath", "other_chris", "buddhist", "hindu", "jewish", "muslim", "sikh", "other_rel", "none", "not_answered")

names(rel_2011) <- c("output_area", "total", "cos", "rom_cath", "other_chris", "buddhist", "hindu", "jewish", "muslim", "sikh", "other_rel", "none", "not_answered")


rel_2001 <- rel_2001 %>% 
transmute(output_area, total, 
          non_catholic_christian = cos + other_chris, 
          catholic_christian= rom_cath, jewish, muslim, 
          none, other = buddhist + hindu + sikh + other_rel + not_answered) 

# check
# TRUE 
# 42604 

rel_2011 <- rel_2011 %>% 
transmute(output_area, total, 
          non_catholic_christian = cos + other_chris, 
          catholic_christian= rom_cath, jewish, muslim, 
          none, other = buddhist + hindu + sikh + other_rel + not_answered) 
# check
# TRUE 
# 46351 

write_csv(rel_2001, path = "output_data/oa_harmonised/religion_2001.csv")
write_csv(rel_2011, path = "output_data/oa_harmonised/religion_2011.csv")

################################
#### TENURE, HOUSEHOLD TYPE

tenure_2001 <- read_csv("output_data/unharmonised/tenure_2001.csv")
tenure_2011 <- read_csv("output_data/unharmonised/tenure_2011.csv")

# > names(tenure_2001)
# [1] "output_area"                                           "ALL PEOPLE"                                           
# [3] " Owned"                                                "Owns outright"                                        
# [5] "Owns  with a mortgage or loan"                         "Shared ownership"                                     
# [7] "Social rented"                                         "Rented from Council (Local Authority/Scottish Homes)" 
# [9] "Other social rented"                                   "Private rented - furnished"                           
# [11] "Private landlord or letting agency furnished"          "Employer of a household member furnished"             
# [13] "Relative or friend of a household member furnished"    "Other furnished"                                      
# [15] "Private rented - unfurnished"                          "Private landlord or letting agency unfurnished"       
# [17] "Employer of a household memeber unfurnished"           "Relative or friend of a household memeber unfurnished"
# [19] "Other unfurnished"                                     "Living rent free"         
names(tenure_2001) <- c(
  "output_area",
  "total",
  "owned",
  "owned_outright",
  "owned_mortgage",
  "owned_shared",
  "social",
  "social_council",
  "social_other",
  "furnished",
  "furnished_landlord",
  "furnished_employer",
  "furnished_relative",
  "furnished_other",
  "unfurnished",
  "unfurnished_landlord",
  "unfurnished_employer",
  "unfurnished_relative",
  "unfurnished_other",
  "rentfree"
)


# > names(tenure_2011)
# [1] "output_area"                                              "All people in households"                                
# [3] "Owned"                                                    "Owned: Owned outright"                                   
# [5] "Owned: Owned with a mortgage or loan"                     "Owned: Shared ownership (part owned and part rented)"    
# [7] "Social rented"                                            "Social rented: Rented from council (Local authority)"    
# [9] "Social rented: Other social rented"                       "Private rented"                                          
# [11] "Private rented: Private landlord or letting agency"       "Private rented: Employer of a household member"          
# [13] "Private rented: Relative or friend of a household member" "Private rented: Other"                                   
# [15] "Living rent free"                                        

names(tenure_2011) <- c(
  "output_area",
  "total",
  "owned",
  "owned_ownedoutright",
  "owned_ownedmortgage",
  "owned_shared",
  "socialrented",
  "socialrented_council",
  "socialrented_other",
  "privaterented",
  "privaterented_landlord",
  "privaterented_employer",
  "privaterented_relative",
  "privaterented_other",
  "rentfree"
)



tenure_2001 <- tenure_2001 %>% 
  transmute(
    output_area,  total, 
    owned,
    social = social,
    rented = furnished + unfurnished,
    rentfree 
      ) 
# > tenure_2001 %>% 
#   +   transmute(
#     +     output_area,  total, 
#     +     owned,
#     +     social = social,
#     +     rented = furnished + unfurnished,
#     +     rentfree 
#     +       ) %>% mutate(t2 = owned + social + rented + rentfree, check = total == t2) %>% xtabs( ~ check, data = .)
# check
# TRUE 
# 42604 


tenure_2011 <- tenure_2011 %>% 
  transmute(
    output_area, total,
    owned, 
    social = socialrented,
    rented = privaterented, 
    rentfree = rentfree
  ) 

# tenure_2011 %>% 
#   transmute(
#     output_area, total,
#     owned, 
#     social = socialrented,
#     rented = privaterented, 
#     rentfree = rentfree
#   ) %>% 
#   mutate(t2 = owned + social + rented + rentfree, check = total == t2) %>% xtabs( ~ check, data = .)


write_csv(tenure_2001, path = "output_data/oa_harmonised/tenure_2001.csv")
write_csv(tenure_2011, path = "output_data/oa_harmonised/tenure_2011.csv")



# Household type ----------------------------------------------------------

hh_2001 <- read_csv("output_data/unharmonised/households_2001.csv")
hh_2011 <- read_csv("output_data/unharmonised/households_2011.csv")

# > names(hh_2001)
# [1] "output_area"                                                                                                      
# [2] "All households"                                                                                                   
# [3] "Number of households comprising: One person Pensioner"                                                            
# [4] "Number of households comprising: One person Other"                                                                
# [5] "Number of households comprising: One family and no others All pensioners"                                         
# [6] "Number of households comprising: One family and no others Married couple households No children"                  
# [7] "Number of households comprising: One family and no others Married couple households With dependent children"      
# [8] "Number of households comprising: One family and no others Married couple households All children non-dependent "  
# [9] "Number of households comprising: One family and no others Cohabiting couple households No children"               
# [10] "Number of households comprising: One family and no others Cohabiting couple households With dependent children"   
# [11] "Number of households comprising: One family and no others Cohabiting couple households All children non-dependent"
# [12] "Number of households comprising: One family and no others Lone parent housheolds With dependent children"         
# [13] "Number of households comprising: One family and no others Lone parent households All children non-dependent"      
# [14] "Number of households comprising: Other households With dependent children"                                        
# [15] "Number of households comprising: Other households All student"                                                    
# [16] "Number of households comprising: Other households All pensioner"                                                  
# [17] "Number of households comprising: Other households Other"  

names(hh_2001) <- c(
  "output_area",
  "total",
  "single_pensioner",
  "single_nonpensioner",
  "nonsingle_pensioner",
  "couple_married_nochildren",
  "couple_married_depchildren",
  "couple_married_nondepchildren",
  "couple_cohab_nochildren",
  "couple_cohab_depchildren",
  "couple_cohab_nondepchildren",
  "single_depchildren",
  "single_nondepchildren",
  "other_depchildren",
  "student",
  "other_pensioner",
  "other_other"
)


# > names(hh_2011)
# [1] "output_area"                                                             
# [2] "All households"                                                          
# [3] "One person household"                                                    
# [4] "One person household: Aged 65 and over"                                  
# [5] "One person household: Aged under 65"                                     
# [6] "One family household"                                                    
# [7] "One family household: All aged 65 and over"                              
# [8] "One family household: Married couple"                                    
# [9] "One family household: Married couple: No children"                       
# [10] "One family household: Married couple: One dependent child"               
# [11] "One family household: Married couple: Two or more dependent children"    
# [12] "One family household: Married couple: All children non-dependent"        
# [13] "One family household: Same-sex civil partnership couple"                 
# [14] "One family household: Cohabiting couple"                                 
# [15] "One family household: Cohabiting couple: No children"                    
# [16] "One family household: Cohabiting couple: One dependent child"            
# [17] "One family household: Cohabiting couple: Two or more dependent children" 
# [18] "One family household: Cohabiting couple: All children non-dependent"     
# [19] "One family household: Lone parent family"                                
# [20] "One family household: Lone parent family: One dependent child"           
# [21] "One family household: Lone parent family: Two or more dependent children"
# [22] "One family household: Lone parent family: All children non-dependent"    
# [23] "Other household types"                                                   
# [24] "Other household types: One dependent child"                              
# [25] "Other household types: Two or more dependent children"                   
# [26] "Other household types: All full-time students"                           
# [27] "Other household types: All aged 65 and over"                             
# [28] "Other household types: Other" 

names(hh_2011) <- c(
"output_area",
"total",
"single",
"single_pensioner",
"single_nonpensioner",
"family",
"family_pensioner",
"family_couple_married",
"family_couple_married_nochildren",
"family_couple_married_onechild",
"family_couple_married_twomorechildren",
"family_couple_nondepchildren",
"family_couple_civil",
"family_cohab",
"family_cohab_nochildren",
"family_cohab_onechild",
"family_cohab_twomoredepchildren",
"family_cohab_nondepchildren",
"lone",
"lone_onechild",
"lone_twomorechildren",
"lone_nondepchildren",
"other",
"other_onechild",
"other_twomoredepchildren",
"other_students",
"other_pensioner",
"other_other"
)


# first way of dividing households: 
# students, pensioners, everyone else

hh_2001_spo <- hh_2001 %>% transmute(
  output_area, total,
  pensioner = single_pensioner + nonsingle_pensioner + other_pensioner,
  student = student
) %>% mutate(other = total - pensioner - student) 

hh_2011_spo <- hh_2011 %>% transmute(
  output_area, total,
  pensioner = single_pensioner + family_pensioner + other_pensioner,
  student = other_students
) %>% mutate(other = total - pensioner - student) 



write_csv(hh_2001_spo, path = "output_data/oa_harmonised/studentspensionersother_2001.csv")
write_csv(hh_2011_spo, path = "output_data/oa_harmonised/studentspensionersother_2011.csv")





# Part 3: mapping from OA to dz_2011 --------------------------------------

# A slightly different process is needed for 2001 oas than for 2011 oas

# 2001 files 
oa_2001 <- dir("output_data/oa_harmonised/", pattern = "_2001.csv$")
oa_2011 <- dir("output_data/oa_harmonised/", pattern = "_2011.csv$")


# first for 2001 tables 

od11 <- final_link %>% select(oa_nrs, dz_2011)



fn <- function(x){
  inloc <- paste0("output_data/oa_harmonised/", x)
  infile <- read_csv(inloc)
  nms <- names(infile)
  nms <- nms[nms != "output_area"]
  
  outfile <- infile  %>% 
    inner_join(od11, by = c("output_area" = "oa_nrs"))  %>% 
    distinct  %>% 
    .[c("dz_2011", nms)]  %>% 
    mutate(dz_2011 = str_trim(dz_2011)) %>% 
    arrange(dz_2011)  %>% 
    filter(dz_2011 != "")  %>% 
    group_by(dz_2011)  %>% 
    summarise_each( funs(sum))
  
  write_csv(outfile, path = paste0("output_data/dz_2011/", x))
  return(NULL)
}

l_ply(oa_2001, fn)


# now for 2011 tables 

rm(fn, od11)


od11 <- final_link %>% select(oa_2011, dz_2011)

fn <- function(x){
  inloc <- paste0("output_data/oa_harmonised/", x)
  infile <- read_csv(inloc)
  nms <- names(infile)
  nms <- nms[nms != "output_area"]
  
  outfile <- infile  %>% 
    inner_join(od11, by = c("output_area" = "oa_2011"))  %>% 
    distinct  %>% 
    .[c("dz_2011", nms)]  %>% 
    mutate(dz_2011 = str_trim(dz_2011)) %>% 
    arrange(dz_2011)  %>% 
    filter(dz_2011 != "")  %>% 
    group_by(dz_2011)  %>% 
    summarise_each( funs(sum))
  
  write_csv(outfile, path = paste0("output_data/dz_2011/", x))
  return(NULL)
}

l_ply(oa_2011, fn)


# Now, for completeness, to do the same with 2001 dzs


# 2001 oas, 2001 dzs
od11 <- final_link %>% select(oa_nrs, dz_2001)



fn <- function(x){
  inloc <- paste0("output_data/oa_harmonised/", x)
  infile <- read_csv(inloc)
  nms <- names(infile)
  nms <- nms[nms != "output_area"]
  
  outfile <- infile  %>% 
    inner_join(od11, by = c("output_area" = "oa_nrs"))  %>% 
    distinct  %>% 
    .[c("dz_2001", nms)]  %>% 
    mutate(dz_2001 = str_trim(dz_2001)) %>% 
    arrange(dz_2001)  %>% 
    filter(dz_2001 != "")  %>% 
    group_by(dz_2001)  %>% 
    summarise_each( funs(sum))
  
  write_csv(outfile, path = paste0("output_data/dz_2001/", x))
  return(NULL)
}

l_ply(oa_2001, fn)


# now for 2011 tables 

rm(fn, od11)


od11 <- final_link %>% select(oa_2011, dz_2001)

fn <- function(x){
  inloc <- paste0("output_data/oa_harmonised/", x)
  infile <- read_csv(inloc)
  nms <- names(infile)
  nms <- nms[nms != "output_area"]
  
  outfile <- infile  %>% 
    inner_join(od11, by = c("output_area" = "oa_2011"))  %>% 
    distinct  %>% 
    .[c("dz_2001", nms)]  %>% 
    mutate(dz_2001 = str_trim(dz_2001)) %>% 
    arrange(dz_2001)  %>% 
    filter(dz_2001 != "")  %>% 
    group_by(dz_2001)  %>% 
    summarise_each( funs(sum))
  
  write_csv(outfile, path = paste0("output_data/dz_2001/", x))
  return(NULL)
}

l_ply(oa_2011, fn)





