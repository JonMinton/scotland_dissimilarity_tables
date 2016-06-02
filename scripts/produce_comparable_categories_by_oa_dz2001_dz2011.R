# Construct simplified dissimilarity scores by 2001 datazone for the following:

# 2/6/2016 - change from Jing link to Norman Link


rm(list = ls())

require(pacman)

pacman::p_load(
  readr, 
  tidyr, stringr, dplyr
)





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

car_2001 <- car_2001 %>% 
  transmute(
    output_area = output_area, 
    total = total, 
    none = none, 
    some = one + two + three + four
    )
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

cob_2001 <- cob_2001 %>% 
  transmute(
    output_area, 
    total, 
    scotland, 
    ruk = england + wales + nir, 
    elsewhere = repirl + other_eu + elsewhere
    )

# > cob_2001  %>% mutate(t2 = scotland + ruk + elsewhere, check = total == t2)  %>% xtabs(~ check, data = .)
# check
# TRUE 
# 42604 

cob_2011 <- cob_2011 %>% 
  transmute(
    output_area, 
    total, 
    scotland, 
    ruk = england + wales + nir, 
    elsewhere = repirl + other_eu_old + other_eu_new + elsewhere
    )

# > cob_2011  %>% mutate(t2 = scotland + ruk + elsewhere, check = total == t2)  %>% xtabs(~ check, data = .)
# check
# TRUE 
# 46351 
 

write_csv(cob_2001, path = "output_data/oa_harmonised/cob_2001.csv")
write_csv(cob_2011, path = "output_data/oa_harmonised/cob_2011.csv")






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
transmute(output_area, 
          total, 
          white = white, 
          pakistani = asian_pakistani, chinese = asian_chinese, 
          other = asian_indian + asian_bangladeshi + asian_other + african + carib + other + mixed) 

write_csv(eth_2001, path = "output_data/oa_harmonised/eth_2001.csv")
write_csv(eth_2011, path = "output_data/oa_harmonised/eth_2011.csv")


eth_2001_2cat <- eth_2001 %>% 
  transmute(
    output_area, 
    total, 
    white = white, 
    nonwhite = total - white
    )

eth_2011_2cat <- eth_2011 %>% 
  transmute(
    output_area, 
    total, 
    white, 
    nonwhite = total - white
    )

write_csv(eth_2001_2cat, path = "output_data/oa_harmonised/eth_2001_2cat.csv")
write_csv(eth_2011_2cat, path = "output_data/oa_harmonised/eth_2011_2cat.csv")




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

gh_2001 <- gh_2001 %>% 
  transmute(
    output_area, 
    total, 
    good = good + fairly_good, not_good
  )

gh_2011 <- gh_2011 %>% 
  transmute(
    output_area, 
    total, 
    good = good + very_good + fair, 
    not_good = bad + very_bad
    )


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
  transmute(
    output_area, total, 
    single = single + separated + divorced + widowed, 
    married = married + remarried
    )

# > ms_2011 %>% transmute(output_area, total, single = single + separated + divorced + widowed, married = married + civil_partner) %>% mutate(t2 = single + married, check = t2 == total) %>% xtabs(~check, data = .)
# check
# TRUE 
# 46351 

ms_2011 <- ms_2011 %>% 
  transmute(
    output_area, total, 
    single = single + separated + divorced + widowed, 
    married = married + civil_partner
  ) 

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
  "small_emp", "lower_sup", "semi_routine", "routine", "never_worked", "ltunemp", "ftstud", "other"
  )

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
  transmute(
    output_area, total, 
    higher_man = higher_total, 
    lower_man = lower_managerial, 
    intermed, 
    small_self = small_emp, 
    routine = lower_supervis + semi_routine + routine, 
    students = ftstud, other = never_total
    )

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
  transmute(
    output_area, total, 
    non_catholic_christian = cos + other_chris, 
    catholic_christian= rom_cath, jewish, muslim, 
    none, other = buddhist + hindu + sikh + other_rel + not_answered
    ) 

# check
# TRUE 
# 42604 

rel_2011 <- rel_2011 %>% 
  transmute(
    output_area, total, 
    non_catholic_christian = cos + other_chris, 
    catholic_christian= rom_cath, jewish, muslim, 
    none, other = buddhist + hindu + sikh + other_rel + not_answered
    ) 
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

hh_2001_spo <- hh_2001 %>% 
  transmute(
    output_area, total,
    pensioner = single_pensioner + nonsingle_pensioner + other_pensioner,
    student = student
  ) %>% 
  mutate(
    other = total - pensioner - student
    ) 

hh_2011_spo <- hh_2011 %>% 
  transmute(
    output_area, total,
    pensioner = single_pensioner + family_pensioner + other_pensioner,
    student = other_students
  ) %>% 
  mutate(
    other = total - pensioner - student
    ) 



write_csv(hh_2001_spo, path = "output_data/oa_harmonised/studentspensionersother_2001.csv")
write_csv(hh_2011_spo, path = "output_data/oa_harmonised/studentspensionersother_2011.csv")



#economic activity

ea_2001 <- read_csv("output_data/unharmonised/economic_activity_2001.csv")
ea_2011 <- read_csv("output_data/unharmonised/economic_activity_2011.csv")

# > names(ea_2001)
# [1] "output_area"                                     
# [2] "ALL PEOPLE"                                      
# [3] "Economically active"                             
# [4] "   Employee total"                               
# [5] "     Employee Part-time"                         
# [6] "     Employee Full-time"                         
# [7] "   Self-employed with employees total"           
# [8] "     Self-employed with employees - Part-time"   
# [9] "     Self-employed with employees - Full-time"   
# [10] "   Self-employed without employees total"        
# [11] "     Self-employed without employees - Part-time"
# [12] "     Self-employed without employees - Full-time"
# [13] "     Unemployed"                                 
# [14] "     Full-time students"                         
# [15] "Economically inactive"                           
# [16] "     Retired"                                    
# [17] "     Student"                                    
# [18] "     Looking after home/family"                  
# [19] "     Permanently sick/disabled"                  
# [20] "     Other" 

names(ea_2001) <- c(
  "output_area",
  "total",
  "ecact",
  "ecact_emp",
  "ecact_emp_pt",
  "ecact_emp_ft",
  "ecact_selfwith",
  "ecact_selfwith_pt",
  "ecact_selfwith_ft",
  "ecact_selfwout",
  "ecact_selfwout_pt",
  "ecact_selfwout_ft",
  "ecact_unemp",
  "ecact_student_ft",
  "ecin",
  "ecin_retired",
  "ecin_student",
  "ecin_homemaker",
  "ecin_sick",
  "ecin_other"
  )

# > names(ea_2011)
# [1] "output_area"                                                    
# [2] "All people aged 16 to 74"                                       
# [3] "Economically active"                                            
# [4] "Economically active: Employee: Part-time"                       
# [5] "Economically active: Employee: Full-time"                       
# [6] "Economically active: Self-employed with employees: Part-time"   
# [7] "Economically active: Self-employed with employees: Full-time"   
# [8] "Economically active: Self-employed without employees: Part-time"
# [9] "Economically active: Self-employed without employees: Full-time"
# [10] "Economically active: Unemployed"                                
# [11] "Economically active: Full-time student"                         
# [12] "Economically inactive"                                          
# [13] "Economically inactive: Retired"                                 
# [14] "Economically inactive: Student"                                 
# [15] "Economically inactive: Looking after home or family"            
# [16] "Economically inactive: Long-term sick or disabled"              
# [17] "Economically inactive: Other"    

names(ea_2011) <- c(
  "output_area",
  "total",
  "ecact",
  "ecact_emp_pt",
  "ecact_emp_ft",
  "ecact_self_pt_emp",
  "ecact_self_ft_emp",
  "ecact_self_pt_sole",
  "ecact_self_ft_sole",
  "ecact_unemp",
  "ecact_student",
  "ecinact",
  "ecinact_retired",
  "ecinact_student",
  "ecinact_homemaker",
  "ecinact_sick",
  "ecinact_other"
)


# first way of dividing households: 
# students, pensioners, everyone else

ea_2001_harmonised <- ea_2001 %>% 
  transmute(
    output_area, total,
    employed = ecact_emp + ecact_selfwith + ecact_selfwout,
    unemployed = ecact_unemp,
    student = ecact_student_ft + ecin_student,
    retired = ecin_retired,
    homemaker = ecin_homemaker,
    sick = ecin_sick,
    inactive_other = ecin_other
  ) 


ea_2011_harmonised <- ea_2011 %>% 
  transmute(
    output_area, total, 
    employed = ecact_emp_pt + ecact_emp_ft + ecact_self_pt_emp + ecact_self_ft_emp + ecact_self_pt_sole + ecact_self_ft_sole,
    unemployed = ecact_unemp,
    student = ecact_student + ecinact_student,
    retired = ecinact_retired,
    homemaker = ecinact_homemaker,
    sick = ecinact_sick,
    inactive_other = ecinact_other
  ) 


write_csv(ea_2001_harmonised, path = "output_data/oa_harmonised/economic_activity_bigger_2001.csv")
write_csv(ea_2011_harmonised, path = "output_data/oa_harmonised/economic_activity_bigger_2011.csv")



# Now to produce a simpler version, just with economic activity and economic inactivity

ea_2001_harmonised_simple <- ea_2001 %>% 
  transmute(
    output_area, total,
    active = ecact,
    inactive = ecin
  ) 


ea_2011_harmonised_simple <- ea_2011 %>% 
  transmute(
    output_area, total, 
    active = ecact,
    inactive = ecinact
    ) 


write_csv(ea_2001_harmonised_simple, path = "output_data/oa_harmonised/economic_activity_simple_2001.csv")
write_csv(ea_2011_harmonised_simple, path = "output_data/oa_harmonised/economic_activity_simple_2011.csv")




