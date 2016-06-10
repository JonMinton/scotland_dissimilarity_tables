# produce binary versions of all tables 

# the tables in question: 

rm(list = ls())

require(readr)
require(stringr)
require(plyr)
require(dplyr)
require(tidyr)



# accom
# car
# cob
# ecact
# eth (done)
# general health
# llti
# marital status
# nssec
# religion
# student pensioner
# tenure
# employed/not employed
# economic inactive



# accom
accom_n_t1 <- read_csv("output_data/dz_2001/accom_2001.csv")
accom_n_t2 <- read_csv("output_data/dz_2001/accom_2011.csv")

# house/nonhouse

accom_2_t1 <- accom_n_t1 %>% transmute(dz_2001, total, house, nonhouse = flat + caravan + shared)
# checked = all OK 

accom_2_t2 <- accom_n_t2 %>% transmute(dz_2001, total, house, nonhouse = flat + caravan + shared)
# checked - all OK

# car 
car_n_t1 <- read_csv("output_data/dz_2001/car_2001.csv")
car_n_t2 <- read_csv("output_data/dz_2001/car_2011.csv")

# inconsistency between t1 and t2# t1 already in correct form

car_2_t1 <- car_n_t1 %>% select(dz_2001, total, some, none) 
car_2_t2 <- car_n_t2 %>% transmute(dz_2001, total, some = one + two + three + four, none)

# both ok now and checked 



# cob 

cob_n_t1 <- read_csv("output_data/dz_2001/cob_2001.csv")
cob_n_t2 <- read_csv("output_data/dz_2001/cob_2011.csv")

cob_2_t1 <- cob_n_t1 %>% transmute(dz_2001, total, scotland, nonscot = ruk + elsewhere)
cob_2_t2 <- cob_n_t2 %>% transmute(dz_2001, total, scotland, nonscot = ruk + elsewhere)

# checked, both OK


# general health 

gh_n_t1 <- read_csv("output_data/dz_2001/general_health_2001.csv")
gh_n_t2 <- read_csv("output_data/dz_2001/general_health_2011.csv")
# already binary
gh_2_t1 <- gh_n_t1
gh_2_t2 <- gh_n_t2


# llti

llti_n_t1 <- read_csv("output_data/dz_2001/llti_2001.csv")
llti_n_t2 <- read_csv("output_data/dz_2001/llti_2011.csv")
# already binary
llti_2_t1 <- llti_n_t1 %>% select(dz_2001, total, no_llti, llti)
llti_2_t2 <- llti_n_t2 %>% select(dz_2001, total, no_llti, llti)


# marstat

ms_n_t1 <- read_csv("output_data/dz_2001/marital_status_2001.csv")
ms_n_t2 <- read_csv("output_data/dz_2001/marital_status_2011.csv")

# different categories 

ms_2_t1 <- ms_n_t1 %>% select(dz_2001, total, married, single)

# checked t1, ok

# checked t2, ok

ms_2_t2 <- ms_n_t2 %>% select(dz_2001, total, married, single)



## nssec

nssec_n_t1 <- read_csv("output_data/dz_2001/nssec_2001.csv")
nssec_n_t2 <- read_csv("output_data/dz_2001/nssec_2011.csv")

# for this should exclude students and other, then recalc total, and make binary break above intermed

nssec_2_t1 <- nssec_n_t1 %>% transmute(
  dz_2001, 
  higher = higher_man + lower_man, 
  lower = small_self + intermed + routine
  ) %>% 
  mutate(total = higher + lower) %>% 
  select(dz_2001, total, higher, lower) 

nssec_2_t2 <- nssec_n_t2 %>% transmute(
  dz_2001, 
  higher = higher_man + lower_man, 
  lower = small_self + intermed + routine
) %>% 
  mutate(total = higher + lower) %>% 
  select(dz_2001, total, higher, lower) 


# religion

rel_n_t1 <- read_csv("output_data/dz_2001/religion_2001.csv")
rel_n_t2 <- read_csv("output_data/dz_2001/religion_2011.csv")


# divide - religious/nonreligious

rel_2_t1 <- rel_n_t1 %>% 
  transmute(dz_2001, total, 
            religious = non_catholic_christian + catholic_christian + jewish + muslim + other, 
            nonreligious = none)
# checked, OK
rel_2_t2 <- rel_n_t2 %>% 
  transmute(dz_2001, total, 
            religious = non_catholic_christian + catholic_christian + jewish + muslim + other, 
            nonreligious = none)

# checked, ok


# student, pensioner or other
spo_n_t1 <- read_csv("output_data/dz_2001/studentspensionersother_2001.csv")
spo_n_t2 <- read_csv("output_data/dz_2001/studentspensionersother_2011.csv")

po_2_t1 <- spo_n_t1 %>% transmute(dz_2001, total, pensioner, nonpensioner = student + other)
po_2_t2 <- spo_n_t2 %>% transmute(dz_2001, total, pensioner, nonpensioner = student + other)
# checked both, OK
so_2_t1 <- spo_n_t1 %>% transmute(dz_2001, total, student, nonstudent = pensioner + other)
so_2_t2 <- spo_n_t2 %>% transmute(dz_2001, total, student, nonstudent = pensioner + other)



# tenure 

tenure_n_t1 <- read_csv("output_data/dz_2001/tenure_2001.csv")
tenure_n_t2 <- read_csv("output_data/dz_2001/tenure_2011.csv")

# owned vs nonowned 

tenure_2_t1 <- tenure_n_t1 %>% 
  transmute(dz_2001, total, owned, nonowned = social + rented + rentfree)

tenure_2_t2 <- tenure_n_t2 %>% 
  transmute(dz_2001, total, owned, nonowned = social + rented + rentfree)
# checked both, OK



# employed/not employed

employed_n_t1 <- read_csv("output_data/dz_2001/economic_activity_bigger_2001.csv")
employed_n_t2 <- read_csv("output_data/dz_2001/economic_activity_bigger_2011.csv")

employed_2_t1 <- employed_n_t1 %>% 
  transmute(dz_2001, total, employed, nonemployed = unemployed + student + retired + homemaker + sick + inactive_other)

employed_2_t2 <- employed_n_t2 %>% 
  transmute(dz_2001, total, employed, nonemployed = unemployed + student + retired + homemaker + sick + inactive_other)


# economic inactive 

ecact_n_t1 <- read_csv("output_data/dz_2001/economic_activity_simple_2001.csv")
ecact_n_t2 <- read_csv("output_data/dz_2001/economic_activity_simple_2011.csv")

# already binary

ecact_2_t1 <- ecact_n_t1
ecact_2_t2 <- ecact_n_t2


# now to write these all out 

write_csv(x = accom_2_t1, path = "output_data/dz_2001/binary/accom_2001.csv")
write_csv(x = accom_2_t2, path = "output_data/dz_2001/binary/accom_2011.csv")

write_csv(x = car_2_t1, path = "output_data/dz_2001/binary/car_2001.csv")
write_csv(x = car_2_t2, path = "output_data/dz_2001/binary/car_2011.csv")

write_csv(x = cob_2_t1, path = "output_data/dz_2001/binary/cob_2001.csv")
write_csv(x = cob_2_t2, path = "output_data/dz_2001/binary/cob_2011.csv")

# ethnicity already done

write_csv(x = gh_2_t1, path = "output_data/dz_2001/binary/generalhealth_2001.csv")
write_csv(x = gh_2_t2, path = "output_data/dz_2001/binary/generalhealth_2011.csv")

write_csv(x = llti_2_t1, path = "output_data/dz_2001/binary/llti_2001.csv")
write_csv(x = llti_2_t2, path = "output_data/dz_2001/binary/llti_2011.csv")

write_csv(x = ms_2_t1, path = "output_data/dz_2001/binary/maritalstatus_2001.csv")
write_csv(x = ms_2_t2, path = "output_data/dz_2001/binary/maritalstatus_2011.csv")

write_csv(x = nssec_2_t1, path = "output_data/dz_2001/binary/nssec_2001.csv")
write_csv(x = nssec_2_t2, path = "output_data/dz_2001/binary/nssec_2011.csv")

write_csv(x = rel_2_t1, path = "output_data/dz_2001/binary/religion_2001.csv")
write_csv(x = rel_2_t2, path = "output_data/dz_2001/binary/religion_2011.csv")

write_csv(x = po_2_t1, path = "output_data/dz_2001/binary/pensioners_2001.csv")
write_csv(x = po_2_t2, path = "output_data/dz_2001/binary/pensioners_2011.csv")

write_csv(x = so_2_t1, path = "output_data/dz_2001/binary/students_2001.csv")
write_csv(x = so_2_t2, path = "output_data/dz_2001/binary/students_2011.csv")


write_csv(x = tenure_2_t1, path = "output_data/dz_2001/binary/homeowners_2001.csv")
write_csv(x = tenure_2_t2, path = "output_data/dz_2001/binary/homeowners_2011.csv")


write_csv(x = employed_2_t1, path = "output_data/dz_2001/binary/employed_2001.csv")
write_csv(x = employed_2_t2, path = "output_data/dz_2001/binary/employed_2011.csv")

write_csv(x = ecact_2_t1, path = "output_data/dz_2001/binary/inactive_2001.csv")
write_csv(x = ecact_2_t2, path = "output_data/dz_2001/binary/inactive_2011.csv")


##As above but dz_2011




# accom
# car
# cob
# ecact
# eth 
# general health
# llti
# marital status
# nssec
# religion
# student pensioner
# tenure
# employed/not employed
# economic inactive



# accom
accom_n_t1 <- read_csv("output_data/dz_2011/accom_2001.csv")
accom_n_t2 <- read_csv("output_data/dz_2011/accom_2011.csv")

# house/nonhouse

accom_2_t1 <- accom_n_t1 %>% transmute(dz_2011, total, house, nonhouse = flat + caravan + shared)
# checked = all OK 

accom_2_t2 <- accom_n_t2 %>% transmute(dz_2011, total, house, nonhouse = flat + caravan + shared)
# checked - all OK

# car 
car_n_t1 <- read_csv("output_data/dz_2011/car_2001.csv")
car_n_t2 <- read_csv("output_data/dz_2011/car_2011.csv")

# inconsistency between t1 and t2# t1 already in correct form

car_2_t1 <- car_n_t1 %>% transmute(dz_2011, total, some, none)
car_2_t2 <- car_n_t2 %>% transmute(dz_2011, total, some = one + two + three + four, none )

# both ok now and checked 



# cob 

cob_n_t1 <- read_csv("output_data/dz_2011/cob_2001.csv")
cob_n_t2 <- read_csv("output_data/dz_2011/cob_2011.csv")

cob_2_t1 <- cob_n_t1 %>% transmute(dz_2011, total, scotland, nonscot = ruk + elsewhere)
cob_2_t2 <- cob_n_t2 %>% transmute(dz_2011, total, scotland, nonscot = ruk + elsewhere)

# checked, both OK


# general health 

gh_n_t1 <- read_csv("output_data/dz_2011/general_health_2001.csv")
gh_n_t2 <- read_csv("output_data/dz_2011/general_health_2011.csv")
# already binary
gh_2_t1 <- gh_n_t1
gh_2_t2 <- gh_n_t2


# llti

llti_n_t1 <- read_csv("output_data/dz_2011/llti_2001.csv")
llti_n_t2 <- read_csv("output_data/dz_2011/llti_2011.csv")
# already binary
llti_2_t1 <- llti_n_t1 %>% select(dz_2011, total, no_llti, llti) 
llti_2_t2 <- llti_n_t2 %>% select(dz_2011, total, no_llti, llti)


# marstat

ms_n_t1 <- read_csv("output_data/dz_2011/marital_status_2001.csv")
ms_n_t2 <- read_csv("output_data/dz_2011/marital_status_2011.csv")

# different categories 

ms_2_t1 <- ms_n_t1 %>% 
  transmute(
    dz_2011, 
    total, 
    married, 
    single 
  )

# checked t1, ok

# checked t2, ok

ms_2_t2 <- ms_n_t2 %>% 
  transmute(dz_2011, total, married, single)



## nssec

nssec_n_t1 <- read_csv("output_data/dz_2011/nssec_2001.csv")
nssec_n_t2 <- read_csv("output_data/dz_2011/nssec_2011.csv")

# for this should exclude students and other, then recalc total, and make binary break above intermed

nssec_2_t1 <- nssec_n_t1 %>% transmute(
  dz_2011, 
  higher = higher_man + lower_man, 
  lower = small_self + intermed + routine
) %>% 
  mutate(total = higher + lower) %>% 
  select(dz_2011, total, higher, lower) 

nssec_2_t2 <- nssec_n_t2 %>% transmute(
  dz_2011, 
  higher = higher_man + lower_man, 
  lower = small_self + intermed + routine
) %>% 
  mutate(total = higher + lower) %>% 
  select(dz_2011, total, higher, lower) 


# religion

rel_n_t1 <- read_csv("output_data/dz_2011/religion_2001.csv")
rel_n_t2 <- read_csv("output_data/dz_2011/religion_2011.csv")


# divide - religious/nonreligious

rel_2_t1 <- rel_n_t1 %>% 
  transmute(dz_2011, total, 
            religious = non_catholic_christian + catholic_christian + jewish + muslim + other, 
            nonreligious = none)
# checked, OK
rel_2_t2 <- rel_n_t2 %>% 
  transmute(dz_2011, total, 
            religious = non_catholic_christian + catholic_christian + jewish + muslim + other, 
            nonreligious = none)

# checked, ok


# student, pensioner or other
spo_n_t1 <- read_csv("output_data/dz_2011/studentspensionersother_2001.csv")
spo_n_t2 <- read_csv("output_data/dz_2011/studentspensionersother_2011.csv")

po_2_t1 <- spo_n_t1 %>% transmute(dz_2011, total, pensioner, nonpensioner = student + other)
po_2_t2 <- spo_n_t2 %>% transmute(dz_2011, total, pensioner, nonpensioner = student + other)
# checked both, OK
so_2_t1 <- spo_n_t1 %>% transmute(dz_2011, total, student, nonstudent = pensioner + other)
so_2_t2 <- spo_n_t2 %>% transmute(dz_2011, total, student, nonstudent = pensioner + other)



# tenure 

tenure_n_t1 <- read_csv("output_data/dz_2011/tenure_2001.csv")
tenure_n_t2 <- read_csv("output_data/dz_2011/tenure_2011.csv")

# owned vs nonowned 

tenure_2_t1 <- tenure_n_t1 %>% 
  transmute(dz_2011, total, owned, nonowned = social + rented + rentfree)

tenure_2_t2 <- tenure_n_t2 %>% 
  transmute(dz_2011, total, owned, nonowned = social + rented + rentfree)
# checked both, OK



# employed/not employed

employed_n_t1 <- read_csv("output_data/dz_2011/economic_activity_bigger_2001.csv")
employed_n_t2 <- read_csv("output_data/dz_2011/economic_activity_bigger_2011.csv")

employed_2_t1 <- employed_n_t1 %>% 
  transmute(dz_2011, total, employed, nonemployed = unemployed + student + retired + homemaker + sick + inactive_other)

employed_2_t2 <- employed_n_t2 %>% 
  transmute(dz_2011, total, employed, nonemployed = unemployed + student + retired + homemaker + sick + inactive_other)


# economic inactive 

ecact_n_t1 <- read_csv("output_data/dz_2011/economic_activity_simple_2001.csv")
ecact_n_t2 <- read_csv("output_data/dz_2011/economic_activity_simple_2011.csv")

# already binary

ecact_2_t1 <- ecact_n_t1
ecact_2_t2 <- ecact_n_t2


# Ethnicity


eth_n_t1 <- read_csv("output_data/dz_2011/eth_2001.csv")
eth_n_t2 <- read_csv("output_data/dz_2011/eth_2011.csv")

eth_2_t1 <- eth_n_t1 %>% 
  transmute(dz_2011, total, white, nonwhite = pakistani + chinese + other)

eth_2_t2 <- eth_n_t2 %>% 
  transmute(dz_2011, total, white, nonwhite = pakistani + chinese + other)


# now to write these all out 
#dir.create("output_data/dz_2011/binary/")
write_csv(x = accom_2_t1, path = "output_data/dz_2011/binary/accom_2001.csv")
write_csv(x = accom_2_t2, path = "output_data/dz_2011/binary/accom_2011.csv")

write_csv(x = car_2_t1, path = "output_data/dz_2011/binary/car_2001.csv")
write_csv(x = car_2_t2, path = "output_data/dz_2011/binary/car_2011.csv")

write_csv(x = cob_2_t1, path = "output_data/dz_2011/binary/cob_2001.csv")
write_csv(x = cob_2_t2, path = "output_data/dz_2011/binary/cob_2011.csv")

write_csv(x = eth_2_t1, path = "output_data/dz_2011/binary/eth_2001.csv")
write_csv(x = eth_2_t2, path = "output_data/dz_2011/binary/eth_2011.csv")


write_csv(x = gh_2_t1, path = "output_data/dz_2011/binary/generalhealth_2001.csv")
write_csv(x = gh_2_t2, path = "output_data/dz_2011/binary/generalhealth_2011.csv")

write_csv(x = llti_2_t1, path = "output_data/dz_2011/binary/llti_2001.csv")
write_csv(x = llti_2_t2, path = "output_data/dz_2011/binary/llti_2011.csv")

write_csv(x = ms_2_t1, path = "output_data/dz_2011/binary/maritalstatus_2001.csv")
write_csv(x = ms_2_t2, path = "output_data/dz_2011/binary/maritalstatus_2011.csv")

write_csv(x = nssec_2_t1, path = "output_data/dz_2011/binary/nssec_2001.csv")
write_csv(x = nssec_2_t2, path = "output_data/dz_2011/binary/nssec_2011.csv")

write_csv(x = rel_2_t1, path = "output_data/dz_2011/binary/religion_2001.csv")
write_csv(x = rel_2_t2, path = "output_data/dz_2011/binary/religion_2011.csv")

write_csv(x = po_2_t1, path = "output_data/dz_2011/binary/pensioners_2001.csv")
write_csv(x = po_2_t2, path = "output_data/dz_2011/binary/pensioners_2011.csv")

write_csv(x = so_2_t1, path = "output_data/dz_2011/binary/students_2001.csv")
write_csv(x = so_2_t2, path = "output_data/dz_2011/binary/students_2011.csv")


write_csv(x = tenure_2_t1, path = "output_data/dz_2011/binary/homeowners_2001.csv")
write_csv(x = tenure_2_t2, path = "output_data/dz_2011/binary/homeowners_2011.csv")


write_csv(x = employed_2_t1, path = "output_data/dz_2011/binary/employed_2001.csv")
write_csv(x = employed_2_t2, path = "output_data/dz_2011/binary/employed_2011.csv")

write_csv(x = ecact_2_t1, path = "output_data/dz_2011/binary/inactive_2001.csv")
write_csv(x = ecact_2_t2, path = "output_data/dz_2011/binary/inactive_2011.csv")

