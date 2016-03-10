


rm(list = ls())

require(xlsx)
require(readr)

require(stringr)
require(plyr)
require(tidyr)
require(dplyr)


# characterisation of TTWA by majority/minority proportions ---------------


# Do not need spatial data for this, just merges between TTWA and other tables 


ttwa <- read_csv(file = "input_data/lookups/LSOA01_TTWA01_UK_LU.csv", col_types = "ccccccc")


# Proportion of people living in a house as a single household

houses_2001 <- read_csv(file = "output_data/dz_2001/binary/accom_2001.csv")

# Non-home
h_t1 <- houses_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, house, nonhouse) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonhouse/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_living_in_house_as_household = proportion_minority, rank)


houses_2011 <- read_csv(file = "output_data/dz_2001/binary/accom_2011.csv")

# Non-home
h_t2 <- houses_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, house, nonhouse) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonhouse/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_living_in_house_as_household = proportion_minority, rank)



h_both <- bind_rows(h_t1, h_t2)

vals_wide <- h_both %>% 
  select(-rank) %>% 
  spread(year, proportion_not_living_in_house_as_household) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)
rank_wide <- h_both %>% 
  select(-proportion_not_living_in_house_as_household) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


home_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)


# car owners 

# Proportion of people living in a house as a single household

car_2001 <- read_csv(file = "output_data/dz_2001/binary/car_2001.csv")

# Households without cars, 2001
c_t1 <- car_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, none, some) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = none/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_without_cars = proportion_minority, rank)


car_2011 <- read_csv(file = "output_data/dz_2001/binary/car_2011.csv")

# Households without cars, 2011
c_t2 <- car_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, none, some) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = none/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_without_cars = proportion_minority, rank)



c_both <- bind_rows(c_t1, c_t2)

vals_wide <- c_both %>% 
  select(-rank) %>% 
  spread(year, proportion_without_cars) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)
rank_wide <- c_both %>% 
  select(-proportion_without_cars) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


car_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)

#country of birth


cob_2001 <- read_csv(file = "output_data/dz_2001/binary/cob_2001.csv")

# country of birth, 2001
c_t1 <- cob_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, scotland, nonscot) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonscot/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_scottish = proportion_minority, rank)


cob_2011 <- read_csv(file = "output_data/dz_2001/binary/cob_2011.csv")

# country of birth, 2011
c_t2 <- cob_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, scotland, nonscot) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonscot/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_scottish = proportion_minority, rank)



c_both <- bind_rows(c_t1, c_t2)

vals_wide <- c_both %>% 
  select(-rank) %>% 
  spread(year, proportion_not_scottish) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)
rank_wide <- c_both %>% 
  select(-proportion_not_scottish) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


cob_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)


# ethnicity

eth_2001 <- read_csv(file = "output_data/dz_2001/binary/eth_2001.csv")

# ethnicity, 2001
e_t1 <- eth_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, white, nonwhite) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonwhite/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_white = proportion_minority, rank)


eth_2011 <- read_csv(file = "output_data/dz_2001/binary/eth_2011.csv")

# ethnicity, 2011
e_t2 <- eth_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, white, nonwhite) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonwhite/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_white = proportion_minority, rank)



e_both <- bind_rows(e_t1, e_t2)

vals_wide <- e_both %>% 
  select(-rank) %>% 
  spread(year, proportion_not_white) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)
rank_wide <- e_both %>% 
  select(-proportion_not_white) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


eth_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)

# general health

gh_2001 <- read_csv(file = "output_data/dz_2001/binary/generalhealth_2001.csv")

# general health, 2001
g_t1 <- gh_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, good, not_good) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = not_good/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_good_health = proportion_minority, rank)


gh_2011 <- read_csv(file = "output_data/dz_2001/binary/generalhealth_2011.csv")

# general_health, 2011
g_t2 <- gh_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, good, not_good) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = not_good/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_good_health = proportion_minority, rank)



g_both <- bind_rows(g_t1, g_t2)

vals_wide <- g_both %>% 
  select(-rank) %>% 
  spread(year, proportion_not_good_health) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)

rank_wide <- g_both %>% 
  select(-proportion_not_good_health) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


gh_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)

# home owners

h_2001 <- read_csv(file = "output_data/dz_2001/binary/homeowners_2001.csv")

# home owners, 2001
h_t1 <- h_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, owned, nonowned) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonowned/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_owning_homes = proportion_minority, rank)


h_2011 <- read_csv(file = "output_data/dz_2001/binary/homeowners_2011.csv")

# home owners, 2011
h_t2 <- h_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, owned, nonowned) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonowned/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_not_owning_homes = proportion_minority, rank)



h_both <- bind_rows(h_t1, h_t2)

vals_wide <- h_both %>% 
  select(-rank) %>% 
  spread(year, proportion_not_owning_homes) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)

rank_wide <- h_both %>% 
  select(-proportion_not_owning_homes) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


h_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)


# llti

l_2001 <- read_csv(file = "output_data/dz_2001/binary/llti_2001.csv")

# llti, 2001
l_t1 <- l_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, llti, no_llti) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = llti/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_with_llti = proportion_minority, rank)


l_2011 <- read_csv(file = "output_data/dz_2001/binary/llti_2011.csv")

# llti, 2011
l_t2 <- l_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, llti, no_llti) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = llti/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_with_llti = proportion_minority, rank)



l_both <- bind_rows(l_t1, l_t2)

vals_wide <- l_both %>% 
  select(-rank) %>% 
  spread(year, proportion_with_llti) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)

rank_wide <- l_both %>% 
  select(-proportion_with_llti) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


llti_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)


# marital status

m_2001 <- read_csv(file = "output_data/dz_2001/binary/maritalstatus_2001.csv")

# ms, 2001
m_t1 <- m_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, single, married) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = single/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_single = proportion_minority, rank)


m_2011 <- read_csv(file = "output_data/dz_2001/binary/maritalstatus_2011.csv")

# ms, 2011
m_t2 <- m_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, single, married) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = single/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_single = proportion_minority, rank)



m_both <- bind_rows(m_t1, m_t2)

vals_wide <- m_both %>% 
  select(-rank) %>% 
  spread(year, proportion_single) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)

rank_wide <- m_both %>% 
  select(-proportion_single) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


marstat_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)


# nssec 


n_2001 <- read_csv(file = "output_data/dz_2001/binary/nssec_2001.csv")

# nssec, 2001
n_t1 <- n_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, higher, lower) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = higher/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_higher_nssec = proportion_minority, rank)


n_2011 <- read_csv(file = "output_data/dz_2001/binary/nssec_2011.csv")

# ms, 2011
n_t2 <- n_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, higher, lower) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = higher/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_higher_nssec = proportion_minority, rank)



n_both <- bind_rows(n_t1, n_t2)

vals_wide <- n_both %>% 
  select(-rank) %>% 
  spread(year, proportion_higher_nssec) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)

rank_wide <- n_both %>% 
  select(-proportion_higher_nssec) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


nssec_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)


# pensioners


p_2001 <- read_csv(file = "output_data/dz_2001/binary/pensioners_2001.csv")

# pensioners, 2001
p_t1 <- p_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, pensioner, nonpensioner) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = pensioner/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_pensioner = proportion_minority, rank)


p_2011 <- read_csv(file = "output_data/dz_2001/binary/pensioners_2011.csv")

# pensioners, 2011
p_t2 <- p_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, pensioner, nonpensioner) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = pensioner/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_pensioner = proportion_minority, rank)



p_both <- bind_rows(p_t1, p_t2)

vals_wide <- p_both %>% 
  select(-rank) %>% 
  spread(year, proportion_pensioner) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)

rank_wide <- p_both %>% 
  select(-proportion_pensioner) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


pensioner_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)




# religion


r_2001 <- read_csv(file = "output_data/dz_2001/binary/religion_2001.csv")

# religiousness, 2001
r_t1 <- r_2001 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, religious, nonreligious) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonreligious/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2001) %>% 
  select(ttwa = TTWA01NM, year, proportion_nonreligious = proportion_minority, rank)


r_2011 <- read_csv(file = "output_data/dz_2001/binary/religion_2011.csv")

# religiousness, 2011
r_t2 <- r_2011 %>% 
  left_join(ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(TTWA01NM, total, religious, nonreligious) %>% 
  group_by(TTWA01NM) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion_minority = nonreligious/total) %>% 
  arrange(desc(proportion_minority)) %>%
  mutate(rank = rank(desc(proportion_minority))) %>% 
  mutate(year = 2011) %>% 
  select(ttwa = TTWA01NM, year, proportion_nonreligious = proportion_minority, rank)



r_both <- bind_rows(r_t1, r_t2)

vals_wide <- r_both %>% 
  select(-rank) %>% 
  spread(year, proportion_nonreligious) %>% 
  rename(prop_2001= `2001`, prop_2011 = `2011`)

rank_wide <- r_both %>% 
  select(-proportion_nonreligious) %>% 
  spread(year, rank) %>% 
  rename(rank_2001= `2001`, rank_2011 = `2011`)


religious_rank_change <- vals_wide %>% 
  inner_join(rank_wide) %>% 
  arrange(rank_2001) %>% 
  mutate(change_in_rank = rank_2011 - rank_2001)

# write out proportions to excel tabs


wb <- createWorkbook()

addDataFrame(
  x = car_rank_change,
  sheet = createSheet(wb, sheetName = "car_rank_change")
)

addDataFrame(
  x = cob_rank_change,
  sheet = createSheet(wb, sheetName = "cob_rank_change")
)

addDataFrame(
  x = eth_rank_change,
  sheet = createSheet(wb, sheetName = "eth_rank_change")
)

addDataFrame(
  x = gh_rank_change,
  sheet = createSheet(wb, sheetName = "gh_rank_change")
)

addDataFrame(
  x = home_rank_change,
  sheet = createSheet(wb, sheetName = "home_rank_change")
)

addDataFrame(
  x = h_rank_change,
  sheet = createSheet(wb, sheetName = "h_rank_change")
)

addDataFrame(
  x = llti_rank_change,
  sheet = createSheet(wb, sheetName = "llti_rank_change")
)

addDataFrame(
  x = marstat_rank_change,
  sheet = createSheet(wb, sheetName = "marstat_rank_change")
)

addDataFrame(
  x = nssec_rank_change,
  sheet = createSheet(wb, sheetName = "nsssec_rank_change")
)

addDataFrame(
  x = pensioner_rank_change,
  sheet = createSheet(wb, sheetName = "pensioner_rank_change")
)

addDataFrame(
  x = religious_rank_change,
  sheet = createSheet(wb, sheetName = "religious_rank_change")
)


saveWorkbook(wb, file = "tables/proportion_ranks.xlsx")


  
)

