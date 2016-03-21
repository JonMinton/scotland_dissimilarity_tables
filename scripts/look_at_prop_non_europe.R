# age by country of origin 

rm(list = ls())


require(readr)

require(repmis)

require(tidyr)
require(stringr)
require(dplyr)

require(ggplot2)


# specific tasks : 
# by local authority
# total number of migrants aged 16+ 
# median age
# percentage employed or self employed
# percentage students 
# percentage with degrees or above

# separately by EEA/Non-EEA status
# UK as one group




dta_2011 <- read_csv(
  file="E:/Dropbox/Data/Census/Scotland 2011 Census/unzipped/3bblk/Council Area/DC2103SC.csv"
)

names(dta_2011)[1:2] <- c("place", "age_group")

dta_tidied <- dta_2011  %>% 
  filter(str_detect(place, "^S12"))  %>% 
  gather("origin", "count", -place, -age_group)  %>% 
  filter(age_group != "All people")  

dta_tidied %>% 
  filter(origin %in% c("All people", "Europe: Total")) %>% 
  spread(origin, count) %>% 
  rename('total' = `All people`,  'europe' = `Europe: Total`) %>% 
  mutate(non_eea = total - europe) %>% 
  filter(!(age_group %in% c("0 to 4", "5 to 9", "10 to 14", "15"))) %>% 
  group_by(place) %>%
  select(-age_group) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(proportion = non_eea / total) %>% 
  arrange(desc(proportion))



