rm(list = ls())


require(readr)
require(readxl)

require(car)
require(stringr)

require(tidyr)
require(dplyr)


require(ggplot2)



dta <- read_excel(path = "input_data/geosegregation outputs.xlsx", sheet = "flat_formatted")


# #
# IS: Segregation index; 
# IS(adj): Segregation index adjusted for tract contiguity; 
# IS(w): Segregation index adjusted for contiguous tract boundary lengths; 
# IS(s): Segregation index adjusted for contiguous tract boundary lengths and perimeter/area ratio; 
# H: Entropy index; 
# G: Gini index; 
# A(0.1): Atkinson index; 
# A(0.5): Atkinson index; 
# A(0.9): Atkinson index; 
# xPx: Isolation index; 
# Eta2: Correlation ratio; 
# DEL: Delta index; 
# ACO: absolute concentration index; 
# ACL: Absolute clustering index; 
# Pxx: Mean proximity between members of group X; 
# Pxx Exp(-Dij): Mean proximity between members of group X (exp dij); 
# DPxx: The distance-decay isolation index.


# let's look just at the different types of segregation index

seg_indices = c("IS", "IS(adj)", "IS(w)", "IS(s)")

minority_group <- c("nonhouse", "none", "nonscot", "nonwhite", "not_good", "llti", "single", "lower", "pensioner", "noowned")

dta %>% 
  gather(key = measure, value = value, -Table, -Year, -Name) %>% 
  select(table = Table, year = Year, name = Name, measure, value) %>% 
  filter(name %in% minority_group) %>% 
  mutate(year = as.factor(year)) %>%  
  mutate(table = str_trim(table)) %>% 
  mutate(table = recode(table,
        recodes = "
        'accom' = 'Non-homeowners';
        'car' = 'Non-car owners';
        'cob' = 'Not Scottish';
        'ethnicity' = 'Non-white';
        'general health' = 'Not good health';
        'llti' = 'Has longstanding limiting illness';
        'marital status' = 'Single';
        'nssec' = 'Lower occupational group'
        "
                        )) %>% 
  filter(measure %in% seg_indices) %>% 
  ggplot(.) + 
  geom_bar(
    mapping = aes(x = measure, group = year, fill = year, y = value), 
    position = "dodge", stat= "identity"
    ) + 
  facet_wrap( ~ table) + 
  labs(x = "Segregation type", y = "Segregation value") + 
  scale_fill_brewer(palette = "Blues") + theme(panel.background = element_rect(fill = "darkgrey"))

ggsave("figures/segregation.png", height = 20, width = 20, units = "cm", dpi = 300)

# Now different Atkins thresholds

atkins_indices = c("A(0.1)", "A(0.5)", "A(0.9)")

minority_group <- c("nonhouse", "none", "nonscot", "nonwhite", "not_good", "llti", "single", "lower", "pensioner", "noowned")

dta %>% 
  gather(key = measure, value = value, -Table, -Year, -Name) %>% 
  select(table = Table, year = Year, name = Name, measure, value) %>% 
  filter(name %in% minority_group) %>% 
  mutate(year = as.factor(year)) %>%  
  mutate(table = str_trim(table)) %>% 
  mutate(table = recode(table,
                        recodes = "
                        'accom' = 'Non-homeowners';
                        'car' = 'Non-car owners';
                        'cob' = 'Not Scottish';
                        'ethnicity' = 'Non-white';
                        'general health' = 'Not good health';
                        'llti' = 'Has longstanding limiting illness';
                        'marital status' = 'Single';
                        'nssec' = 'Lower occupational group'
                        "
  )) %>% 
  filter(measure %in% atkins_indices) %>% 
  ggplot(.) + 
  geom_bar(
    mapping = aes(x = measure, group = year, fill = year, y = value), 
    position = "dodge", stat= "identity"
  ) + 
  facet_wrap( ~ table) + 
  labs(x = "Atkins threshold", y = "Atkins value") + 
  scale_fill_brewer(palette = "Blues") + theme(panel.background = element_rect(fill = "darkgrey"))

ggsave("figures/atkins.png", height = 20, width = 20, units = "cm", dpi = 300)


# Some basic versions of core indices
# IS: Segregation index; 
# H: Entropy index; 
# G: Gini index; 
# A(0.5): Atkinson index; 
# xPx: Isolation index; 
# Eta2: Correlation ratio; 
# DEL: Delta index; 
# ACO: absolute concentration index; 
# ACL: Absolute clustering index; 
# Pxx: Mean proximity between members of group X; 


core_indices = c("IS", "H", "G", "A(0.5)", "xPx", "Eta2", "DEL", "ACO", "ACL")

minority_group <- c("nonhouse", "none", "nonscot", "nonwhite", "not_good", "llti", "single", "lower", "pensioner", "noowned")

dta %>% 
  gather(key = measure, value = value, -Table, -Year, -Name) %>% 
  select(table = Table, year = Year, name = Name, measure, value) %>% 
  filter(name %in% minority_group) %>% 
  mutate(year = as.factor(year)) %>%  
  mutate(table = str_trim(table)) %>% 
  mutate(table = recode(table,
                        recodes = "
                        'accom' = 'Non-homeowners';
                        'car' = 'Non-car owners';
                        'cob' = 'Not Scottish';
                        'ethnicity' = 'Non-white';
                        'general health' = 'Not good health';
                        'llti' = 'Has longstanding limiting illness';
                        'marital status' = 'Single';
                        'nssec' = 'Lower occupational group'
                        "
  )) %>% 
  filter(measure %in% core_indices) %>% 
  ggplot(.) + 
  geom_bar(
    mapping = aes(x = measure, group = year, fill = year, y = value), 
    position = "dodge", stat= "identity"
  ) + 
  facet_wrap( ~ table) + 
  labs(x = "Index type", y = "Index value") + 
  scale_fill_brewer(palette = "Blues") + theme(panel.background = element_rect(fill = "darkgrey"))


ggsave("figures/core_segregation_indices.png", height = 20, width = 25, units = "cm", dpi = 300)

# show table of proportion increases and decreases in seg values from 2001 to 2011
dta %>% 
  gather(key = measure, value = value, -Table, -Year, -Name) %>% 
  select(table = Table, year = Year, name = Name, measure, value) %>% 
  filter(name %in% minority_group) %>% 
  mutate(year = as.factor(year)) %>%  
  mutate(table = str_trim(table)) %>% 
  mutate(table = recode(table,
                        recodes = "
                        'accom' = 'Non-homeowners';
                        'car' = 'Non-car owners';
                        'cob' = 'Not Scottish';
                        'ethnicity' = 'Non-white';
                        'general health' = 'Not good health';
                        'llti' = 'Has longstanding limiting illness';
                        'marital status' = 'Single';
                        'nssec' = 'Lower occupational group'
                        "
  )) %>% 
  filter(measure %in% seg_indices) %>% 
  spread(year, value) %>% 
  mutate(ratio = `2011` / `2001`) %>% 
  select(table, measure, ratio) %>% 
  spread(measure, ratio)


