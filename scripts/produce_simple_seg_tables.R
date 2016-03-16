rm(list = ls())

require(readxl)
require(xlsx)
require(readr)

require(car)
require(stringr)
require(plyr)
require(tidyr)
require(dplyr)

require(ggplot2)


# load data ---------------------------------------------------------------

dta <- read_excel(path = "geoseg_outputs/geoseg_outputs.xlsx", sheet = "tidy")


minority_vars <- c(
  "not_good", "nonhouse", "higher", 
  "llti", "single", "none", "nonrlgs", 
  "nonscot", "nonwhite", "pensinr" 
  )

minority_labels <- c(
  "Poor Health", "Non-house", "Grade 1 or 2", 
  "LLTI", "Single", "No Car", "Non-religious", 
  "Not Scottish", "Not White", "Pensioner")

names(minority_labels) <- minority_vars


propchange <- dta %>% 
  filter(variable %in% minority_vars) %>% 
#  mutate(variable = minority_labels[variable]) %>%  # This converts the variables to prettier names
  gather(key = index, value = value, -ttwa, -table, -year, -variable) %>% 
  spread(year, value) %>%
  mutate(change = (`2011` - `2001`) / `2001`) %>% 
  select(ttwa, variable, index, change)


# Let's create a separate tab for each table 


fn <- function(input){
  nm <- input$variable[1]
  
  output <- input %>% 
    select(ttwa, index, change) %>% 
    spread(ttwa, change)
  
  addDataFrame(
    x = output,
    sheet = createSheet(wb, sheetName =  nm)
  )
  
  return(NULL)
}

wb <- createWorkbook()
d_ply(propchange, .(variable), fn)
saveWorkbook(wb, file = "tables/change_in_segs.xlsx")



propchange <- propchange %>% 
  mutate(
    dimension = recode(
      var = index,
      recodes = 
      "
  c('IS', 'IS(adj)', 'IS(w)', 'IS(s)', 'H', 'G', 'A(0.1)', 'A(0.5)', 'A(0.9)') = 'Evenness';
  c('xPx', 'Eta2') = 'Exposure';
  c('DEL', 'ACO') = 'Concentration';
  c('ACL', 'Pxx', 'Pxx Exp(-Dij)', 'DPxx') = 'Clustering';
  c('PCC', 'ACE') = 'Centralisation'
      "
    )
  ) %>% 
  select(ttwa, variable, dimension, index, change)


propchange  %>% 
  filter(index %in% c("IS", "H", "G", "A(0.5)"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = H, y = IS)) + 
  geom_text(aes(label = ttwa)) + 
  facet_grid( ~variable, scales = "free")  +
  stat_smooth(method = "lm") + 
  theme_minimal()

propchange  %>% 
  filter(index %in% c("IS", "H", "G", "A(0.5)"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = G, y = IS)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm") + 
  facet_grid( ~variable, scales = "free")  + 
  theme_minimal()

propchange  %>% 
  filter(index %in% c("IS", "H", "G", "A(0.5)"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `A(0.5)`, y = IS)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm") + 
  facet_grid( ~variable, scales = "free")  + 
  theme_minimal() +
  coord_equal()



propchange  %>% 
  filter(index %in% c("IS", "H", "G", "A(0.5)"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = H, y = IS)) + 
  geom_text(aes(label = ttwa)) + 
  facet_grid( ~variable, scales = "free")  +
  stat_smooth(method = "lm") + 
  theme_minimal()

propchange  %>% 
  filter(index %in% c("IS", "H", "G", "A(0.5)"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = G, y = IS)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm") + 
  facet_grid( ~variable, scales = "free")  + 
  theme_minimal()


propchange  %>% 
  filter(index %in% c("xPx", "Eta2"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `Eta2`, y = xPx, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(y = 0)) + geom_vline(aes(x = 0))


propchange  %>% 
  filter(index %in% c("xPx", "Eta2"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `Eta2`, y = xPx)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  facet_grid(~variable, scales = "free") + 
  geom_hline(aes(y = 0), colour = "grey") + geom_vline(aes(x = 0), colour = "grey")




             









