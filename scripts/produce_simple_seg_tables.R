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
require(GGally) # for ggpairs



# load data ---------------------------------------------------------------

dta <- read_excel(path = "geoseg_outputs/geoseg_outputs.xlsx", sheet = "tidy")


minority_vars <- c(
  "not_good", "nonhouse", "lower", 
  "llti", "single", "some", "nonrlgs", 
  "nonscot", "nonwhite", "pensinr",
  "student", "employd", "inactive",
  "owned"
  )

minority_labels <- c(
  "Poor Health", "Non-house", "Grades 3 to 5", 
  "LLTI", "Single", "Has Car", "Non-religious", 
  "Not Scottish", "Not White", "Pensioner",
  "Student", "Employed", "Inactive",
  "Home Owner"
  )

names(minority_labels) <- minority_vars


propchange <- dta %>% 
  filter(variable %in% minority_vars) %>% 
  mutate(variable = minority_labels[variable]) %>%  # This converts the variables to prettier names
  gather(key = index, value = value, -ttwa, -table, -year, -variable) %>% 
  spread(year, value) %>%
  mutate(change = (`2011` - `2001`) / `2001`) %>% 
  select(ttwa, variable, index, change)



pointchange <- dta %>% 
  filter(variable %in% minority_vars) %>% 
  mutate(variable = minority_labels[variable]) %>%  # This converts the variables to prettier names
  gather(key = index, value = value, -ttwa, -table, -year, -variable) %>% 
  spread(year, value) %>%
  mutate(change = `2011` - `2001` ) %>% 
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



wb <- createWorkbook()
d_ply(pointchange, .(variable), fn)
saveWorkbook(wb, file = "tables/pointchange_in_segs.xlsx")




# barcharts, 
# for variables IS ETA2 ACO ACL ACE

# For following clusters of measures

# socioeconomic
# SES
# employed
# car
# home owners

pointchange  %>% 
  rename(City = ttwa) %>% 
  filter(index %in% c("IS", "Eta2", "ACO", "ACL", "ACE"))  %>% 
  mutate(index = recode(
    index, 
    recodes = "
  'IS' = 'More Uneven';
  'Eta2' = 'More Isolated';
  'ACO' = 'More Spatially Dense';
  'ACL' = 'More Clustered';
  'ACE' = 'More Central'
", levels = c("More Uneven", "More Isolated", "More Spatially Dense", "More Clustered", "More Central")
    )) %>% 
  filter(variable %in% c("Grades 3 to 5", "Employed", "Has Car", "Home Owner"))  %>% 
  ggplot(.) + 
  geom_bar(aes( y = change, x = index, group = City, fill = City), stat = "identity", position = "dodge") + 
  facet_wrap(~ variable) + 
  labs(x = "Segregation measure", y = "Point change between 2001 and 2011", main = "Socioeconomic change") +
  theme(axis.text.x = element_text(angle = 90))


ggsave("figures/bar_change_socioeconomic.png", width = 15, height = 15, dpi = 150, units = "cm")

# ethnonational 
# non white
# non scottish
# non religious

pointchange  %>% 
  rename(City = ttwa) %>% 
  filter(index %in% c("IS", "Eta2", "ACO", "ACL", "ACE"))  %>% 
  mutate(index = recode(
    index, 
    recodes = "
    'IS' = 'More Uneven';
    'Eta2' = 'More Isolated';
    'ACO' = 'More Spatially Dense';
    'ACL' = 'More Clustered';
    'ACE' = 'More Central'
    ", levels = c("More Uneven", "More Isolated", "More Spatially Dense", "More Clustered", "More Central")
  )) %>% 
  filter(variable %in% c("Not White", "Not Scottish", "Non-religious"))  %>% 
  ggplot(.) + 
  geom_bar(aes( y = change, x = index, group = City, fill = City), stat = "identity", position = "dodge") + 
  facet_wrap(~ variable) + 
  labs(x = "Segregation measure", y = "Point change between 2001 and 2011", title = "Ethno-national Change") +
  theme(axis.text.x = element_text(angle = 90))

ggsave("figures/bar_change_ethnonational.png", width = 15, height = 12, dpi = 150, units = "cm")

# demographic
# student
# single 
# pensioner
# self assessed health
# llti

pointchange  %>% 
  rename(City = ttwa) %>% 
  filter(index %in% c("IS", "Eta2", "ACO", "ACL", "ACE"))  %>% 
  mutate(index = recode(
    index, 
    recodes = "
    'IS' = 'More Uneven';
    'Eta2' = 'More Isolated';
    'ACO' = 'More Spatially Dense';
    'ACL' = 'More Clustered';
    'ACE' = 'More Central'
    ", levels = c("More Uneven", "More Isolated", "More Spatially Dense", "More Clustered", "More Central")
  )) %>% 
  filter(variable %in% c("Student", "Pensioner", "Single", "LLTI"))  %>% 
  ggplot(.) + 
  geom_bar(aes( y = change, x = index, group = City, fill = City), stat = "identity", position = "dodge") + 
  facet_wrap(~ variable) + 
  labs(x = "Segregation measure", y = "Point change between 2001 and 2011", title = "Demographic Change") +
  theme(axis.text.x = element_text(angle = 90))

ggsave("figures/bar_change_demographic.png", width = 15, height = 15, dpi = 150, units = "cm")

# build environment
# house
# car

pointchange  %>% 
  rename(City = ttwa) %>% 
  filter(index %in% c("IS", "Eta2", "ACO", "ACL", "ACE"))  %>%   
  mutate(index = recode(
    index, 
    recodes = "
  'IS' = 'More Uneven';
  'Eta2' = 'More Isolated';
  'ACO' = 'More Spatially Dense';
  'ACL' = 'More Clustered';
  'ACE' = 'More Central'
", levels = c("More Uneven", "More Isolated", "More Spatially Dense", "More Clustered", "More Central")
  )) %>% 
  filter(variable %in% c("Non-house", "Has Car"))  %>% 
  ggplot(.) + 
  geom_bar(aes( y = change, x = index, group = City, fill = City), stat = "identity", position = "dodge") + 
  facet_wrap(~ variable) + 
  labs(x = "Segregation measure", y = "Point change between 2001 and 2011", title = "Built Environment Change") +
  theme(axis.text.x = element_text(angle = 90))

ggsave("figures/bar_change_built.png", width = 15, height = 12, dpi = 150, units = "cm")

# 





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
    ),
    index = factor(
      index, 
      levels = c(
        "IS", "IS(adj)", "IS(w)", "IS(s)", "H", "G", "A(0.1)", "A(0.5)", "A(0.9)",
        "xPx", "Eta2",
        "DEL", "ACO",
        "ACL", "Pxx", "Pxx Exp(-Dij)", "DPxx",
        "PCC", "ACE"
      )
    ) # convert to factor so variables are shown in correct order
  ) %>% 
  select(ttwa, variable, dimension, index, change) 
  


# Evenness comparisons

# IS 
# H
# G
# A(0.5)



propchange  %>% 
  filter(index %in% c("IS", "H"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `IS`, y = H, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 
  
ggsave("figures/comparisons/H_vs_IS.png", dpi = 300, height = 20, width = 20, units = "cm")

propchange  %>% 
  filter(index %in% c("IS", "G"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `IS`, y = G, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 

ggsave("figures/comparisons/G_vs_IS.png", dpi = 300, height = 20, width = 20, units = "cm")

propchange  %>% 
  filter(index %in% c("IS", "A(0.5)"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `IS`, y = `A(0.5)`, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 
  
  ggsave("figures/comparisons/A_vs_IS.png", dpi = 300, height = 20, width = 20, units = "cm")

propchange  %>% 
  filter(index %in% c("G", "H"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `H`, y = `G`, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 
  
  ggsave("figures/comparisons/G_vs_H.png", dpi = 300, height = 20, width = 20, units = "cm")

propchange  %>% 
  filter(index %in% c("G", "A(0.5)"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `A(0.5)`, y = `G`, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 
  
  ggsave("figures/comparisons/G_vs_A.png", dpi = 300, height = 20, width = 20, units = "cm")

# Exposure comparison


propchange  %>% 
  filter(index %in% c("xPx", "Eta2"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `xPx`, y = `Eta2`, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 
  
  ggsave("figures/comparisons/exposure.png", dpi = 300, height = 20, width = 20, units = "cm")

# Clustering comparisons

propchange  %>% 
  filter(index %in% c("ACL", "DPxx"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `ACL`, y = `DPxx`, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  theme_minimal() + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 
  
  ggsave("figures/comparisons/clustering_ACL_DPxx.png", dpi = 300, height = 20, width = 20, units = "cm")

propchange  %>% 
  filter(index %in% c("ACL", "Pxx"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `ACL`, y = `Pxx`, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) +
  theme_minimal() + 
  scale_colour_manual(
    values = c(
    "red", "darkred",
    "orange", "darkorange",
    "darkgrey", "black",
    "lightgreen", "green", "darkgreen",
    "blue", "darkblue",
    "purple", "pink"
    )
    ) +
  guides(col = guide_legend(title = "Census Group"))  
  
  ggsave("figures/comparisons/clustering_ACL_Pxx.png", dpi = 300, height = 20, width = 20, units = "cm")



propchange  %>% 
  filter(index %in% c("ACL", "Pxx Exp(-Dij)"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `ACL`, y = `Pxx Exp(-Dij)`, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 
  
  ggsave("figures/comparisons/clustering_ACL_Pxx Exp(-Dij).png", dpi = 300, height = 20, width = 20, units = "cm")


propchange  %>% 
  filter(index %in% c("ACL", "DPxx"))  %>% 
  spread(index, change)  %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
  ggplot(., mapping =aes(x = `ACL`, y = `DPxx`, group = variable, colour = variable)) + 
  geom_text(aes(label = ttwa)) + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") + 
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  scale_colour_manual(
    values = c(
      "red", "darkred",
      "orange", "darkorange",
      "darkgrey", "black",
      "lightgreen", "green", "darkgreen",
      "blue", "darkblue",
      "purple", "pink"
    )
  ) +
  guides(col = guide_legend(title = "Census Group")) 
  
  ggsave("figures/comparisons/clustering_ACL_DPxx.png", dpi = 300, height = 20, width = 20, units = "cm")


# concentration
  
  
  propchange  %>% 
    filter(index %in% c("DEL", "ACO"))  %>% 
    spread(index, change)  %>% 
    mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'")) %>% 
    ggplot(., mapping =aes(x = `DEL`, y = `ACO`, group = variable, colour = variable)) + 
    geom_text(aes(label = ttwa)) + 
    stat_smooth(method = "lm", se = F, linetype = "dashed") + 
    theme_minimal() +
    geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
    scale_colour_manual(
      values = c(
        "red", "darkred",
        "orange", "darkorange",
        "darkgrey", "black",
        "lightgreen", "green", "darkgreen",
        "blue", "darkblue",
        "purple", "pink"
      )
    ) +
    guides(col = guide_legend(title = "Census Group")) 
  
  ggsave("figures/comparisons/concentration.png", dpi = 300, height = 20, width = 20, units = "cm")
  
  
  
  
declutter <- function(x){
  output <- x %>% str_replace_all("\\(", "") %>% 
    str_replace_all("\\)", "") %>% 
    str_replace_all(" ", "_") %>% 
    str_replace_all("\\-", "less")
  return(output)  
}

png(filename = "figures/ggpairs_example.png", height = 80, width = 80, units = "cm", res = 300)
propchange  %>% 
  select(-dimension)  %>% 
  filter(index %in% c(
    "IS", "H", "G", "A(0.5)",
    "xPx", "Eta2",
    "DEL", "ACO",
    "ACL", "Pxx", 
    "PCC", "ACE"                  
                      )) %>% 
  mutate(index = declutter(index))  %>% 
  spread(index, change)   %>%  
  select(
    ttwa, variable, 
    IS, H, G, `A0.5`,
    xPx, Eta2,
    DEL, ACO,
    ACL, Pxx, 
    PCC, ACE    
         ) %>% 
  mutate(ttwa = recode(ttwa, "'Aberdeen' = 'A'; 'Glasgow' = 'G'; 'Dundee' = 'D'; 'Edinburgh' = 'E'"))  %>% 
  ggpairs(
    data = ., 
    mapping = ggplot2::aes(label = ttwa, group = variable, colour = variable), 
    columns = 3:14,
    diag = NULL,
    lower = list(continuous = "points")
  ) 
dev.off()











