rm(list = ls())

require(readxl)
require(xlsx)
require(readr)

require(stringr)
require(plyr)
require(tidyr)
require(dplyr)




# load data ---------------------------------------------------------------

dta <- read_excel(path = "geoseg_outputs/geoseg_outputs.xlsx", sheet = "tidy")


minority_vars <- c(
  "not_good", "nonhouse", "higher", "llti", "single", 
  "none", "nonhouse", "nonrlgs", "nonscot", "nonwhite",
  "pensinr" 
  )

propchange <- dta %>% 
  filter(variable %in% minority_vars) %>% 
  gather(key = index, value = value, -ttwa, -table, -year, -variable) %>% 
  spread(year, value) %>% 
  mutate(change = (`2011` - `2001`) / `2001`) %>% 
  select(ttwa, table, index, change)


# Let's create a separate tab for each table 


fn <- function(input){
  nm <- input$table[1]
  
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
d_ply(propchange, .(table), fn)
saveWorkbook(wb, file = "tables/change_in_segs.xlsx")







