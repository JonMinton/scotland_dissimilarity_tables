# Script for exploring the proportions of people in minority categories by travel to work areas 



# load prereqs ------------------------------------------------------------


rm(list = ls())

require(stringr)
require(readr)

require(plyr)
require(dplyr)
require(tidyr)
require(car)


require(ggplot)


# data loading ------------------------------------------------------------



# Load ttwa lookups

ttwa <- read_csv(file = "input_data/lookups/LSOA01_TTWA01_UK_LU.csv", col_types = "ccccccc")


# For each file in 
# output_data/dz_2001/binary

# load the file, 
# attach to ttwa
# save 

dir("output_data/dz_2001/binary")
infile <- read_csv(paste0("output_data/dz_2001/binary/", x$attributes))
