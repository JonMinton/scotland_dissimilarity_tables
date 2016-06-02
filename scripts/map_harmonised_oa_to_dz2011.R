
rm(list = ls())

require(pacman)

pacman::p_load(
  readr, 
  plyr, # using this to avoid writing purrr style
  tidyr, stringr, dplyr
)






# Part 3: mapping from OA to dz_2011 --------------------------------------

# A slightly different process is needed for 2001 oas than for 2011 oas

# 2001 files 
oa_2001 <- dir("output_data/oa_harmonised/", pattern = "_2001.csv$|_2001_2cat.csv$")
oa_2011 <- dir("output_data/oa_harmonised/", pattern = "_2011.csv$|_2011_2cat.csv$")

# Paul Norman's link file

final_link <- read_csv("input_data/lookups/paul_norman_dz2011_table.csv")

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

l_ply(oa_2001, fn, .progress = "text")


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

l_ply(oa_2011, fn, .progress = "text")


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

l_ply(oa_2001, fn, .progress = "text")


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

l_ply(oa_2011, fn, .progress = "text")



