# Produce new table for joining oa_2001 and oa_2011 to dz_2011 using 
# Paul Norman's table 


# Construct simplified dissimilarity scores by 2001 datazone for the following:

# 2/6/2016 - change from Jing link to Norman Link


rm(list = ls())

require(pacman)

pacman::p_load(
  readr, 
  tidyr, stringr, dplyr
)



# Linking tables ----------------------------------------------------------

norman_link <- read_delim("input_data/lookups/paul_norman_lookup_oa_to_dz2011.txt", delim = "\t")

# > norman_link
# Source: local data frame [53,367 x 7]
# 
# name01    label01  datazone                                      name   add add_source      gctwt
# (chr)      (chr)     (chr)                                     (chr) (int)      (int)      (dbl)
# 1  60QA000001 60QA000001 S01006608     Kincorth, Leggart and Nigg South - 01    33         34 0.97058824
# 2  60QA000001 60QA000001 S01006621                           Cove North - 03     1         34 0.02941176
# 3  60QA000002 60QA000002 S01006732                           Kingswells - 01    53         61 0.86885246
# 4  60QA000002 60QA000002 S01006736                           Kingswells - 05     1         61 0.01639344
# 5  60QA000002 60QA000002 S01006737                           Kingswells - 06     7         61 0.11475410
# 6  60QA000003 60QA000003 S01006710                         Sheddocksley - 03     1         33 0.03030303
# 7  60QA000003 60QA000003 S01006737                           Kingswells - 06    32         33 0.96969697
# 8  60QA000004 60QA000004 S01006516 Cults, Bieldside and Milltimber West - 04    24         25 0.96000000
# 9  60QA000004 60QA000004 S01006518 Cults, Bieldside and Milltimber East - 01     1         25 0.04000000
# 10 60QA000005 60QA000005 S01006518 Cults, Bieldside and Milltimber East - 01    23         24 0.95833333
# ..        ...        ...       ...                                       ...   ...        ...        ...


# name01 and label01 are identical, 

norman_link <- norman_link %>% select(oa_nrs = name01, dz_2011=datazone)


# nrs_to_oa

# source_DropboxData is now no longer supported. Instead I will have to put these inside the project

# Now moved locally 

nrs_oa_link <- read_csv("input_data/lookups/OUTPUT_AREA_2001_LOOKUP.csv")

# First few lines of the above table

# > nrs_oa_link
# Source: local data frame [42,604 x 2]
# 
# OutputArea2001Code NRSoldOutputArea2001Code
# 1           S00000001               60QA000001
# 2           S00000002               60QA000002
# 3           S00000003               60QA000003
# 4           S00000004               60QA000004
# 5           S00000005               60QA000005
# 6           S00000006               60QA000006
# 7           S00000007               60QA000007
# 8           S00000008               60QA000008
# 9           S00000009               60QA000009
# 10          S00000010               60QA000010
# ..                ...                      ...

# > length(unique(nrs_oa_link$OutputArea2001Code))
# [1] 42604
# > length(unique(nrs_oa_link$NRSoldOutputArea2001Code))
# [1] 42604

# It therefore seems that each nrs_oa code has maps 1:1 to standard codes, with 60QA in place of S01
# To check this: 

# tmp1 <- nrs_oa_link$NRSoldOutputArea2001Code %>% str_replace("60QA", "")
# tmp2 <- nrs_oa_link$OutputArea2001Code  %>% str_replace("^S00", "")
# tmp3 <- cbind(tmp1, tmp2)

# head(apply(tmp3, 1, function(x) x[1] == x[2]))
#[1] TRUE TRUE TRUE TRUE TRUE TRUE

# all(apply(tmp3, 1, function(x) x[1] == x[2]))
#[1] FALSE

# table(apply(tmp3, 1, function(x) x[1] == x[2]))
# FALSE  TRUE 
# 40743  1861 

# > head(tmp3[apply(tmp3, 1, function(x) x[1] == x[2]) ==FALSE,])
# tmp1         tmp2    
# [1,] "60QB000001" "001862"
# [2,] "60QB000002" "001863"
# [3,] "60QB000003" "001864"
# [4,] "60QB000004" "001865"
# [5,] "60QB000005" "001866"
# [6,] "60QB000006" "001867"

# So, the codes are almost but not quite the same apart from the prefixes, but they are 1:1 matches
#rm(tmp1, tmp2, tmp3)

nrs_oa_link <- nrs_oa_link %>% 
  rename(oa_2001 = OutputArea2001Code, oa_nrs = NRSoldOutputArea2001Code)


norm_link2 <- inner_join(nrs_oa_link, norman_link)


# Moved locally

big_link <- read_csv(
  "input_data/lookups/Census_2011_Lookup__OA_TO_HIGHER_AREAS.csv",
  col_types = paste(rep("c", 34), collapse = "")
)


oa_dz_link <- big_link %>% 
  select(
    oa_2001 = OutputArea2001Code,
    oa_2011 = OutputArea2011Code,
    dz_2001 = Datazone2001Code, 
    la_2011 = CouncilArea2011Code # this will allow links to specific cities later 
  )
rm(big_link)

# > oa_dz_link
# Source: local data frame [46,351 x 4]
# 
# oa_2001   oa_2011   dz_2001   la_2011
# 1  S00004507 S00093944 S01000675 S12000041
# 2  S00040457 S00132898 S01006149 S12000030
# 3  S00005737 S00095247 S01005406 S12000026
# 4  S00027843 S00119131 S01003962 S12000017
# 5  S00008211 S00097920 S01001005 S12000006
# 6  S00006223 S00095782 S01005441 S12000026
# 7  S00029197 S00120583 S01004183 S12000019
# 8  S00007822 S00097516 S01000915 S12000006
# 9  S00027793 S00119061 S01003870 S12000017
# 10 S00036870 S00128942 S01005501 S12000027
# ..       ...       ...       ...       ...

# Moved locally - dropbox location is 
# Data/local_authority_links/local_authority_districts_(UK)_2012_Names_and_Codes

la_2011_codes <- read_csv("input_data/lookups/LAD_2012_UK_NC.csv")


# > la_2011_codes
# Source: local data frame [406 x 3]
# 
# LAD12CD LAD12CDO                     LAD12NM
# 1  E06000001     00EB                  Hartlepool
# 2  E06000002     00EC               Middlesbrough
# 3  E06000003     00EE        Redcar and Cleveland
# 4  E06000004     00EF            Stockton-on-Tees
# 5  E06000005     00EH                  Darlington
# 6  E06000006     00ET                      Halton
# 7  E06000007     00EU                  Warrington
# 8  E06000008     00EX       Blackburn with Darwen
# 9  E06000009     00EY                   Blackpool
# 10 E06000010     00FA Kingston upon Hull, City of
# ..       ...      ...                         ...


la_2011_codes <- la_2011_codes %>% 
  select(
    la_2011 = LAD12CD, 
    la_name = LAD12NM
  )

# > la_2011_codes
# Source: local data frame [406 x 2]
# 
# la_2011                     la_name
# 1  E06000001                  Hartlepool
# 2  E06000002               Middlesbrough
# 3  E06000003        Redcar and Cleveland
# 4  E06000004            Stockton-on-Tees
# 5  E06000005                  Darlington
# 6  E06000006                      Halton
# 7  E06000007                  Warrington
# 8  E06000008       Blackburn with Darwen
# 9  E06000009                   Blackpool
# 10 E06000010 Kingston upon Hull, City of
# ..       ...                         ...

oa_dz_link <- oa_dz_link %>% inner_join(la_2011_codes) # inner_join so just Scotland


joined_table <- norm_link2  %>% inner_join(oa_dz_link)  %>% inner_join(la_2011_codes)

write_csv(joined_table, "input_data/lookups/paul_norman_dz2011_table.csv")