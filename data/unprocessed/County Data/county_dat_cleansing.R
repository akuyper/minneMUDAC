#### County Data Cleaning

# load packages
library(tidyverse)
library(readxl)
library(lubridate)

## read in data
files <- dir("./data/unprocessed/County Data/", ".csv$", full.names = TRUE)

# 2016 results, to be used for county id
county_ids <- read_xlsx("./data/unprocessed/2016_general_results.xlsx") %>%
  select(COUNTYNAME, COUNTYCODE) %>%
  mutate(COUNTYNAME = toupper(COUNTYNAME)) %>% 
  unique()

# county data
county_dat <- map(files, read_csv)

# data descriptions
description_dat <- read_csv("./data/unprocessed/MN_potential_data_list.csv") %>%
  filter(str_detect(Title, "County"))

# remove county qualifier from Title
short_title <- vector("character", dim(description_dat)[1])
Title <- description_dat %>% select(Title) %>% as_vector()
for(i in 1:dim(description_dat)[1]){
  short_title[i] <- str_sub(Title[i], 1L, str_locate(Title[i], " in | for ")[1] - 1)
}

description_dat <- description_dat %>%
  mutate(Title = short_title)


## rename features to short title
Title <- description_dat %>% select(Title) %>% unique() %>% as_vector()
identifiers <- c(
    "URN$|^LAUCN",
    "SPHOUSE",
    "MHIMN",
    "^ATNHP",
    "^S1701",
    "^CBR27",
    "^B01002001E027",
    "^CDC20N2UAA027",
    "^B0300200",
    "^PPAAMN",
    "^PUAAMN",
    "^NETMIGNA",
    "^2020RATIO",
    "^PCPI",
    "POP$",
    "DCYAC",
    "^HOWNRATE",
    "MWACL",
    "^B080AC",
    "^B03002006E027",
    "^B03002012E027",
    "^B03002004E027",
    "^B03002005E027",
    "^EQFXSUBPRIME",
    "HC01ESTVC",
    "^CDC20N2U027",
    "^DP04ACS",
    "^BPPRIV",
    "^USPTOISSUED",
    "^FBITC027",
    "^PEAAMN",
    "FN$",
    "^DMPCRATE027",
    "^ENU27",
    "^PEU18MN27",
    "^PI",
    "^RACEDISPARITY",
    "^S1501ACSTOTAL",
    "^PPU18MN27",
    "^HC01ESTVC",
    "^PE5T17MN27",
    "^PUA0T17MN",
    "^PP5T17MN27"
)
identifier_id <- c(1:37, 40:42, 44:45, 47)

county_dat_renamed <- vector("list", 87)
for(i in 1:length(county_dat)){
  dat <- county_dat[i][[1]]
  vars <- names(dat)
  
  for(j in 1:length(vars)){
    for(k in 1:length(identifiers)){
      
      if(str_detect(vars[j], identifiers[k])){
        vars[j] <- Title[identifier_id[k]]
      } 
      if(str_detect(vars[j], "CI[L|U]B")) {
        vars[j] <- "BAD"
      }
    }
  }
  names(dat) <- vars
  county_dat_renamed[i][[1]] <- dat
}



# combine renamed county data into one tibble, add county id column, summarise by yearly average per county
combined_counties <- bind_rows(county_dat_renamed) %>%
  select(contains(" "), DATE, county) %>%
  mutate(Year = year(DATE)) %>% 
  group_by(Year, county) %>% 
  summarise_all(.funs = funs(mean(., na.rm = TRUE))) %>%
  left_join(county_ids, by = c("county" = "COUNTYNAME"))

write_csv(combined_counties, "./data/processed/combined_counties.csv")
