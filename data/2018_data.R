# creating 2018 data

# load packages
library(tidyverse)
library(janitor)
library(skimr)
library(randomForest)

# read data
elecDat <- read_rds("./data/processed/competition_data.rds")

county_dat1 <- read_csv("./data/processed/combined_counties/combined_counties.csv") %>% 
  distinct(year, countycode, .keep_all = TRUE) %>% 
  select(-snap, -poverty_perc, -net_mig, -prevent_admin, -real_wage_coladj, -patents, -crime, -civ_labor, -at_hpi, -pre_death, -age_pre_death, -date) %>% 
  filter(year >= 2010)

county_dat1 %>% 
  select(year) %>% 
  unique()

# extract county level numbers
county_dat <- elecDat %>% 
  distinct(year, countycode, .keep_all = TRUE) %>% 
  select(-pctname, -pctcode, -congdist, -mailballot, -reg7am, -signatures, -ab_mb, -totvoting, -county, -pres, -date)

countycodes <- county_dat %>% select(countycode) %>% unique() %>% as_vector()

vars <- names(county_dat1)[3:31]

weights <- c(.7, .25, .05)

county_dat %>% 
  select(vars[2], year, countycode) %>% 
  filter(countycode == countycodes[1]) %>% 
  spread(key = year, value = vars[2]) %>% 
  mutate(avg_inc = sum(c(`2016` - `2014`, `2014` - `2012`, `2012` - `2010`) * weights),
         `2018` = `2016` + avg_inc)

elecDat %>% 
  mutate(turnout = totvoting/(edr + reg7am)) %>% 
  select(turnout, county)
