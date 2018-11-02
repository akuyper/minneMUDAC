#### Overview of  all general results with election outcome
#### dem == 1 if Democratic Party won

# Load packages
library(tidyverse)
library(skimr)
library(janitor)
library(readxl)

dat2010 <- read_xls("data/unprocessed/2010_general_results.xls") %>% 
  clean_names() %>% 
  remove_empty("cols") %>%  
  add_column(year=2010)
dat2010 <- dat2010 %>% 
  add_column(dem=ifelse(dat2010$congdfl > dat2010$congr, 1, 0)) %>% 
  select(dem, year,precinct_name,precinct_code,county_id,cg,mail_ballot,x7am,edr,signatures,reg_mil_ab,tot_voters) 

dat2012 <- read_xlsx("data/unprocessed/2012_general_results.xlsx") %>% 
  clean_names() %>% 
  remove_empty_cols() %>% 
  add_column(year=2012)
dat2012 <- dat2012 %>% 
  add_column(dem=ifelse(dat2012$usprsdfl > dat2012$usprsr, 1, 0)) %>% 
  select(dem,year,pctname,pctcode,countycode,congdist,mailballot,x7am,edr,signatures,regmilovab,totvoting)

dat2014 <- read_xlsx("data/unprocessed/2014_general_results.xlsx") %>% 
  clean_names() %>% 
  remove_empty_cols() %>%  
  add_column(year=2014) 
dat2014 <- dat2014 %>% 
  add_column(dem=ifelse(dat2014$ussendfl > dat2014$ussenr, 1, 0)) %>% 
  select(dem,year,pctname,pctcode,countycode,congdist,mailballot,reg7am,edr,signatures,ab_mb,totvoting)

dat2016 <- read_xlsx("data/unprocessed/2016_general_results.xlsx") %>% 
  clean_names() %>% 
  remove_empty_cols() %>%
  add_column(year=2016) 
dat2016 <- dat2016 %>%
  add_column(dem=ifelse(dat2016$usprsdfl > dat2016$usprsr, 1, 0)) %>% 
  select(dem, year,pctname,pctcode,countycode,congdist,mailballot,reg7am,edr,signatures,ab_mb,totvoting)

# rename
names(dat2014) <- names(dat2016)  
names(dat2012)<- names(dat2016)
names(dat2010) <- names(dat2016)

# combine
dat20102016 <-rbind(dat2010,dat2012,dat2014,dat2016)
dat20102016$mailballot<-ifelse(is.na(dat20102016$mailballot),0,1)

saveRDS(dat20102016,"data/processed/all_data_inspect_withresult.rds")

