####Overview of  all general results

# Load packages
library(tidyverse)
library(skimr)
library(janitor)
library(readxl)

dat2010 <- read_xls("data/unprocessed/2010_general_results.xls") %>% 
  clean_names() %>% 
  remove_empty("cols") %>%  
  add_column(year=2010) %>% 
  add_column(dem=ifelse(dat2010$congdfl > dat2010$congr, 1, 0)) %>% 
  select(year,precinct_name,precinct_code,county_id,cg,mail_ballot,x7am,edr,signatures,reg_mil_ab,tot_voters) 
names(dat2010) <- names(dat2016)  

dat2016 <- read_xlsx("data/unprocessed/2016_general_results.xlsx") %>% 
  clean_names() %>% 
  remove_empty_cols() %>%
  add_column(year=2016) %>% 
  select(year,pctname,pctcode,countycode,congdist,mailballot,reg7am,edr,signatures,ab_mb,totvoting)

dat2014 <- read_xlsx("data/unprocessed/2014_general_results.xlsx") %>% 
  clean_names() %>% 
  remove_empty_cols() %>%  
  add_column(year=2014) %>% 
  select(year,pctname,pctcode,countycode,congdist,mailballot,reg7am,edr,signatures,ab_mb,totvoting)
names(dat2014) <- names(dat2016)  

dat2012 <- read_xlsx("data/unprocessed/2012_general_results.xlsx") %>% 
  clean_names() %>% 
  remove_empty_cols() %>% 
  add_column(year=2012) %>% 
  select(year,pctname,pctcode,countycode,congdist,mailballot,x7am,edr,signatures,regmilovab,totvoting)
names(dat2012)<-names(dat2016)





dat20082016 <-rbind(dat2008,dat2010,dat2012,dat2014,dat2016)

dat20002016$mailballot<-ifelse(dat20082016$year>=2010 & is.na(dat20082016$mailballot),"NO",dat20082016$mailballot)

write_csv(dat20082016,"data/processed/2008_2016_general_result.csv",na="NA")

