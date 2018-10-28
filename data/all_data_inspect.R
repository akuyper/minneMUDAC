#### Quick Overview of  all general results

# Load packages
library(tidyverse)
library(skimr)
library(janitor)
library(readxl)

# read in data
# dat_1416 <- NULL
# years_1416 <- seq(2014,2016,2)
# for (year in years_1416) {
#   dat <- read_xlsx(paste0("data/unprocessed/",year,"_general_results.xlsx") ) %>% 
#   clean_names() %>% 
#   remove_empty_cols() %>%
#   select(pctname,prct,mcd_name,county_id,cg,registered_at_7am,edr,signature,reg_mil_ab,tot_voters) %>% 
#   add_column(year=year)
#   dat_1416 <- rbind(dat_1416,dat_1416)
# }

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

dat2010 <- read_xls("data/unprocessed/2010_general_results.xls") %>% 
  clean_names() %>% 
  remove_empty("cols") %>%  
  add_column(year=2010) %>% 
  select(year,precinct_name,precinct_code,county_id,cg,mail_ballot,x7am,edr,signatures,reg_mil_ab,tot_voters) 
names(dat2010) <- names(dat2016)  

dat2008 <- read_xls("data/unprocessed/2008_general_results.xls") %>% 
  clean_names() %>% 
  remove_empty("cols") %>%  
  add_column(year=2008) %>% 
  add_column(mail_ballot=NA) %>% 
  select(year,precinct_name,prct,county_id,cg,mail_ballot,x7am,edr,signatures,reg_mil_ab,tot_voters) 
names(dat2008) <- names(dat2016) 

# dat2006 <- read_xls("data/unprocessed/2006_general_results.xls") %>% 
#   clean_names() %>% 
#   remove_empty("cols") %>%  
#   add_column(year=2006) %>% 
#   add_column(mail_ballot=NA) %>% 
#   select(year,precinct_name,prct,county_id,cg,mail_ballot,registered_at_7am,edr,signature,reg_mil_ab,tot_voters) 
# names(dat2006) <- names(dat2016) 
# 
# dat2004 <- read_xls("data/unprocessed/2004_general_results.xls") %>% 
#   clean_names() %>% 
#   remove_empty("cols") %>% 
#   add_column(year=2004) %>% 
#   add_column(mail_ballot=NA) %>% 
#   select(year,precinct_name,prct,cc,cg,mail_ballot,x7am,edr,sigs,reg_mil_ab,tot_voters) 
# names(dat2004) <- names(dat2016) 
# 
# dat2002 <- read_xls("data/unprocessed/2002_general_results.xls") %>% 
#   clean_names() %>% 
#   remove_empty("cols") %>% 
#   add_column(year=2002) %>% 
#   add_column(mail_ballot=NA) %>% 
#   select(year,precinct_name,prct,cc,cg,mail_ballot,x7am,edr,signatures,ab_reg,ballots) 
# names(dat2002) <- names(dat2016) 
# 
# dat2000 <- read_xls("data/unprocessed/2000_general_results.xls") %>% 
#   clean_names() %>% 
#   remove_empty("cols") %>% 
#   add_column(year=2000) %>% 
#   add_column(mail_ballot=NA) %>% 
#   select(year,precinct_name,prct,cc,cg,mail_ballot,x7am,new,sign,reg_a,totl) 
# names(dat2000) <- names(dat2016) 

dat20082016 <-rbind(dat2008,dat2010,dat2012,dat2014,dat2016)

dat20002016$mailballot<-ifelse(dat20082016$year>=2010 & is.na(dat20082016$mailballot),"NO",dat20082016$mailballot)
  
write_csv(dat20082016,"data/processed/2008_2016_general_result.csv",na="NA")

# dat1998 <- read_xls("data/unprocessed/1998_general_results.xls") %>% 
#   clean_names() %>% 
#   remove_empty("cols") %>% 
#   add_column(year=1998) %>% 
#   add_column(mail_ballot=NA, cg =NA, signatures = NA, reg_ab=NA) %>% 
#   select(year,precinct_name,prct,fips,cg,mail_ballot,x7am,edr,signatures,reg_ab,ballots) 
# 
# dat1996 <- read_xls("data/unprocessed/1996_general_results.xls") %>% 
#   clean_names() %>% 
#   remove_empty("cols") %>% 
#   add_column(year=1996) %>% 
#   add_column(mail_ballot=NA, cg =NA) %>% 
#   select(precinct_name,prct,fips,cg,mail_ballot,x7am,edr,signatures,ab_reg,ballots,year) 
# 
# dat1994 <- read_xls("data/unprocessed/1994_general_results.xls") %>% 
#   clean_names() %>% 
#   remove_empty("cols") %>% 
#   add_column(year=1994) %>% 
#   add_column(mail_ballot=NA) %>% 
#   select(precinct_name,prct,cc,cg,mail_ballot,x7am,edr,signatures,ab_reg,ballots,year) 
# 
# dat1992 <- read_xls("data/unprocessed/1992_general_results.xls") %>% 
#   clean_names() %>% 
#   remove_empty("cols") %>% 
#   add_column(year=1992) %>% 
#   add_column(mail_ballot=NA, cg=NA) %>% 
#   select(precinct_name,prct,cc,mail_ballot,x7am,edr,signatures,ab_reg,ballots,year) 

