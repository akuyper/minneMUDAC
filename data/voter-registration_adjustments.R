library(tidyverse)
library(janitor)
library(readxl)

may_nov_data <- read_xlsx("data/unprocessed/minnesota-voter-registration-by-county-since-2000.xlsx")%>% 
  clean_names() 

colnames(may_nov_data) <- c("county","may10","nov10","may12","nov12","may14","nov14","may16","nov16","may18")

may_nov_data<-may_nov_data %>% 
   mutate(adjustment10=nov10/may10,adjust12=nov12/may12,adjust14=nov14/may14,adjust16=nov16/may16) %>% 
   mutate(county=tolower(county))

#weighted adjustments
may_nov_data <- may_nov_data %>% 
  mutate(adjustment18=1/3*adjustment10+1/6*adjust12+1/3*adjust14+1/6*adjust16)

countyid <- read.csv("data/processed/countycodes.csv") %>% 
  clean_names() %>% 
  mutate(county=tolower(countyname))

adjustments <- left_join(may_nov_data,countyid,by="county") %>% 
  select(countycode,county,adjustment18)

adjustments<-adjustments[-nrow(adjustments),]

#load precinct 2018may
precinct2018 <- read_xlsx("data/unprocessed/may-1-2018-registered-voter-counts-by-precinct-split.xlsx") %>% 
  clean_names() %>% 
  group_by(county_id,county_name,precinct_code,precinct_name) %>% 
  summarise(registered_voters=sum(registered_voters))

colnames(precinct2018)[1]="countycode"

voter2018<- left_join(precinct2018,adjustments,by="countycode") %>% 
  select(countycode,county_name,precinct_code,precinct_name,registered_voters,adjustment18) %>% 
  mutate(estimated_registered_voters=registered_voters*adjustment18) %>% 
  select(-registered_voters,-adjustment18) %>% 
  add_column(year=2018)

voter2018 <- voter2018[-nrow(voter2018),]

saveRDS(voter2018, file = "data/processed/voter2018.rds")

