# Voter Supplement Data form IPUMS CPS, 2014 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2014 <- read_csv('data/unprocessed/ipumscps/cps_00024.csv')
ipums_data_2014 %>% skim()

ipums_data_2014 %>% 
  filter(!is.na(VOWHYNOT)) %>% 
  filter(VOWHYNOT!=99 & VOWHYNOT != 96) %>% 
  ggplot(aes(x=VOWHYNOT))+
  geom_bar()+
  labs(x="reasons",title="VOWHYNOT 2014")

