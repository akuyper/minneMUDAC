# Voter Supplement Data from IPUMS CPS, 2010 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2010 <- read_csv('data/unprocessed/ipumscps/cps_00018.csv')
ipums_data_2010 %>% skim()

ipums_data_2010 %>% 
  filter(!is.na(VOWHYNOT)) %>% 
  filter(VOWHYNOT!=99 & VOWHYNOT != 96) %>% 
  ggplot(aes(x=VOWHYNOT))+
  geom_bar()+
  labs(x="reasons",title="VOWHYNOT 2010")



