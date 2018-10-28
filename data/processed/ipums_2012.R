# Voter Supplement Data form IPUMS CPS, 2012 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2012 <- read_csv('data/unprocessed/ipumscps/cps_00020.csv')
ipums_data_2012 %>% skim()

ipums_data_2012 %>% 
  filter(!is.na(VOWHYNOT)) %>% 
  filter(VOWHYNOT!=99 & VOWHYNOT != 96) %>% 
  ggplot(aes(x=VOWHYNOT))+
  geom_bar()+
  labs(x="reasons",title="VOWHYNOT 2012")

