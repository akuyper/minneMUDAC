# Voter Supplement Data form IPUMS CPS, 2018 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2018 <- read_csv('data/unprocessed/ipumscps/cps_00023.csv')
ipums_data_2018 %>% skim()

ipums_data_2018 %>% 
  filter(!is.na(VOWHYNOT)) %>% 
  filter(VOWHYNOT!=99 & VOWHYNOT != 96) %>% 
  ggplot(aes(x=VOWHYNOT))+
  geom_bar()+
  labs(x="reasons",title="VOWHYNOT 2018")

