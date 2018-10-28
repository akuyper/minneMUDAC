# Voter Supplement Data form IPUMS CPS, 2016 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2016 <- read_csv('data/unprocessed/ipumscps/cps_00022.csv')
ipums_data_2016 %>% skim()

ipums_data_2016 %>% 
  filter(!is.na(VOWHYNOT)) %>% 
  filter(VOWHYNOT!=99 & VOWHYNOT != 96) %>% 
  ggplot(aes(x=VOWHYNOT))+
  geom_bar()+
  labs(x="reasons",title="VOWHYNOT 2016")

