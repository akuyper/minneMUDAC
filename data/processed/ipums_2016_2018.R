# Voter Supplement Data from IPUMS CPS, 2016-2018 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

ipums_data_2016_2018 <- read_csv('data/unprocessed/ipumscps/cps_00013.csv')
ipums_data_2016_2018 %>% skim()
