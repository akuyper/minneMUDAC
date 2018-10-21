# Voter Supplement Data from IPUMS CPS, 2013-2015 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2013_2015 <- read_csv('data/unprocessed/ipumscps/cps_00014.csv')
ipums_data_2013_2015 %>% skim()
