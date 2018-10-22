# Voter Supplement Data from IPUMS CPS, 2011 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2011_2012 <- read_csv('data/unprocessed/ipumscps/cps_00021.csv')
ipums_data_2011_2012 %>% skim()
