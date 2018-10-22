# Voter Supplement Data from IPUMS CPS, 2010 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2010 <- read_csv('data/unprocessed/ipumscps/cps_00018.csv')
ipums_data_2010 %>% skim()
