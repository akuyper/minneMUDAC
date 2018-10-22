# Voter Supplement Data form IPUMS CPS, 2012 (monthly)

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)

# Read Data
ipums_data_2012 <- read_csv('data/unprocessed/ipumscps/cps_00020.csv')
ipums_data_2012 %>% skim()
