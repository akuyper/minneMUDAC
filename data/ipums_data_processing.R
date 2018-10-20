#### Quick Overview of IPUMS data

# load packages 
library(tidyverse)
library(janitor)
library(skimr)
# library(ipumsr) # use for fixed width file (.dat)

# Following for fixed-width format (.dat)
# # Read data
# ddi_test_00001 <- read_ipums_ddi(ddi_file = "data/unprocessed/ipumscps/cps_00001.xml")
# data_test_00001 <- read_ipums_micro(ddi = ddi_test_00001)
# 
# data_test_00001 %>% skim()

# For CSV:
test <- read_csv('data/unprocessed/ipumscps/cps_00012.csv')

