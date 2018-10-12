#### Quick Overview of 2014_general_results.xlsx

# Load packages
library(tidyverse)
library(skimr)
library(janitor)
library(readxl)

# read in data
dat2014 <- read_xlsx("data/unprocessed/2014_general_results.xlsx") %>% 
  clean_names() %>% 
  remove_empty_cols()

#skim the data
dat2014 %>% 
  glimpse()
