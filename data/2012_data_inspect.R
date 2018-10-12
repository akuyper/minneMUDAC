#### Quick Overview of 2012_general_results.xlsx
#### Ziyi Lu

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)

# Read in Data
dat2012 <- read_xlsx("data/unprocessed/2012_general_results.xlsx") %>% 
  clean_names() %>% 
  janitor::remove_empty_cols()

# Skim Data
dat2012 %>% 
  glimpse() # var name and type

dat2012 %>% 
  skim() # missing data and distributions

