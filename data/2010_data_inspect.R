#### Quick Overview of 2010_general_results

# load packages
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)


# read in data
dat2010 <- read_xls("data/unprocessed/2010_general_results.xls") %>% 
  clean_names() %>%
  remove_empty("cols")

# skim data
dat2010 %>% 
  glimpse()

dat2010 %>%
  skim()
