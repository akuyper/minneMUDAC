#### Quick Overview of 2016_general_results.xlsx

# load packages 
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)

# read in data
# dat2016 is where you are storing the data
dat2016 <- read_xlsx("data/unprocessed/2016_general_results.xlsx") %>%
  # pipe operator(shift + command + m) - says we're going to do the next action
  clean_names() %>% 
  remove_empty_cols()


# skim/summarize the data
dat2016 %>% 
  # glimpse tells you what types of variables are in the data
  glimpse()


dat2016 %>% 
  # skim gives more details
  skim()
