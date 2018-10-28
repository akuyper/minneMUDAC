# Joining datasets

# load packages
library(tidyverse)

results <- read_csv("./data/processed/2008_2016_general_result.csv")
county_acs <- read_csv("./data/processed/combined_counties/combined_counties.csv")

files <- dir("./data/unprocessed/Census Data/", ".csv$", full.names = TRUE)[1:8]

comp_dat <- results %>% 
  left_join(county_acs, by = c("countycode", "year"))

write_rds(comp_dat, "./data/processed/competition_data.rds")


