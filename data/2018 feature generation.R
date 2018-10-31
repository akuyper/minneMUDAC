#### 2018 feature creation

# load packages
library(tidyverse)
library(readxl)

# read data
path <- "./data/processed/all_county.rds"
county_dat <- read_rds(path)

results <- read_csv("./data/processed/2008_2016_general_result.csv") %>% 
  filter(year %% 2 == 0)

county_ids <- read_xlsx(
  "./data/unprocessed/2016_general_results.xlsx")

county_ids <- county_ids %>% 
  select(COUNTYNAME, COUNTYCODE) %>%
  mutate(COUNTYNAME = toupper(COUNTYNAME)) %>% 
  unique()

# returns year range of data
county_dat %>% 
  select(year) %>% 
  unique()

# returns missingness per year per variable
county_dat %>% 
  group_by(year) %>% 
  summarise_all(.funs = funs(sum(is.na(.)))) %>% 
  summarise_all(.funs = funs(sum(.))) %>% 
  select(-year) %>% 
  filter_all(.vars_predicate = any_vars(. != 0))

# view missingness excluding 2017
county_dat %>% 
  filter(year != 2017) %>% 
  group_by(year) %>%
  summarise_all(.funs = funs(sum(is.na(.)))) %>% 
  summarise_all(.funs = funs(sum(.))) %>% 
  select(-year) %>% 
  filter_all(.vars_predicate = any_vars(. != 0))

# remove 2017 to avoid missing values
county_dat <- county_dat %>% filter(year != 2017) %>% select(-date, -countycode, -pres, -cong)

# gather features
county_dat <- county_dat %>% 
  gather(-year, -county, key = "feature", value = "value")

# spread over year
county_dat <- county_dat %>% 
  spread(year, value) %>% 
  `colnames<-`(c("county", "feature", "y10", "y11", "y12", "y13", "y14", "y15", "y16"))

# create weights
weights <- c(1/1.96875, .5/1.96875, .25/1.96875, .125/1.96875, .0625/1.96875, .03125/1.96875)

# generate 2018 values
county_dat <- county_dat %>%
  group_by(county, feature) %>% 
  mutate(y18_increase = sum(c(y16-y15, y15-y14, y14-y13, y13-y12, y12-y11, y11-y10)*weights),
         y18 = y16 + y18_increase)

# gather back to original form
county_dat <- county_dat %>% 
  select(-y18_increase) %>% 
  `colnames<-`(c("county", "feature", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2018")) %>% 
  gather(starts_with("201"), key = "year", value = "value") %>% 
  spread(feature, value)

# check for smoothness of 2018 values
county_dat %>% 
  gather(-county, -year, key = "feature", value = "value") %>%
  mutate(countyfeature = paste0(county, feature, "")) %>% 
  ggplot(aes(factor(year), value, group = countyfeature, color = countyfeature)) +
  geom_path(stat = "identity") +
  theme(legend.position = "none")

# join to results data
county_dat_2018 <- county_dat %>% 
  left_join(county_ids, by = c("county" = "COUNTYNAME")) %>% 
  mutate(year = as.integer(year)) %>% 
  filter(year == 2018)

# write county dataset with 2018 features to rds file
write_rds(county_dat_2018, "./data/processed/county_dat_2018.rds")
