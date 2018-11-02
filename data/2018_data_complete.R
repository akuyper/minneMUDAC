#### build full 2018 dataset

# load packages
library(tidyverse)

# read data
voters2018 <- read_rds("./data/processed/voter2018.rds") %>% 
  mutate(county_name = toupper(county_name))
county_dat <- read_rds("./data/processed/county_dat_2018.rds")
precincts <- read_csv("./data/processed/official_precincts.csv", col_names = TRUE) %>% 
  rename(
    county_name = `County Name`,
    countycode = `County Code`,
    precinct_code = `Precinct Code`,
    precinct_name = `Precinct Name`,
    congdist = Congressional
  ) %>% 
  mutate(county_name = toupper(county_name))

# join tables
dat_2018 <- voters2018 %>% 
  left_join(county_dat, by = c("countycode" = "COUNTYCODE", "county_name" = "county")) %>% 
  left_join(precincts) %>% 
  filter(precinct_name != "NULL")

# check for missingness
dat_2018 %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  t() # 15 congressional districts

dat_2018 <- dat_2018 %>% 
  mutate(lesue = if_else(countycode == 40 & str_detect(precinct_name, "^LE SUEUR"), 1, 0),
         warren = if_else(countycode == 45 & str_detect(precinct_name, "^WARREN P-1"), 1, 0),
         hibbing = if_else(countycode == 69 & str_detect(precinct_name, "^HIBBING"), 1, 0))

congdist <- dat_2018 %>% select(congdist) %>% as_vector()
lesue <- dat_2018 %>% select(lesue) %>% as_vector()
warren <- dat_2018 %>% select(warren) %>% as_vector()
hibbing <- dat_2018 %>% select(hibbing) %>% as_vector()

congdist_vec <- vector("double", length(congdist))
for(i in 1:length(congdist_vec)){
  if(lesue[i] == 1){
    congdist_vec[i] <- 1
  } else if(warren[i] == 1){
    congdist_vec[i] <- 7
  } else if(hibbing[i] == 1){
    congdist_vec[i] <- 8
  } else {
    congdist_vec[i] <- congdist[i]
  }
}

dat_2018 <- dat_2018 %>% 
  select(-lesue, -warren, -hibbing) %>% 
  mutate(congdist = congdist_vec)

dat_2018 %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  t

write_rds(dat_2018, "./data/processed/data_2018.rds")
