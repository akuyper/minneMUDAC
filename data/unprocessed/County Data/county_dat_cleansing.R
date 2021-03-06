#### County Data Cleaning

# load packages
library(tidyverse)
library(readxl)
library(lubridate)

## read in data
files <- dir(
  "./data/unprocessed/County Data/", ".csv$", full.names = TRUE)

# 2016 results, to be used for county id
county_ids <- read_xlsx(
  "./data/unprocessed/2016_general_results.xlsx") %>%
  select(COUNTYNAME, COUNTYCODE) %>%
  mutate(COUNTYNAME = toupper(COUNTYNAME)) %>% 
  unique()

write_csv(county_ids, "./data/processed/countycodes.csv")

# county data
county_dat <- map(files, read_csv)

# data descriptions
description_dat <- read_csv(
  "./data/unprocessed/MN_potential_data_list.csv"
  ) %>%
  filter(str_detect(Title, "County"))


## remove county qualifier from Title
short_title <- vector("character",
                      dim(description_dat)[1])

Title <- description_dat %>% select(Title) %>% as_vector()

for(i in 1:dim(description_dat)[1]){
  short_title[i] <- str_sub(
    Title[i], 1L, str_locate(Title[i], " in | for ")[1] - 1)
}

description_dat <- description_dat %>%
  mutate(Title = short_title)


## rename features to short title
Title <- description_dat %>% select(Title) %>% unique() %>% as_vector()

# extract variable ID to match with
identifiers <- c(
  "URN$|^LAUCN", "SPHOUSE", "MHIMN", "^ATNHP", "^S1701", "^CBR27",
  "^B01002001E027", "^CDC20N2UAA027", "^B0300200", "^PPAAMN", "^PUAAMN",
  "^NETMIGNA", "^2020RATIO", "^PCPI", "POP$", "DCYAC", "^HOWNRATE",
  "MWACL", "^B080AC", "^B03002006E027", "^B03002012E027", 
  "^B03002004E027", "^B03002005E027", "^EQFXSUBPRIME", "HC01ESTVC", 
  "^CDC20N2U027", "^DP04ACS", "^BPPRIV", "^USPTOISSUED", "^FBITC027",
  "^PEAAMN", "FN$", "^DMPCRATE027", "^ENU27", "^PEU18MN27", "^PI",
  "^RACEDISPARITY", "^S1501ACSTOTAL", "^PPU18MN27", "^HC01ESTVC",
  "^PE5T17MN27", "^PUA0T17MN", "^PP5T17MN27"
)

# corresponds to index in Title
identifier_id <- c(1:37, 40:42, 44:45, 47)

# renames features in county datasets, combines into list
county_dat_renamed <- vector("list", 87)
for(i in 1:length(county_dat)){
  dat <- county_dat[i][[1]]
  vars <- names(dat)
  # if identifying pattern matches with variable name, renames it to title
  for(j in 1:length(vars)){
    for(k in 1:length(identifiers)){
      if(str_detect(vars[j], identifiers[k])){
        vars[j] <- Title[identifier_id[k]]
      }
      # marks CI bound data as bad data
      if(str_detect(vars[j], "CI[L|U]B")) {
        vars[j] <- "BAD"
      }
    }
  }
  names(dat) <- vars
  county_dat_renamed[i][[1]] <- dat
}

# combine renamed county data into one tibble
# add county id column
# summarise by yearly average per county 

combined_counties <- bind_rows(county_dat_renamed) %>%
  select(contains(" "), DATE, county) %>%
  mutate(year = year(DATE)) %>% 
  group_by(year, county) %>% 
  summarise_all(.funs = funs(mean(., na.rm = TRUE))) %>%
  left_join(county_ids, by = c("county" = "COUNTYNAME"))

# use august values
unemployment <- bind_rows(county_dat_renamed) %>%
  select(contains(" "), DATE, county) %>%
  mutate(year = year(DATE),
         month = month(DATE)) %>% 
  select(`Unemployment Rate`, month, year, county) %>% 
  filter(month == 8) %>% select(-month)

civ_labor <- bind_rows(county_dat_renamed) %>%
  select(contains(" "), DATE, county) %>%
  mutate(year = year(DATE),
         month = month(DATE)) %>% 
  select(`Civilian Labor Force`, month, year, county) %>% 
  filter(month == 8) %>% select(-month)

combined_counties_aug <- bind_rows(county_dat_renamed) %>%
  select(contains(" "), DATE, county) %>%
  mutate(year = year(DATE)) %>% 
  group_by(year, county) %>% 
  summarise_all(.funs = funs(mean(., na.rm = TRUE))) %>%
  left_join(county_ids, by = c("county" = "COUNTYNAME")) %>% 
  select(-`Unemployment Rate`, -`Civilian Labor Force`) %>% 
  left_join(unemployment, by = c("county", "year")) %>% 
  left_join(civ_labor, by = c("county", "year"))

var_descs <- names(combined_counties)

# renames features to shortened description
var_names <- c(
  "year", "county", "undergrad", "unemployment", "income_percap",
  "civ_labor", "race_white", "income_med", "income_person", "pop_res",
  "net_mig", "homeowner", "crime", "racical_dissim", "snap", "inc_ineq",
  "discon_yth", "burd_house", "pre_death", "patents", "poverty", "age_med",
  "commute", "age_pre_death", "at_hpi", "eq_sprime", "poverty_perc",
  "hisp_latin", "privat_ests", "buildings", "poverty_u18", "house_singp",
  "pop_est", "pop_perc", "prevent_admin", "assoc_deg", "rel_chil_518",
  "prec_rel_chil_518", "pop_u18", "perc_pop_u18", "real_wage_coladj",
  "date", "countycode"
)

var_names_aug <- c(
  "year", "county", "undergrad", "income_percap",
  "race_white", "income_med", "income_person", "pop_res",
  "net_mig", "homeowner", "crime", "racical_dissim", "snap", "inc_ineq",
  "discon_yth", "burd_house", "pre_death", "patents", "poverty", "age_med",
  "commute", "age_pre_death", "at_hpi", "eq_sprime", "poverty_perc",
  "hisp_latin", "privat_ests", "buildings", "poverty_u18", "house_singp",
  "pop_est", "pop_perc", "prevent_admin", "assoc_deg", "rel_chil_518",
  "prec_rel_chil_518", "pop_u18", "perc_pop_u18", "real_wage_coladj",
  "date", "countycode", "unemployment", "civ_labor"
)

names(combined_counties) <- var_names
names(combined_counties_aug) <- var_names

## create variable description table
varID <- description_dat %>%
  filter(!str_detect(ID, "CI[L|U]B")) %>% 
  distinct(Title, .keep_all = TRUE) %>%
  select(ID) %>%
  as_vector()

variable_descriptions <- tibble(
  name = var_names,
  description = var_descs,
  varID_guess = varID
)


## write files to csv
write_csv(combined_counties, "./data/processed/combined_counties.csv")
write_csv(combined_counties_aug, "./data/processed/combined_counties_aug.csv")
write_csv(variable_descriptions, "./data/processed/combined_counties_descriptions.csv")
