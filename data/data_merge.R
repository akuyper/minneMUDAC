# Joining datasets

# load packages
library(tidyverse)
library(skimr)

# read data
results <- read_csv("./data/processed/2008_2016_general_result.csv")
county_acs <- read_csv("./data/processed/combined_counties/combined_counties.csv")
county_acs_aug <- read_csv("./data/processed/combined_counties_aug.csv")

# join and cleanse for even numbered years
comp_dat <- results %>% 
  left_join(county_acs, by = c("countycode", "year")) %>% 
  filter(!is.na(eq_sprime)) %>% 
  filter(year >= 2010) %>% 
  # remove missing 
  select(-snap, -poverty_perc, -net_mig, -prevent_admin, -real_wage_coladj, -patents, -crime, -civ_labor, -at_hpi, -pre_death, -age_pre_death)

# add 2016 st louis county data
comp_dat <- comp_dat %>% 
  mutate(unemplyoment = if_else(is.na(unemplyoment), 5.7, unemplyoment),
         pres = if_else(year %% 4 == 0, 1, 0)) %>% 
  rename(unemployment = unemplyoment)

comp_dat %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  t()

# join and cleanse for all years 2010-present
# odd number election results will be NA
county_all <- county_acs %>% 
  filter(year >= 2010, year < 2018) %>% 
  mutate(unemployment = if_else(is.na(unemployment) & year == 2016, 5.7, unemployment),
         unemployment = if_else(is.na(unemployment) & year == 2017, 4.6, unemployment),
         pres = if_else(year %% 4 == 0, 1, 0),
         cong = if_else(year %% 2 == 0, 1, 0)) %>% 
  rename(unemployment = unemployment)

county_all_aug <- county_acs_aug %>% 
  filter(year >= 2010, year < 2018) %>% 
  mutate(unemployment = if_else(is.na(unemployment) & year == 2016, 5.7, unemployment),
         unemployment = if_else(is.na(unemployment) & year == 2017, 4.6, unemployment),
         pres = if_else(year %% 4 == 0, 1, 0),
         cong = if_else(year %% 2 == 0, 1, 0)) %>% 
  rename(unemployment = unemployment)

comp_dat_full <- results %>%
  filter(year >= 2010) %>% 
  full_join(county_all, by = c("countycode", "year")) %>%
  filter(!is.na(eq_sprime)) %>%
  select(-snap, -poverty_perc, -net_mig, -prevent_admin, -real_wage_coladj, -patents, -crime, -civ_labor, -at_hpi, -pre_death, -age_pre_death)
  
county_all <- county_all %>% 
  select(-snap, -poverty_perc, -net_mig, -prevent_admin, -real_wage_coladj, -patents, -crime, -civ_labor, -at_hpi, -pre_death, -age_pre_death)

comp_dat_aug <- results %>% 
  left_join(county_acs_aug, by = c("countycode", "year")) %>% 
  filter(year >= 2010) %>% 
  # remove missing 
  select(-snap, -poverty_perc, -net_mig, -prevent_admin, -real_wage_coladj, -patents, -crime, -civ_labor, -at_hpi, -pre_death, -age_pre_death)

# add 2016 st louis county data
comp_dat_aug <- comp_dat_aug %>% 
  mutate(unemployment = if_else(is.na(unemployment), 5.7, unemployment),
         pres = if_else(year %% 4 == 0, 1, 0)) %>% 
  rename(unemployment = unemployment)

comp_dat_full_aug <- results %>%
  filter(year >= 2010) %>% 
  full_join(county_all_aug, by = c("countycode", "year")) %>%
  select(-snap, -poverty_perc, -net_mig, -prevent_admin, -real_wage_coladj, -patents, -crime, -civ_labor, -at_hpi, -pre_death, -age_pre_death)

# write data
write_rds(county_all, "./data/processed/all_county.rds")
write_rds(comp_dat, "./data/processed/competition_data.rds")
write_rds(comp_dat_full, "./data/processed/competition_data_oddyears.rds")
write_rds(comp_dat_aug, "./data/processed/competition_data_aug.rds")
write_rds(comp_dat_full_aug, "./data/processed/competition_data_full_aug.rds")

# check for completeness among variables
year_check <- function(var){
  comp_dat_full %>% 
    select(var, year) %>% 
    remove_missing() %>% 
    group_by(year) %>% 
    count() %>% 
    mutate(var = var)
}

vars <- names(comp_dat_full)

var_ranges <- bind_rows(map(vars, year_check)) %>% 
  group_by(var) %>% 
  summarise(min = min(year),
            max = max(year))

var_range_plot <- var_ranges %>% 
  ggplot(aes(y = factor(var), yend = factor(var))) +
  geom_segment(aes(x = factor(min), xend = factor(max))) +
  labs(
    x = "year",
    y = "feature",
    title = "Yearly Coverage by Feature"
  )

perc_missing <- function(var){
  comp_dat_full %>% 
    select(var, year) %>% 
    group_by(year) %>% 
    summarise(pna = sum(is.na(get(var)))/87) %>% 
    mutate(var = var)
}

var_p_na <- bind_rows(map(vars, perc_missing))

percent_missing_plot <- var_p_na %>% 
  filter(pna != 1) %>% 
  ggplot(aes(factor(year), factor(var), size = pna*100)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "year",
    y = "feature",
    size = "percent missing",
    title = "Percentage of Missing Values"
  )

comp_dat_full %>% 
  filter(year == 2010, is.na(unemployment))
