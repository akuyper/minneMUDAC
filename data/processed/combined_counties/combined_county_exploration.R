#### Combined County Data Exploration

# load packages
library(tidyverse)
library(skimr)

# read data
files <- dir("./data/processed/", ".csv$", full.names = TRUE)

county_dat <- read_csv(files[1])
county_descrip <- read_csv(files[2])

# identify year range of features
year_check <- function(var){
  county_dat %>% 
    select(var, year) %>% 
    remove_missing() %>% 
    group_by(year) %>% 
    count() %>% 
    mutate(var = var)
}

vars <- names(county_dat)[3:41]

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
  county_dat %>% 
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

write_rds(var_range_plot, "./data/processed/combined_counties/plots/variable_coverage.rds")
write_rds(percent_missing_plot, "./data/processed/combined_counties/plots/percent_missing.rds")