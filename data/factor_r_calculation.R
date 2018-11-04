library(tidyverse)

competition_data <- readRDS("data/processed/competition_data.rds")

colnames(competition_data)

competition_data <- 
  competition_data %>%
  filter(reg7am != 0) %>% 
  mutate(r_factor=(totvoting-edr)/reg7am)

write.csv(competition_data, file = "data/processed/competition_data_with_r.csv")
write_rds(competition_data, "./data/processed/competition_data_with_r.rds")

only_r <- competition_data %>% 
  select(year,pctcode,congdist,r_factor)

only_r_2016 <- only_r %>% 
  filter(year==2016) %>% 
  mutate(r_2016=r_factor) %>% 
  select(-r_factor,-year)
  
only_r_2014 <- only_r %>% 
  filter(year==2014) %>% 
  mutate(r_2014=r_factor) %>% 
  select(-r_factor,-congdist,-year) %>% 
  left_join(only_r_2016,by="pctcode")
  
only_r_2012 <- only_r %>% 
  filter(year==2012) %>% 
  mutate(r_2012=r_factor) %>% 
  select(-r_factor,-congdist,-year) %>% 
  left_join(only_r_2014,by="pctcode")

only_r_2010 <- only_r %>% 
  filter(year==2010) %>% 
  mutate(r_2010=r_factor) %>% 
  select(-r_factor,-congdist,-year) %>% 
  left_join(only_r_2012,by="pctcode")

write.csv(only_r_2010, file = "data/processed/only_r.csv")
