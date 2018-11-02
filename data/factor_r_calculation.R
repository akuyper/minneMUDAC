library(tidyverse)

competition_data <- readRDS("data/processed/competition_data.rds")

colnames(competition_data)

competition_data <- 
  competition_data %>%
  filter(reg7am != 0) %>% 
  mutate(r_factor=(totvoting-edr)/reg7am)

write.csv(competition_data, file = "data/processed/competition_data_with_r.csv")
write_rds(competition_data, "./data/processed/competition_data_with_r.rds")