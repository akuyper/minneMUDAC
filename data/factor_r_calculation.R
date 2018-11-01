library(tidyverse)

competition_data <- readRDS("data/processed/competition_data.rds")

colnames(competition_data)

competition_data <- 
  competition_data %>% 
  mutate(r_factor=(totvoting-edr)/reg7am)

write.csv(competition_data, file = "data/processed/competition_data_with_r.csv")
