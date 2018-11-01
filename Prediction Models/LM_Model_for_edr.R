#### LM Model for edr (County Level)
#### Ziyi Lu

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)

# Load data
data <- readRDS("data/processed/competition_data.rds") %>%
  select(-pctname,-signatures,-county,-date)  #total = signatures + ab_mb
data$mailballot <-ifelse(data$mailballot =="Yes", 1, 0)
data <- mutate_all(data, funs(as.numeric))

# # County Level
# df2012 <- data %>% 
#   filter(year == 2012) %>% 
#   select(-year,-pctcode,-pres) %>% 
#   group_by(congdist,countycode) %>%
#   summarise_all(mean) %>% 
#   select_if(~sum(.) > 0) %>% 
#   arrange(countycode) %>% 
#   ungroup() 
# df2014 <- data %>% 
#   filter(year == 2014) %>% 
#   select(-year,-pctcode,-pres) %>% 
#   group_by(congdist,countycode) %>%
#   summarise_all(mean) %>% 
#   select_if(~sum(.) > 0) %>% 
#   arrange(countycode)%>% 
#   ungroup() 
# df2016 <- data %>% 
#   filter(year == 2016) %>% 
#   select(-year,-pctcode,-pres) %>% 
#   group_by(congdist,countycode) %>%
#   summarise_all(mean) %>% 
#   select_if(~sum(.) > 0) %>% 
#   arrange(countycode)%>% 
#   ungroup()
# df2012 %>%
#   select(-countycode,-congdist,-totvoting) %>%
#   lm(edr~.,data = .) %>%
#   summary()
# df2014 %>%
#   select(-countycode,-congdist,-totvoting) %>%
#   lm(edr~.,data = .) %>%
#   summary()
# df2016 %>%
#   select(-countycode,-congdist,-totvoting) %>%
#   lm(edr~.,data = .) %>%
#   summary()

#### Presinct Level
df2012 <- data %>% 
  filter(year == 2012) %>% 
  select(-year,-countycode,-pres) %>% 
  group_by(congdist,pctcode) %>%
  summarise_all(mean) %>% 
  select_if(~sum(.) > 0) %>% 
  arrange(pctcode) %>% 
  ungroup() 
df2014 <- data %>% 
  filter(year == 2014) %>% 
  select(-year,-countycode,-pres) %>% 
  group_by(congdist,pctcode) %>%
  summarise_all(mean) %>% 
  select_if(~sum(.) > 0) %>% 
  arrange(pctcode)%>% 
  ungroup() 
df2016 <- data %>% 
  filter(year == 2016) %>% 
  select(-year,-countycode,-pres) %>% 
  group_by(congdist,pctcode) %>%
  summarise_all(mean) %>% 
  select_if(~sum(.) > 0) %>% 
  arrange(pctcode)%>% 
  ungroup()

df2012 %>%
  select(-pctcode,-congdist,-totvoting) %>%
  lm(edr~.,data = .) %>%
  summary()
df2014 %>%
  select(-pctcode,-congdist,-totvoting) %>%
  lm(edr~.,data = .) %>%
  summary()
df2016 %>%
  select(-pctcode,-congdist,-totvoting) %>%
  lm(edr~.,data = .) %>%
  summary()


