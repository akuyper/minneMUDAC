library(tidyverse)
library(skimr)
library(janitor)

# load data
general_results <- read_csv("data/processed/2008_2016_general_result.csv")
general_results <- general_results[-nrow(general_results),]

#skim
general_results %>% 
  skim()
#change congdist format
general_results<-general_results %>% 
  mutate(congdist=as.character(as.numeric(congdist))) %>% 
  mutate(year=as.character(year))


#examine total voting
general_results %>% 
  filter(!is.na(congdist)) %>% 
  #filter(year==2016) %>% 
  group_by(congdist) %>% 
  summarise(totvoting=sum(totvoting)) %>% 
  ggplot(aes(x=congdist,y=totvoting)) +
  geom_col()+
  labs(title="total voting on congressional district level")

general_results %>% 
  filter(!is.na(congdist)) %>% 
  filter(year==2016) %>% 
  group_by(congdist) %>% 
  summarise(totvoting=sum(totvoting)) %>% 
  ggplot(aes(x=congdist,y=totvoting)) +
  geom_col()+
  labs(title="total voting on congressional district level in 2016")

general_results %>% 
  filter(!is.na(congdist)) %>% 
  filter(year==2008) %>% 
  group_by(congdist) %>% 
  summarise(totvoting=sum(totvoting)) %>% 
  ggplot(aes(x=congdist,y=totvoting)) +
  geom_col()+
  labs(title="average total voting on congressional district level in 2008")

general_results %>% 
  filter(!is.na(congdist)) %>% 
  #filter(year==2008) %>% 
  group_by(congdist,year) %>% 
  summarise(totvoting=sum(totvoting)) %>% 
  ggplot(aes(x=year,y=totvoting,group=congdist,color=congdist)) +
  geom_line()+
  labs(title="total voting change since 2008 to 2016 on congressional district level")

#mailbot 
general_results %>% 
  filter(!is.na(mailballot) & !is.na(congdist)) %>%
  group_by(mailballot,congdist) %>% 
  summarise(totvoting=sum(totvoting)) %>% 
  ggplot(aes(x=congdist,y=totvoting,color=mailballot)) +
  geom_col()+
  labs(title="average total voting on congressional district level in 2008")

  
