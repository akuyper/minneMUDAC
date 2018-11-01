#### LM Model for edr (Presinct Level)
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

data_2018 <- readRDS("data/processed/county_dat_2018.rds")%>%
  ungroup() %>% 
  select(-county,-year) %>% 
  rename(countycode = COUNTYCODE)


#### Presinct Level
df2010 <- data %>% 
  filter(year == 2010) %>% 
  select(-year,-countycode,-pres) %>% 
  group_by(congdist,pctcode) %>%
  summarise_all(mean) %>% 
  select_if(~sum(.) > 0) %>% 
  arrange(pctcode) %>% 
  ungroup() 
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

# Full LM Model
mod_2010 <- df2010 %>%
  lm(edr~age_med+assoc_deg+buildings++burd_house+      
       commute+discon_yth+eq_sprime+hisp_latin+       
       homeowner+house_singp+inc_ineq+income_med+       
       income_percap+income_person+perc_pop_u18+pop_est+          
       pop_perc+pop_res+pop_u18+poverty+poverty_u18+prec_rel_chil_518+
       privat_ests+race_white+racical_dissim+rel_chil_518+undergrad+unemployment,data = .)
mod_2012 <- df2012 %>%
  lm(edr~age_med+assoc_deg+buildings++burd_house+      
       commute+discon_yth+eq_sprime+hisp_latin+       
       homeowner+house_singp+inc_ineq+income_med+       
       income_percap+income_person+perc_pop_u18+pop_est+          
       pop_perc+pop_res+pop_u18+poverty+poverty_u18+prec_rel_chil_518+
       privat_ests+race_white+racical_dissim+rel_chil_518+undergrad+unemployment,data = .)
mod_2014 <- df2014 %>%
  lm(edr~age_med+assoc_deg+buildings++burd_house+      
       commute+discon_yth+eq_sprime+hisp_latin+       
       homeowner+house_singp+inc_ineq+income_med+       
       income_percap+income_person+perc_pop_u18+pop_est+          
       pop_perc+pop_res+pop_u18+poverty+poverty_u18+prec_rel_chil_518+
       privat_ests+race_white+racical_dissim+rel_chil_518+undergrad+unemployment,data = .)
mod_2016 <- df2016 %>%
  lm(edr~age_med+assoc_deg+buildings++burd_house+      
       commute+discon_yth+eq_sprime+hisp_latin+       
       homeowner+house_singp+inc_ineq+income_med+       
       income_percap+income_person+perc_pop_u18+pop_est+          
       pop_perc+pop_res+pop_u18+poverty+poverty_u18+prec_rel_chil_518+
       privat_ests+race_white+racical_dissim+rel_chil_518+undergrad+unemployment,data = .)

lm_coef_2010 <- as.matrix(mod_2010$coefficients)
lm_coef_2012 <- as.matrix(mod_2012$coefficients)
lm_coef_2014 <- as.matrix(mod_2014$coefficients)
lm_coef_2016 <- as.matrix(mod_2016$coefficients)
lm_coef <- (1/3)*lm_coef_2014 + (1/3)*lm_coef_2010 + (1/6)*lm_coef_2016 + (1/6)*lm_coef_2012 

test_data <- data_2018 %>% 
  select(-countycode)
test_data <- as.matrix(data.frame(1, test_data))
edr_hat <- (test_data %*% lm_coef)
data.frame(data_2018$countycode,edr_hat)


