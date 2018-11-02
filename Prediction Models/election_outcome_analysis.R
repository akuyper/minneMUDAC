#### Models to analyse MN election outcome on prescint level
#### Ziyi Lu

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(glmnet)

# Load data
data <- readRDS("data/processed/competition_data.rds") %>%
  select(-pctname,-signatures,-county,-date)  #total = signatures + ab_mb
data$mailballot <- ifelse(data$mailballot =="Yes", 1, 0)
data <- mutate_all(data, funs(as.numeric))

data_withresult <- readRDS("data/processed/all_data_inspect_withresult.rds") %>% 
  na.omit() %>% 
  select(year,countycode,pctcode,dem)
data_withresult <- mutate_all(data_withresult, funs(as.numeric))

df <- data_withresult %>% 
  left_join(data, by = c("year","countycode","pctcode"))

#### Presinct Level Data
df2010 <- df %>% 
  filter(year == 2010) 
df2012 <- df %>% 
  filter(year == 2012)
df2014 <- df %>% 
  filter(year == 2014) 
df2016 <- df %>% 
  filter(year == 2016)

# Logistic Models with all FRED variables
logistic_mod_2010 <- df2010 %>% 
  glm(dem~age_med+assoc_deg+buildings+burd_house+      
       commute+discon_yth+eq_sprime+hisp_latin+       
       homeowner+house_singp+inc_ineq+income_med+       
       income_percap+income_person+perc_pop_u18+pop_est+          
       pop_perc+pop_res+pop_u18+poverty+poverty_u18+prec_rel_chil_518+
       privat_ests+race_white+racical_dissim+rel_chil_518+undergrad+unemployment,data = .,family = "binomial")

logistic_mod_2012 <- df2012 %>% 
  glm(dem~age_med+assoc_deg+buildings+burd_house+      
        commute+discon_yth+eq_sprime+hisp_latin+       
        homeowner+house_singp+inc_ineq+income_med+       
        income_percap+income_person+perc_pop_u18+pop_est+          
        pop_perc+pop_res+pop_u18+poverty+poverty_u18+prec_rel_chil_518+
        privat_ests+race_white+racical_dissim+rel_chil_518+undergrad+unemployment,data = .,family = "binomial")

logistic_mod_2014 <- df2014 %>% 
  glm(dem~age_med+assoc_deg+buildings+burd_house+      
        commute+discon_yth+eq_sprime+hisp_latin+       
        homeowner+house_singp+inc_ineq+income_med+       
        income_percap+income_person+perc_pop_u18+pop_est+          
        pop_perc+pop_res+pop_u18+poverty+poverty_u18+prec_rel_chil_518+
        privat_ests+race_white+racical_dissim+rel_chil_518+undergrad+unemployment,data = .,family = "binomial")

logistic_mod_2016 <- df2016 %>% 
  glm(dem~age_med+assoc_deg+buildings+burd_house+      
        commute+discon_yth+eq_sprime+hisp_latin+       
        homeowner+house_singp+inc_ineq+income_med+       
        income_percap+income_person+perc_pop_u18+pop_est+          
        pop_perc+pop_res+pop_u18+poverty+poverty_u18+prec_rel_chil_518+
        privat_ests+race_white+racical_dissim+rel_chil_518+undergrad+unemployment,data = .,family = "binomial")

# comparing Z scores
zvalue_logit2010 <- as.matrix(summary(logistic_mod_2010)$coefficients[-1,])
zvalue_logit2010 <- as.matrix(zvalue_logit2010[order(zvalue_logit2010[,3]),])
logit2010_negfct <- head(zvalue_logit2010, 3) 
logit2010_posfct <- tail(zvalue_logit2010, 3)

zvalue_logit2012 <- as.matrix(summary(logistic_mod_2012)$coefficients[-1,])
zvalue_logit2012 <- as.matrix(zvalue_logit2012[order(zvalue_logit2012[,3]),])
logit2012_negfct <- as.matrix(head(zvalue_logit2012, 3))
logit2012_posfct <- as.matrix(tail(zvalue_logit2012, 3))

zvalue_logit2014 <- as.matrix(summary(logistic_mod_2014)$coefficients[-1,])
zvalue_logit2014 <- as.matrix(zvalue_logit2014[order(zvalue_logit2014[,3]),])
logit2014_negfct <- as.matrix(head(zvalue_logit2014, 3))
logit2014_posfct <- as.matrix(tail(zvalue_logit2014, 3))

zvalue_logit2016 <- as.matrix(summary(logistic_mod_2016)$coefficients[-1,])
zvalue_logit2016 <- as.matrix(zvalue_logit2016[order(zvalue_logit2016[,3]),])
logit2016_negfct <- as.matrix(head(zvalue_logit2016, 3))
logit2016_posfct <- as.matrix(tail(zvalue_logit2016, 3))


# pos influce trend
logit2010_posfct
logit2012_posfct
logit2014_posfct
logit2016_posfct

# neg influce trend
logit2010_negfct
logit2012_negfct
logit2014_negfct
logit2016_negfct


## common factots

## pos influce trend
## assoc_deg burd_house hisp_latin race_white pop_perc
logit2010_poscommfct <- data.frame(zvalue_logit2010[c("assoc_deg","burd_house","hisp_latin","race_white","pop_perc"),3])
colnames(logit2010_poscommfct) <- "yr2010"
logit2012_poscommfct <- data.frame(zvalue_logit2012[c("assoc_deg","burd_house","hisp_latin","race_white","pop_perc"),3])
colnames(logit2012_poscommfct) <- "yr2012"
logit2014_poscommfct <- data.frame(zvalue_logit2014[c("assoc_deg","burd_house","hisp_latin","race_white","pop_perc"),3])
colnames(logit2014_poscommfct) <- "yr2014"
logit2016_poscommfct <- data.frame(zvalue_logit2016[c("assoc_deg","burd_house","hisp_latin","race_white","pop_perc"),3])
colnames(logit2016_poscommfct) <- "yr2016"

poscommfct1 <- merge(logit2010_poscommfct,logit2012_poscommfct,by="row.names")
poscommfct2 <- merge(logit2014_poscommfct,logit2016_poscommfct,by="row.names")
poscommfct <- right_join(poscommfct1,poscommfct2,by = "Row.names")

saveRDS(poscommfct,"data/processed/pos_comm_fct.rds")

## neg influce trend
## burd_house
## perc_pop_u18 inc_ineq pop_res prec_rel_chil_518 privat_ests pop_res poverty_u18 privat_ests pop_u18
logit2010_negcommfct <- data.frame(zvalue_logit2010[c("perc_pop_u18","inc_ineq","pop_res","prec_rel_chil_518","privat_ests"),3])
colnames(logit2010_negcommfct) <- "yr2010"
logit2012_negcommfct <- data.frame(zvalue_logit2012[c("perc_pop_u18","inc_ineq","pop_res","prec_rel_chil_518","privat_ests"),3])
colnames(logit2012_negcommfct) <- "yr2012"
logit2014_negcommfct <- data.frame(zvalue_logit2014[c("perc_pop_u18","inc_ineq","pop_res","prec_rel_chil_518","privat_ests"),3])
colnames(logit2014_negcommfct) <- "yr2014"
logit2016_negcommfct <- data.frame(zvalue_logit2016[c("perc_pop_u18","inc_ineq","pop_res","prec_rel_chil_518","privat_ests"),3])
colnames(logit2016_negcommfct) <- "yr2016"
negcommfct1 <- merge(logit2010_negcommfct,logit2012_negcommfct,by="row.names")
negcommfct2 <- merge(logit2014_negcommfct,logit2016_negcommfct,by="row.names")
negcommfct3 <- right_join(negcommfct1,negcommfct2,by = "Row.names")

logit2010_negcommfct <- data.frame(zvalue_logit2010[c("pop_res","poverty_u18","privat_ests","pop_u18"),3])
colnames(logit2010_negcommfct) <- "yr2010"
logit2012_negcommfct <- data.frame(zvalue_logit2012[c("pop_res","poverty_u18","privat_ests","pop_u18"),3])
colnames(logit2012_negcommfct) <- "yr2012"
logit2014_negcommfct <- data.frame(zvalue_logit2014[c("pop_res","poverty_u18","privat_ests","pop_u18"),3])
colnames(logit2014_negcommfct) <- "yr2014"
logit2016_negcommfct <- data.frame(zvalue_logit2016[c("pop_res","poverty_u18","privat_ests","pop_u18"),3])
colnames(logit2016_negcommfct) <- "yr2016"
negcommfct4 <- merge(logit2010_negcommfct,logit2012_negcommfct,by="row.names")
negcommfct5 <- merge(logit2014_negcommfct,logit2016_negcommfct,by="row.names")
negcommfct6 <- right_join(negcommfct4,negcommfct5,by = "Row.names")

samecols <- intersect(colnames(negcommfct3),colnames(negcommfct6))
negcommfct <- merge(negcommfct3,negcommfct6, by=samecols, all=TRUE)[samecols]

saveRDS(negcommfct,"data/processed/pos_comm_fct.rds")
