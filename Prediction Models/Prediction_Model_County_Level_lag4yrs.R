#### Prediction Model (County Level) using prev 4years data
#### Ziyi Lu

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(glmnet)


totvoting_2014 <- read_xlsx("data/unprocessed/2014_general_results.xlsx") %>% 
  select(TOTVOTING,COUNTYCODE) %>% 
  rename(countycode = COUNTYCODE) %>% 
  mutate_all(., funs(as.numeric)) %>% 
  group_by(countycode) %>%
  summarise_all(sum)

data <- readRDS("data/processed/competition_data_oddyears.rds") %>%
  select(-pctname,-signatures,-county,-date)  #total = signatures + ab_mb
data$mailballot <-ifelse(data$mailballot =="Yes", 1, 0)
data <- mutate_all(data, funs(as.numeric))

dat2010 <- data %>% 
  filter(year == 2010) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_2010"))) %>% 
  rename(countycode = countycode_2010) %>% 
  group_by(countycode) %>%
  summarise_all(sum)
df <- unique(inner_join(dat2010,totvoting_2014, by = "countycode"))

dat2011 <- data %>% 
  filter(year == 2011) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_2011"))) %>% 
  rename(countycode = countycode_2011) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # no election data
  group_by(countycode) %>%
  summarise_all(sum)
df <- unique(inner_join(dat2011,df, by = "countycode"))

dat2012 <- data %>% 
  filter(year == 2012) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_2012"))) %>% 
  rename(countycode = countycode_2012) %>% 
  group_by(countycode) %>%
  summarise_all(sum)
df <- unique(inner_join(dat2012,df, by = "countycode"))

dat2013 <- data %>% 
  filter(year == 2013) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_2013"))) %>% 
  rename(countycode = countycode_2013) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # no election data
  group_by(countycode) %>%
  summarise_all(sum)
df <- unique(inner_join(dat2013,df, by = "countycode"))

#### Preparing training and testing sets
x <- model.matrix(TOTVOTING~., df)[,-1:-2]
y <- df$TOTVOTING

set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
lambda <- 10^seq(10, -2, length = 100)

#### Models
ridge_mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
lasso_mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
cv_out <- cv.glmnet(x[train,], y[train], alpha = 0)
bestlam <- cv_out$lambda.min

# make predictions
ridge_pred <- predict(ridge_mod, s = bestlam, newx = x[test,])
lasso_pred <- predict(lasso_mod, s = bestlam, newx = x[test,])

# Absolute percent error
sum(abs(ridge_pred-ytest)/ytest) # 22.29813
sum(abs(lasso_pred-ytest)/ytest) # 91.10171
