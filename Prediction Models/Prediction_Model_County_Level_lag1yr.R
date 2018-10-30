#### Prediction Model (County Level) using prev 1year data
#### Ziyi Lu

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(glmnet)

# Load Voting Data (even years)
totvoting_2012 <- read_xlsx("data/unprocessed/2012_general_results.xlsx") %>% 
  select(TOTVOTING,COUNTYCODE) %>% 
  rename(countycode = COUNTYCODE) %>% 
  mutate_all(., funs(as.numeric)) %>% 
  group_by(countycode) %>%
  summarise_all(sum)
totvoting_2014 <- read_xlsx("data/unprocessed/2014_general_results.xlsx") %>% 
  select(TOTVOTING,COUNTYCODE) %>% 
  rename(countycode = COUNTYCODE) %>% 
  mutate_all(., funs(as.numeric)) %>% 
  group_by(countycode) %>%
  summarise_all(sum)
totvoting_2016 <- read_xlsx("data/unprocessed/2016_general_results.xlsx") %>% 
  select(TOTVOTING,COUNTYCODE) %>% 
  rename(countycode = COUNTYCODE) %>% 
  mutate_all(., funs(as.numeric)) %>% 
  group_by(countycode) %>%
  summarise_all(sum)

# Load FRED data
data <- readRDS("data/processed/competition_data_oddyears.rds") %>%
  select(-pctname,-signatures,-county,-date)  #total = signatures + ab_mb
data$mailballot <-ifelse(data$mailballot =="Yes", 1, 0)
data <- mutate_all(data, funs(as.numeric))


dat2011 <- data %>% 
  filter(year == 2011) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag1yr"))) %>% 
  rename(countycode = countycode_lag1yr) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # no election data
  group_by(countycode) %>%
  summarise_all(sum)
df2012 <- unique(inner_join(dat2011,totvoting_2012, by = "countycode"))

dat2013 <- data %>% 
  filter(year == 2013) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag1yr"))) %>% 
  rename(countycode = countycode_lag1yr) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # no election data
  group_by(countycode) %>%
  summarise_all(sum)
df2014 <- unique(inner_join(dat2013,totvoting_2014, by = "countycode"))

dat2015 <- data %>% 
  filter(year == 2015) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag1yr"))) %>% 
  rename(countycode = countycode_lag1yr) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # no election data
  group_by(countycode) %>%
  summarise_all(sum)
df2016 <- unique(inner_join(dat2015,totvoting_2016, by = "countycode"))

df <- bind_rows(df2012, df2014, df2016)[,-1]

#### Preparing training and testing sets
x <- model.matrix(TOTVOTING~., df)[,-1]
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
sum(abs(ridge_pred-ytest)/ytest) # 91.83608
sum(abs(lasso_pred-ytest)/ytest) # 542.3697
