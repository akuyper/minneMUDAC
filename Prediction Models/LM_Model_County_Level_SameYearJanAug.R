#### LM&Lasso&Ridge&PCA Model (County Level) using same year Jan - Aug data
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

df2012 <- data %>% 
  filter(year == 2012) %>% 
  select(-year,-pctcode,-pres) %>% 
  group_by(congdist,countycode) %>%
  summarise_all(sum) %>% 
  select_if(~sum(.) > 0) %>% 
  arrange(countycode) %>% 
  ungroup() 

df2014 <- data %>% 
  filter(year == 2014) %>% 
  select(-year,-pctcode,-pres) %>% 
  group_by(congdist,countycode) %>%
  summarise_all(sum) %>% 
  select_if(~sum(.) > 0) %>% 
  arrange(countycode)%>% 
  ungroup() 

df2016 <- data %>% 
  filter(year == 2016) %>% 
  select(-year,-pctcode,-pres) %>% 
  group_by(congdist,countycode) %>%
  summarise_all(sum) %>% 
  select_if(~sum(.) > 0) %>% 
  arrange(countycode)%>% 
  ungroup() 

#### Preparing training and testing sets
set.seed(1)
train <- sample(1:dim(df2012)[1], .75*dim(df2012)[1]) #75% train
test <- (-train) #25% train

#### LM model: directly predicting V
df_list <- list(df2012,df2014,df2016)
lapply(df_list, function(x) {
  lm_mod <-  x[train,] %>% 
    select(-countycode,-congdist) %>% 
    lm(totvoting~.,data = .)
  lm_coef <- as.matrix(lm_mod$coefficients)
  test_data <- x[test,] %>% 
    select(-countycode,-congdist,-totvoting)
  test_data <- as.matrix(data.frame(1, test_data))
  yhat <- test_data %*% lm_coef
  y_data <- x[test,]$totvoting
  aps <- abs(yhat - y_data)/y_data
  cong <- x[test,]$congdist
  total_result <- data.frame(aps,cong) %>% 
    group_by(cong) %>%
    summarise_all(sum)
  sum(total_result)})

# [LM Model Absolute percent error]
# [2012] 33.23076
# [2014] 35.70696
# [2016] 32.96768

# df2012[train,] %>% 
#   select(-countycode,-congdist) %>% 
#   lm(totvoting~.,data = .) %>% 
#   summary()

# LM model variation I: V = edr + r*N
lapply(df_list, function(x) {
  x_train <-  x[train,] %>% 
    select(-countycode,-congdist,-totvoting,-edr) %>% 
    as.matrix()
  y_train_totvoting <-  x[train,] %>% 
    select(totvoting)
  y_train_edr <-  x[train,] %>% 
    select(edr)
  y_train <- (y_train_totvoting-y_train_edr) %>% 
    rename(y_train = totvoting) %>% 
    as.matrix()
  lm_mod <- lm(y_train~x_train)
  lm_coef <- as.matrix(lm_mod$coefficients)
  test_data <- x[test,] %>% 
    select(-countycode,-congdist,-totvoting,-edr)
  test_data <- as.matrix(data.frame(1, test_data))
  yhat <- (test_data %*% lm_coef) + x[test,]$edr
  y_test <-  x[test,] %>% 
    select(totvoting)
  aps <- abs(yhat - y_test)/y_test
  cong <- x[test,]$congdist
  total_result <- data.frame(aps,cong) %>% 
    group_by(cong) %>%
    summarise_all(sum)
  sum(total_result)})
# [LM Model Var1 Absolute percent error]
# [2012] 33.05455
# [2014] 35.70539
# [2016] 33.05389

# LM model variation II: V = r*popu
lapply(df_list, function(x) {
  x_train <-  x[train,] %>% 
    select(-countycode,-congdist,-totvoting,-pop_est) %>% 
    as.matrix()
  y_train_totvoting <-  x[train,] %>% 
    select(totvoting)
  y_train_pop_est <-  x[train,] %>% 
    select(pop_est)
  y_train <- (y_train_totvoting/y_train_pop_est) %>% 
    rename(y_train = totvoting) %>% 
    as.matrix()
  lm_mod <- lm(y_train~x_train)
  lm_coef <- as.matrix(lm_mod$coefficients)
  test_data <- x[test,] %>% 
    select(-countycode,-congdist,-totvoting,-pop_est)
  test_data <- as.matrix(data.frame(1, test_data))
  yhat <- (test_data %*% lm_coef)*(x[test,]$pop_est)
  y_test_totvoting <-  x[test,] %>% select(totvoting)
  aps <- abs(yhat - y_test_totvoting)/y_test_totvoting
  cong <- x[test,]$congdist
  total_result <- data.frame(aps,cong) %>% 
    group_by(cong) %>%
    summarise_all(sum)
  sum(total_result)})
# [LM Model Var2 Absolute percent error]
# [2012] 2067.747
# [2014] 2655.257
# [2016] 2646.285


#### Lasso & Ridge model
lambda <- 10^seq(10, -2, length = 100)
lapply(df_list, function(x) {
  x_train <-  x[train,] %>% 
    select(-countycode,-congdist,-totvoting)%>% 
    as.matrix
  y_train <-  x[train,] %>% 
    select(totvoting)%>% 
    as.matrix
  ridge_mod <- glmnet(x_train, y_train, alpha = 0, lambda = lambda)
  cv_out <- cv.glmnet(x_train, y_train, alpha = 0)
  bestlam <- cv_out$lambda.min
  x_test <-  x[test,] %>% 
    select(-countycode,-congdist,-totvoting)%>% 
    as.matrix
  y_test <-  x[test,] %>% 
    select(totvoting)%>% 
    as.matrix
  ridge_pred <- predict(ridge_mod, s = bestlam, newx = x_test)
  ridge_aes <- abs(ridge_pred - y_test)/y_test
  cong <- x[test,]$congdist
  ridge_total_result <- data.frame(ridge_aes,cong) %>% 
    group_by(cong) %>%
    summarise_all(sum)
  sum(ridge_total_result)})
# [Ridge Model Absolute percent error]
# [2012] 38.61277
# [2014] 54.48763
# [2016] 38.54313

lapply(df_list, function(x) {
  x_train <-  x[train,] %>% 
    select(-countycode,-congdist,-totvoting)%>% 
    as.matrix
  y_train <-  x[train,] %>% 
    select(totvoting)%>% 
    as.matrix
  lasso_mod <- glmnet(x_train, y_train, alpha = 1, lambda = lambda)
  cv_out <- cv.glmnet(x_train, y_train, alpha = 0)
  bestlam <- cv_out$lambda.min
  x_test <-  x[test,] %>% 
    select(-countycode,-congdist,-totvoting)%>% 
    as.matrix
  y_test <-  x[test,] %>% 
    select(totvoting)%>% 
    as.matrix
  lasso_pred <- predict(lasso_mod, s = bestlam, newx = x_test)
  lasso_aes <- abs(lasso_pred - y_test)/y_test
  cong <- x[test,]$congdist
  lasso_total_result <- data.frame(lasso_aes,cong) %>% 
    group_by(cong) %>%
    summarise_all(sum)
  sum(lasso_total_result)})
# [Ridge Model Absolute percent error]
# [2012] 37.06057
# [2014] 77.99822
# [2016] 38.08232

#### PCA LM Model
lapply(df_list, function(x) {
  x_train <-  x[train,] %>% 
    select(-countycode,-congdist,-totvoting)%>% 
    as.matrix
  y_train <-  x[train,] %>% 
    select(totvoting)%>% 
    as.matrix
  pca_model <- prcomp(x_train, scale = TRUE)
  
  # #scree plot
  # std_dev <- pca_model$sdev
  # pr_var <- std_dev^2
  # prop_varex <- pr_var/sum(pr_var)
  # plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained",type = "b")
  
  train_pcs <- pca_model$x[,1:2]
  lm.model <- lm(y_train ~ train_pcs)
  lm_coef <- as.matrix(lm.model$coefficients)
  test_pcs <- as.data.frame(predict(pca_model, x[test,])[,1:2])
  test_data <- as.matrix(data.frame(1, test_pcs))
  yhat <- test_data %*% lm_coef
  y_data <- x[test,]$totvoting
  aps <- abs(yhat - y_data)/y_data
  cong <- x[test,]$congdist
  total_result <- data.frame(aps,cong) %>% 
    group_by(cong) %>%
    summarise_all(sum)
  sum(total_result)})
# [PCA LM Model Absolute percent error]
# [2012] 47.51538
# [2014] 46.71851
# [2016] 47.89884