#### PCA Model (County Level) using prev 2years data
#### Ziyi Lu

# Load Packages
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(pls)
library(rpart)

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

dat2010 <- data %>% 
  filter(year == 2010) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag2yr"))) %>% 
  rename(countycode = countycode_lag2yr) %>% 
  group_by(countycode) %>%
  summarise_all(sum)
dat2011 <- data %>% 
  filter(year == 2011) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag1yr"))) %>% 
  rename(countycode = countycode_lag1yr) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # no election data
  group_by(countycode) %>%
  summarise_all(sum)
df2012 <- unique(inner_join(dat2010,totvoting_2012, by = "countycode"))
df2012 <- unique(inner_join(dat2011,df2012, by = "countycode"))

dat2012 <- data %>% 
  filter(year == 2012) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag2yr"))) %>% 
  rename(countycode = countycode_lag2yr) %>% 
  group_by(countycode) %>%
  summarise_all(sum)
dat2013 <- data %>% 
  filter(year == 2013) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag1yr"))) %>% 
  rename(countycode = countycode_lag1yr) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # no election data
  group_by(countycode) %>%
  summarise_all(sum)
df2014 <- unique(inner_join(dat2012,totvoting_2014, by = "countycode"))
df2014 <- unique(inner_join(dat2013,df2014, by = "countycode"))

dat2014 <- data %>% 
  filter(year == 2014) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag2yr"))) %>% 
  rename(countycode = countycode_lag2yr) %>% 
  group_by(countycode) %>%
  summarise_all(sum)
dat2015 <- data %>% 
  filter(year == 2015) %>% 
  select(-year,-congdist,-pctcode,-pres,-cong) %>% 
  rename_all(., funs(paste0(., "_lag1yr"))) %>% 
  rename(countycode = countycode_lag1yr) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # no election data
  group_by(countycode) %>%
  summarise_all(sum)
df2016 <- unique(inner_join(dat2014,totvoting_2016, by = "countycode"))
df2016 <- unique(inner_join(dat2015,df2016, by = "countycode"))

df <- bind_rows(df2012, df2014, df2016)[,-1] %>% select_if(~sum(.) > 0)

#### Preparing training and testing sets
x <- model.matrix(TOTVOTING~., df)[,-1]
y <- df$TOTVOTING

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
ytest <- y[test]
ytrain <- y[train]

#### PCA Model
pca_model <- prcomp(x[train,], scale = TRUE)

#### Decide number of PCs to use
#compute standard deviation of each principal component
std_dev <- pca_model$sdev
#compute variance
pr_var <- std_dev^2
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
#scree plot
#should use 2 PC
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained",type = "b")


#### LM method
train_pcs <- pca_model$x[,1:2]
lm.model <- lm(ytrain ~ train_pcs)
lm_coef <- as.matrix(lm.model$coefficients)
test_pcs <- as.data.frame(predict(pca_model, x[test,])[,1:2])
test_data <- as.matrix(data.frame(1, test_pcs))
lm.prediction <- test_data %*% lm_coef
# Absolute percent error
sum(abs(lm.prediction-ytest)/ytest) #116.16

#### Regression Tree
train.data <- data.frame(ytrain, pca_model$x)[,1:2]
rpart.model <- rpart(ytrain ~ .,data = train.data, method = "anova")
plotcp(rpart.model) 
test.data <- predict(pca_model, newdata = data.frame(ytest, x[test,]))
test.data <- as.data.frame(test.data)[,1:2]
rpart.prediction <- predict(rpart.model, test.data)
# Absolute percent error
sum(abs(rpart.prediction-ytest)/ytest) #125.0285
