#### randomForest -- edr predictions

# load packages
library(tidyverse)
library(randomForest)

# read data
path <- "./Prediction Models/RandomForest/"

results_train <- read_rds(paste0(path, "results_train.rds"))
results_test <- read_rds(paste0(path, "results_test.rds"))

# split into x and y
results_trainx <- results_train %>% 
  select(-year, -pctname, -pctcode, -countycode, -congdist, -mailballot, -signatures, -date, -county, -ab_mb, -edr, -totvoting) %>% 
  as.matrix()

results_trainy <- results_train %>% mutate(edr = as.double(edr)) %>% select(edr) %>% as_vector()

results_testx <- results_test %>%
  select(-year, -pctname, -pctcode, -countycode, -congdist, -mailballot, -signatures, -date, -county, -ab_mb, -edr, -totvoting) %>% 
  as.matrix()

results_testy <- results_test %>% mutate(edr = as.double(edr)) %>% select(edr) %>% as_vector()

# train randomForest model
set.seed(713)
rforest_mod <- randomForest(x = results_trainx, y = results_trainy, xtest = results_testx, ytest = results_testy, mtry = 10, importance = TRUE, keep.forest = TRUE)

# save model
write_rds(rforest_mod, paste0(path, "randomForest_edr.rds"))

plot(rforest_mod)
importance(rforest_mod) %>% as_tibble() %>% mutate(feature = rownames(importance(rforest_mod))) %>% arrange(desc(`%IncMSE`))

predicted <- predict(rforest_mod, newdata = results_testx)

predicted_table <- tibble(
  predicted = predicted,
  observed = results_testy
)

predicted_table %>%
  mutate(observed = if_else(observed == 0, 0.0000001, observed)) %>% 
  summarise(acc = mean((predicted - observed)^2)) # MSE = 1965

predicted_table %>% 
  ggplot(aes(observed, predicted)) +
  geom_point(alpha = 0.05) +
  geom_line(aes(observed, observed))

# generate 2018 predictions
rforest_mod <- read_rds("./Prediction Models/randomForest/randomForest_edr.rds")

dat_2018 <- read_rds("./data/processed/data_2018.rds") %>% 
  select(-precinct_name, -precinct_code, -countycode, -congdist, -county_name) %>% 
  as.matrix()

edr_preds <- predict(rforest_mod, newdata = dat_2018)

write_rds(edr_preds, "./Prediction Models/randomForest/edrpreds.rds")
