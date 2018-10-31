#### regression tree

# load packages
library(tidyverse)
library(randomForest)

# load data
results_dat <- read_rds("./data/processed/competition_data.rds")
county_2018 <- read_rds("./data/processed/county_dat_2018.rds")

# create training and test sets
results_predictive <- results_dat %>% 
  select(-year, -pctname, -pctcode, -countycode, -congdist, -mailballot, -signatures, -date, -county, -ab_mb) %>% 
  mutate(edr = as.double(edr))

set.seed(713)

results_test <- results_dat %>% 
  filter(year == 2014) %>% 
  sample_frac(0.5) %>% 
  bind_rows(results_dat %>% 
              filter(year == 2016) %>%
              sample_frac(0.5))

results_train <- results_dat %>% 
  setdiff(results_test)

# use only predictive features
results_trainx <- results_train %>% 
  select(-year, -pctname, -pctcode, -countycode, -congdist, -mailballot, -signatures, -date, -county, -ab_mb, -totvoting) %>% 
  mutate(edr = as.double(edr)) %>% 
  as.matrix()

results_trainy <- results_train %>% select(totvoting) %>% as_vector()

results_testx <- results_test %>% 
  select(-year, -pctname, -pctcode, -countycode, -congdist, -mailballot, -signatures, -date, -county, -ab_mb, -totvoting) %>% 
  mutate(edr = as.double(edr)) %>% 
  as.matrix()

results_testy <- results_test %>% select(totvoting) %>% as_vector()

# returns random forest model using 10 variables at each split
set.seed(713)
rforest_mod <- randomForest(x = results_trainx, y = results_trainy, xtest = results_testx, ytest = results_testy, mtry = 10, importance = TRUE)

# write model to rds file
write_rds(rforest_mod, "./Prediction Models/randomforest_precinctlvl_evenyrs_mod.rds")

# view variable importance
plot(rforest_mod)
importance(rforest_mod) %>% as_tibble() %>% mutate(feature = rownames(importance(rforest_mod))) %>% arrange(desc(`%IncMSE`))

predicted_table <- tibble(
  predicted = rforest_mod$predicted,
  observed = results_trainy
)

predicted_table %>% 
  summarise(acc = 100 - sqrt((sum(predicted) - sum(observed))^2)/(sum(observed))*100) # 99.9% accuracy for statewide turnout

predicted_table %>% 
  ggplot(aes(observed, predicted)) +
  geom_point(alpha = 0.05) +
  geom_line(aes(observed, observed))
