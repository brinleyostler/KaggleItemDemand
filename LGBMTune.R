#### STORE ITEM DEMAND ####
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(vroom)
library(embed)
library(bonsai)
library(lightgbm)

#### READ IN THE DATA ####
test = vroom("./test.csv")
train = vroom("./train.csv")

### LIGHTGBM MODEL ####
testTune <- test %>%
  filter(store %in% 1:3) %>%
  filter(item == 1:3)
trainTune <- train %>% 
  filter(store %in% 1:3) %>% 
  filter(item == 1:3)

## Recipe
store_recipe <- recipe(sales~., data=trainTune) %>%
  step_date(date, features = c("dow", "month", "decimal", "doy", "year")) %>% 
  step_holiday(date, role = "predictor") %>% 
  step_range(date_doy, min = 0, max = pi) %>%                                 
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%             
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales)) %>%     
  step_normalize(all_numeric_predictors()) %>%                                
  step_rm(date, item, store)                                                  

## Model
lgbm_model <- boost_tree(tree_depth=tune(), # 1
                         learn_rate=tune(),  # 0.1
                         trees=1000) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

## Workflow
lgbm_wf <- workflow() %>%
  add_recipe(store_recipe) %>%
  add_model(lgbm_model)

## Tuning Grid
tuning_grid_lgbm <- grid_regular(tree_depth(),
                                 learn_rate(),
                                 levels=3)

## CV 
folds <- vfold_cv(trainTune, v=5, repeats=1)
CV_results_lgbm <- lgbm_wf %>% 
  tune_grid(resamples=folds,
            grid=tuning_grid_lgbm,
            metrics=metric_set(smape))

best_tune_lgbm <- CV_results_lgbm %>% 
  select_best(metric="smape")
print(best_tune_lgbm) 

