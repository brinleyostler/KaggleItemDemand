#### STORE ITEM DEMAND ####
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(modeltime)
library(timetk)
library(plotly)

#### READ IN THE DATA ####
test = vroom("/kaggle/input/demand-forecasting-kernels-only/test.csv")
train = vroom("/kaggle/input/demand-forecasting-kernels-only/train.csv")

### LIGHTGBM MODEL ####
nStores <- max(train$store)
nItems <- max(train$item)

## Recipe
store_recipe <- recipe(sales~., data=train) %>%
  step_date(date, features = c("dow", "month", "decimal", "doy", "year")) %>% 
  step_holiday(date, role = "predictor") %>% 
  step_range(date_doy, min = 0, max = pi) %>%                                 
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%             
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales)) %>%     
  step_normalize(all_numeric_predictors()) %>%                                
  step_rm(date, item, store)                                                  

## Model
lgbm_model <- boost_tree(tree_depth=1, # tune()
                         learn_rate=0.1,  # tune()
                         trees=1000) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

## Workflow
lgbm_wf <- workflow() %>%
  add_recipe(store_recipe) %>%
  add_model(lgbm_model)

### FOR LOOP ####
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
      filter(store==s, item==i)
    storeItemTest <- test %>%
      filter(store==s, item==i)
    
    ## Fit storeItem models here
    fitted_wf <- lgbm_wf %>%
      fit(data=storeItemTrain)
    
    ## Predict storeItem sales
    preds <- predict(fitted_wf, new_data=storeItemTest) %>%
      bind_cols(storeItemTest) %>%
      rename(sales=.pred) %>%
      select(id, sales)
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}

vroom_write(all_preds, file="submission.csv", delim=",")

