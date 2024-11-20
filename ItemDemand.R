#### STORE ITEM DEMAND ####
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(modeltime)
library(timetk)

#### READ IN THE DATA ####
test = vroom("test.csv")
train = vroom("train.csv")

#### EDA ####
glimpse(train)

#### COMBO OF STORE-ITEM PLOTS ####

## Time Series Plot
ts1 = storeItem1 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

## ACF Plot 1
acf1 = storeItem1 %>%
  pull(sales) %>% 
  forecast::ggAcf(.)

## ACF Plot 2
acf2 = storeItem1 %>%
  pull(sales) %>% 
  forecast::ggAcf(., lag.max=2*365)


## Filter to 1 store & item
storeItem2 <- train %>%
  filter(store==5, item==35)

## Time Series Plot
ts2 = storeItem2 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

## ACF Plot 1
acf3 = storeItem2 %>%
  pull(sales) %>% 
  forecast::ggAcf(.)

## ACF Plot 2
acf4 = storeItem2 %>%
  pull(sales) %>% 
  forecast::ggAcf(., lag.max=2*365)

#### PATCHWORK PLOTS ####
(ts1 + acf1 + acf2) / (ts2 + acf3 + acf4)


#### FEATURE ENGINEERING ####
storeItem <- train %>%
  filter(store==8, item==25)

store_recipe = recipe(sales~., storeItem) %>% 
  step_date(date, features="dow") %>% 
  step_date(date, features="month") %>% 
  #step_date(date, features="doy") %>%                 
  step_date(date, features="decimal") %>% 
  step_mutate(date_decimal=as.numeric(date_decimal)) %>% 
  step_mutate_at(date_dow, fn=factor) %>% 
  step_mutate_at(date_month, fn=factor) %>% 
  #step_range(date_doy, min=0, max=pi) %>%
  #step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  #step_lag(sales, lag=7) %>% 
  step_rm(c(store, item))

prepped_recipe = prep(store_recipe)
baked = bake(prepped_recipe, new_data=train)

#### RANDOM FOREST MODEL ####
forest_model <- rand_forest(mtry=tune(),
                          min_n=tune(),
                          trees=500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

forest_wf <- workflow() %>%
  add_recipe(store_recipe) %>%
  add_model(forest_model)

# Grid of values to tune over
grid_of_tuning_params <- grid_regular(mtry(range = c(1,10)),
                                      min_n(),
                                      levels = 3) 

## Split data for CV
folds <- vfold_cv(storeItem, v=5, repeats=2)

# Run the CV1
CV_results <- forest_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(smape))

# Find Best Tuning Parameters
best_tune <- CV_results %>%
  show_best(metric="smape", n=1)
best_tune
