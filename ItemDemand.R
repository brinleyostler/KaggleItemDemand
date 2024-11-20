#### STORE ITEM DEMAND ####
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(modeltime)
library(timetk)
library(plotly)

#### READ IN THE DATA ####
test = vroom("test.csv")
train = vroom("train.csv")

## Filter to 1 store & item
storeItemTrain1 <- train %>%
  filter(store==8, item==25)
storeItemTest1 <- test %>% 
  filter(store==8, item==25)

storeItemTrain2 <- train %>%
  filter(store==5, item==35)
storeItemTest2 <- test %>% 
  filter(store==5, item==35)

## CV SPLIT
cv_split1 <- time_series_split(storeItemTrain1, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeItemTrain2, assess="3 months", cumulative = TRUE)


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

## PATCHWORK PLOTS ##
(ts1 + acf1 + acf2) / (ts2 + acf3 + acf4)


#### FEATURE ENGINEERING ####
arima_recipe1 = recipe(sales~., storeItemTrain1) %>% 
  step_date(date, features=c("dow", "month", "doy", "decimal")) %>%                 
  step_mutate(date_decimal=as.numeric(date_decimal)) %>% 
  step_mutate_at(date_dow, fn=factor) %>% 
  step_mutate_at(date_month, fn=factor) %>% 
  #step_range(date_doy, min=0, max=pi) %>%
  #step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  #step_lag(sales, lag=7) %>% 
  step_rm(c(store, item, date_doy))

arima_recipe2 = recipe(sales~., storeItemTrain2) %>% 
  step_date(date, features=c("dow", "month", "doy", "decimal")) %>%                 
  step_mutate(date_decimal=as.numeric(date_decimal)) %>% 
  step_mutate_at(date_dow, fn=factor) %>% 
  step_mutate_at(date_month, fn=factor) %>% 
  #step_range(date_doy, min=0, max=pi) %>%
  #step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  #step_lag(sales, lag=7) %>% 
  step_rm(c(store, item, date_doy))

prepped1 = prep(arima_recipe1)
prepped2 = prep(arima_recipe2)
baked1 = bake(prepped1, new_data=storeItemTrain1)
baked2 = bake(prepped2, new_data=storeItemTrain2)

#### ARIMA MODEL ####
arima_model <- arima_reg() %>%
  set_engine("auto_arima")


#### CV ####
## Split data
cv_split1 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)
cv_split2 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


#### WORKFLOW ####
arima_wf1 <- workflow() %>%
  add_recipe(arima_recipe1) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split1))

arima_wf2 <- workflow() %>%
  add_recipe(arima_recipe2) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split2))

# Run the CV
cv_results1 <- modeltime_calibrate(arima_wf1,
                                  new_data = testing(cv_split1))
cv_results2 <- modeltime_calibrate(arima_wf2,
                                   new_data = testing(cv_split2))

## Visualize results
p1 = cv_results1 %>%
  modeltime_forecast(
    new_data = testing(cv_split1),
    actual_data = storeItemTrain1) %>%
  plot_modeltime_forecast(.interactive=TRUE)
p2 = cv_results2 %>%
  modeltime_forecast(
    new_data = testing(cv_split2),
    actual_data = storeItemTrain2) %>%
  plot_modeltime_forecast(.interactive=TRUE)


## Evaluate the accuracy
cv_results1 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE)
cv_results2 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE)

## Refit to whole data
fullfit1 <- cv_results1 %>%
  modeltime_refit(data = storeItemTrain1)
fullfit2 <- cv_results2 %>%
  modeltime_refit(data = storeItemTrain2)

p3 = fullfit1 %>%
  modeltime_forecast(
    new_data = storeItemTest1,
    actual_data = storeItemTrain1) %>%
  plot_modeltime_forecast(.interactive=TRUE)
p4 = fullfit2 %>%
  modeltime_forecast(
    new_data = storeItemTest2,
    actual_data = storeItemTrain2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## FOUR PLOTS
plotly::subplot(p1, p3, p2, p4, nrows=2)
