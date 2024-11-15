#### STORE ITEM DEMAND ####
library(tidyverse)
library(vroom)
library(patchwork)
library(forecast)

#### READ IN THE DATA ####
test = vroom("test.csv")
train = vroom("train.csv")

#### EDA ####
glimpse(train)

#### COMBO OF STORE-ITEM PLOTS ####
## Filter to 1 store & item
storeItem1 <- train %>%
  filter(store==8, item==25)

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
