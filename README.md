# Kaggle Store Item Demand Forecasting Challenge
This repository contains the R script used for my participation in the Kaggle Store Item Demand Forecasting Challenge. The goal of the competition was to predict the sales of different items in various stores over a three-month period.

This project aims to develop a predictive model for forecasting the demand for store items. Accurate demand forecasting helps in inventory management, reducing stockouts and overstock situations, and improving overall operational efficiency.

## Dataset
The dataset used in this project is provided by Kaggle and includes the following features:

**date:** Date of the sales data

**store:** Store ID

**item:** Item ID

**sales:** Number of items sold (target variable)

The dataset spans five years of daily sales data for 50 different items across 10 different stores.

## Model
The model with the best results was a LightGBM model. I tuned the parameters on only stores 1-3 and items 1-3, and then applied those tuning parameters to the rest of the dataset. (All tuning can be seen in TuneLGBM.R). 

To view all models that I experimented with for this Kaggle competition, check the git history for the ItemDemand.R file.
