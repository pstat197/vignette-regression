# Brian Che's scratch work for Ridge Regression

# Set Up
# load libraries/packages
library(tidyverse)
library(tidymodels)
library(modelr)
library(rsample)
library(yardstick)
library(DataExplorer)
library(Matrix)
library(glmnet)
library(rsample)
library(dplyr)

# read in csv file
life_clean <- read.csv("data/life_clean.csv")

# using glimpse, we can see that all the variables are numerical consisting of int and dbl
glimpse(life_clean)

# print the dimensions of dataset
dim(life_clean)

# Checking the model
summary(life_clean)

# set seed
set.seed(13)

# partitioning data into training and test set
partitions <- life_clean %>%
  initial_split(prop = 0.8)

# creating training set
train <- training(partitions)

train_x <- training(partitions) %>%
  select(-Life.expectancy) %>%
  as.matrix()
train_y <- training(partitions) %>%
  pull(Life.expectancy)

# creating test set
test <- testing(partitions)

test_x <- testing(partitions) %>%
  select(-Life.expectancy) %>%
  as.matrix()
test_y <- testing(partitions) %>%
  pull(Life.expectancy)

# Setting the range of lambda values
lambda_seq <- 10^seq(2, -3, by = -.1)

# Using glmnet function to build the ridge regression in r
fit <- glmnet(train_x, train_y, alpha = 0, lambda  = lambda_seq)

# Checking the model
summary(fit)

# Using cross validation glmnet
ridge_cv <- cv.glmnet(train_x, train_y, alpha = 0, lambda = lambda_seq)

# Acquire best lambda value
best_lambda <- ridge_cv$lambda.min
best_lambda

# extract the best model using K-cross validation
best_fit <- ridge_cv$glmnet.fit

# Rebuilding the model with optimal lambda value 0.01
best_ridge <- glmnet(train_x, train_y, alpha = 0, lambda = 0.01)

# Checking the coefficients
coef(best_ridge)

# apply prediction model to test_x
pred <- predict(best_ridge, s = best_lambda, newx = test_x)

# use prediction function and R squared formula to compute R^2 value
actual <- test_y
rss <- sum((pred - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq # 0.8278766
