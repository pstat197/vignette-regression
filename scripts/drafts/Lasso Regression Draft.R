# LASSO Regression

# Loading package
library(dplyr)
library(glmnet)
library(caret)
library(tidyverse)
library(tidymodels)
# Loading Data

# load cleaned data
# Loading Data
life_clean <- read.csv("data/life_clean.csv")

# Cleaning data
life_clean <- life_clean %>%
  na.omit()

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square)
}
# Model Building : LASSO Regression

# for reproducibility
set.seed(12)

# partition data
partitions <- life_clean %>%
  initial_split(prop = 0.8)

# Training Set
train_x <- training(partitions) %>%
  select(-Life.expectancy) %>%
  as.matrix()
train_y <- training(partitions) %>%
  pull(Life.expectancy)

# Testing Set
test_x <- testing(partitions) %>%
  select(-Life.expectancy) %>%
  as.matrix()
test_y <- testing(partitions) %>%
  pull(Life.expectancy)

# examine
lambdas <- 10^seq(2, -3, by = -.1)

# Perform cross validation to find best value of lambda
lasso_reg <- cv.glmnet(train_x, train_y, alpha = 1, lambda = lambdas)

# Best lambda value
best_lambda <- lasso_reg$lambda.min

lasso_model <- glmnet(train_x, train_y, alpha = 1, lambda = best_lambda)

pred_train <- predict(lasso_model, s = best_lambda, newx = train_x)

mean((pred_train-train_y)^2)

pred_test <- predict(lasso_model, s = best_lambda, newx = test_x)

mean((pred_test-test_y)^2)

lasso.coef=predict(lasso_model,type="coefficients",s=best_lambda)

lasso.coef


