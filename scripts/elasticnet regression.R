# Elastic Net

# Loading package
library(dplyr)
library(glmnet)
library(caret)
# Loading Data
life_clean <- read.csv("data/life_clean.csv")

# remove some categorical variables
df <- life_clean
x <- df %>% select(-c(Life.expectancy))

# Model Building : Elastic Net Regression
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)

# Training ELastic Net Regression model
elastic_model <- train(Life.expectancy ~ .,
                       data = df,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 25,
                       trControl = control)
# Model Prediction
x_hat_pre <- predict(elastic_model, x)

# Multiple R-squared
rsq <- cor(df$Life.expectancy, x_hat_pre)^2
rsq

# Mean Squared Error
mse <- mean((x_hat_pre - df$Life.expectancy)^2)
mse