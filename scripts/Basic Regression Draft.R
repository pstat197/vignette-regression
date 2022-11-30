life_clean <- read.csv("data/life_clean.csv")

set.seed(13)
# We will use a simple linear regression as our basis for comparing all other models
base_reg <- lm(Life.expectancy ~ ., data = life_clean)
summary(base_reg)
