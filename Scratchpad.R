
library(caret)
library(xgboost)
library(dplyr)

data <- read.csv(file="Ass4Data.csv")
rownames(data) <- data$ID
data$ID <- NULL
data$BloodType <- NULL

summary(data)

trControl <- trainControl("cv", number = 5)  # shared cross validation specification

method = "xgbTree"

tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

mods <- caret::train(Y ~ ., data = data, method = method, eval_metric = "rmse",
                     trControl = trControl,
                     tuneGrid = expand.grid(nrounds = 1000, colsample_bytree = .5, min_child_weight = 0, subsample = .5,
                                            max_depth = 2, eta = 0.2, gamma = 0.01)
) 
