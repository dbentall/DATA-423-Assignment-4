
library(caret)
library(kernlab)
library(dplyr)

data <- read.csv(file="Ass4Data.csv")
rownames(data) <- data$ID
data$ID <- NULL
#data$BloodType <- NULL

summary(data)

trControl <- trainControl("cv", number = 5, timingSamps = 100)  # shared cross validation specification

method = "svmRadial"

tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

mods <- caret::train(Y ~ ., data = data, method = method, metric = "RMSE",
                     trControl = trControl,
                     tuneGrid = expand.grid(C = 10^seq(4,6), sigma = 10^seq(-2,-10))
) 
plot(mods, log = 'Sigma')
m2 <- train(Y~., data = data, method = "lm", trControl = trControl)

res <- resamples(list(t=mods, l=m2))
cat("Model processing times (s):")
print(select(res$timings, Training=FinalModel, Predicting=Prediction))
print(mods)
