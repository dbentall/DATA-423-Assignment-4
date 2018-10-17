
library(caret)
library(kernlab)
library(dplyr)
library(ggplot2)

data <- read.csv(file="Ass4Data.csv")
rownames(data) <- data$ID
data$ID <- NULL
#data$BloodType <- NULL

summary(data)

trControl <- trainControl("cv", number = 10, timingSamps = 100)  # shared cross validation specification

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
                     tuneGrid = expand.grid(C = 10^seq(4,7), sigma = 10^seq(-2,-5))
) 

mods$bestTune

ggplot(filter(mods$results, C < 1E8)) +
  geom_line(aes(sigma, RMSE, col=factor(C))) +
  geom_point(aes(sigma, RMSE, col=factor(C))) + 
  scale_x_continuous(trans='log10') +coord_cartesian(ylim = c(10, 60))

filter(mods$results, sigma == 1e-4)

m2 <- train(Y~., data = data, method = "lm", trControl = trControl)

res <- resamples(list(t=mods, l=m2))

