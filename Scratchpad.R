library(caret)
library(gam)
library(dplyr)
library(ggplot2)
library(doParallel)
library(monomvn)
library(party)

clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(clus)  # this will work on windows

data <- read.csv(file="Ass4Data.csv")
rownames(data) <- data$ID
data$ID <- NULL
#data$BloodType <- NULL

summary(data)

trControl <- trainControl("cv", number = 20, timingSamps = 100)  # shared cross validation specification


method = "xgbLinear"
tuneGrid = expand.grid(nrounds = 100, lambda = c(0.25, 0.5, 1), alpha = seq(0.1, 0.2, 0.3, 0.4), 
                       eta = 10^-4)

method = "gamSpline"
tuneGrid = expand.grid(df = seq(1, 3))

method = "knn"
tuneGrid = expand.grid(k = seq(1, 20))

set.seed(1)
mods <- caret::train(Y ~ ., data = data, method = method, metric = "RMSE",
                     trControl = trControl,
                     tuneGrid = tuneGrid
) 

plot(mods)
print(mods)

mods$bestTune
mods$finalModel$coefficients

zeros <- data.frame(t(rep(0, 19)))
names(zeros) <- names(data)
zeros[1,"Alcohol"] = 1

predict(mods$finalModel, newdata = zeros)
mods$results$RMSESD[6]

c <- cor(data)
corrplot(c)
plot(data$DoseN, data$Y)

ggplot(filter(mods$results, C < 1E8)) +
  geom_line(aes(sigma, RMSE, col=factor(C))) +
  geom_point(aes(sigma, RMSE, col=factor(C))) + 
  scale_x_continuous(trans='log10') +coord_cartesian(ylim = c(10, 60))

filter(mods$results, sigma == 1e-4)

m2 <- train(Y~., data = data, method = "lm", trControl = trControl)

res <- resamples(list(t=mods, l=m2))

stopCluster(clus) # stop the cluster and free memory on app close

