library(readr)
library(caret)
library(dplyr)

df <- read.csv("heart_failure.csv",header = T)
head(df)
glimpse(df)

df$DEATH_EVENT <- factor(df$DEATH_EVENT, levels=c(0,1), labels=c("No", "Yes"))

#Split
Splitdata <- function(data,n=0.8){
  nrow <- nrow(data)
  id <- sample(1:nrow, size = n*nrow)
  train_data <- data[id,]
  test_data <- data[-id,]
  return(list(train_data,test_data))
}

splitdf <- Splitdata(df,0.7)
nrow(splitdf[[1]]);nrow(splitdf[[2]])

#train + test
set.seed(42)
crtl <- trainControl(method="cv",
                     number = 5,
                     verboseIter = T)

#glm_model
glm_model <- train(DEATH_EVENT~.,
                   data = splitdf[[1]],
                   method = "glm",
                   trControl = crtl)
glm_p <- predict(glm_model, newdata = splitdf[[2]])

#knn_model
knn_model <- train(DEATH_EVENT~.,
                   data = splitdf[[1]],
                   method = "knn",
                   trControl = crtl)
knn_p <- predict(knn_model, newdata = splitdf[[2]])

#rf_model
rf_model <- train(DEATH_EVENT~.,
                   data = splitdf[[1]],
                   method = "rf",
                   trControl = crtl)
rf_p <- predict(rf_model, newdata = splitdf[[2]])

#score
list_models <- list(
  logistic = glm_model,
  knn = knn_model,
  ramdomforst = rf_model)

result <- resamples(list_models)
summary(result)

mean(glm_p == splitdf[[2]]$DEATH_EVENT)
mean(knn_p == splitdf[[2]]$DEATH_EVENT)
mean(rf_p == splitdf[[2]]$DEATH_EVENT)

#save model
saveRDS(glm_model,"heart_failure_logistic_model.RDS")
saveRDS(knn_model,"heart_failure_knn_model.RDS")
saveRDS(rf_model,"heart_failure_rainforest_model.RDS")
