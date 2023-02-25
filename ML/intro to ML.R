
library(caret)


head(mtcars)

set.seed(49)
n <- nrow(mtcars)
(id <- sample(n, size=0.8*n))
train_data <- mtcars[id,] #choose row at id every col
test_data <- mtcars[-id,] #choose row at -id every col

train_test_split <- function(data){
  set.seed(49)
  n <- nrow(data)
  id <- sample(n, size=0.8*n)
  train_data <- data[id,]
  test_data <- data[-id,]
  return(list(train_data,test_data))
}

split_dat <- train_test_split(mtcars) ##split_dat collect sth that return from train_test_split >> list with 2 var

split_dat[[2]]

#train (mpg~hp // mpg==y)

lm_model <- train(mpg ~ hp,
                  data = split_dat[[1]],
                  method = "lm")
#test and evaluate
p <- predict(lm_model,newdata = split_dat[[2]])
p
class(p)
class(split_dat[[2]]$mpg)


error <- split_dat[[2]]$mpg - p
rms <- sqrt(mean(error**2))


#glm example
data(mtcars)

mtcars$am <- factor(mtcars$am,
                    levels = c(0,1),
                    labels = c("auto","manual"))

Split2 <- function(data){
  set.seed(7)
  n2 <- nrow(data)
  id2 <- sample(n2,size = 0.7*n2)
  train2 <- data[id2,]
  test2 <-data[-id2,]
  return(list(train2,test2))
}

data_split2 <- Split2(mtcars)

glm_model <- train(am ~ mpg, 
                   data = data_split2[[1]], 
                   method="glm")

p2 <- predict(glm_model, data_split2[[2]])

acc <- mean(p2 == data_split2[[2]]$am)
acc
glm_model


