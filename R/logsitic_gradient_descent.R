#====================
# Kevin Tse
# Machine Learning 
# 3/2/2017
#====================

library(readr)
library(mosaic)
library(gettingtothebottom)

# Objective function
object_function = function(X, y, b){
  n = nrow(X) ;sum = 0;
  for (i in 1:n) {
    sum = sum + log(1 + exp(as.double(X[i,] %*% b)))
  }
  return(as.double(-t(y) %*% X %*% b) + sum)
}
# Gradient function
grad_function = function(X, y, b){
  # alpha = 0.01 # learning rate
  n = nrow(X)
  p = rep(0,n)
  for (i in 1:n) {
    p[i] = 1/(1+exp(as.double(-X[i,] %*% b)))
  }
  p = as.matrix(p)
  # print(p); #print(typeof(p)); print(dim(p))
  # db = -(t(X) %*% (y - p))
  return(-(t(X) %*% (y - p)))
  # return(b - alpha * db)
}

# Data

train <- read.csv("train.csv")
test <- read.csv("test.csv")

X <- as.matrix(train[-c(1)]) 
y <- as.vector(train$Y) 
# b <- vector(length=ncol(X))
# b <- matrix(nrow=ncol(X), ncol = 1) 

# gdescent returns several values, including trace of parameters in all iterations
# see names of returned values
# names(logistic_gd)


# Run gradient descent
t = 1:30000
lgd0.01 <- gdescent(object_function, grad_function, X, y, alpha = 0.01, iter=2000)
b0.01 = lgd0.01$b[,which.min(lgd0.01$f)]
plot(lgd0.01$f~t[1:2000], main = "Objective Function vs Iteration, a = 0.01", xlab = "Number of Iteration", ylab = "Objective Function")


lgd0.0001 <- gdescent(object_function, grad_function, X, y, alpha = 0.0001, iter=30000)
b0.0001 = lgd0.0001$b[,which.min(lgd0.0001$f)]
plot(lgd0.0001$f[1:21337]~t[1:21337], main = "Objective Function vs Iteration, a = 0.0001", xlab = "Number of Iteration", ylab = "Objective Function")

lgd0.000001 <- gdescent(object_function, grad_function, X, y, alpha = 0.000001, iter=30000)
b0.000001 = lgd0.000001$b[,which.min(lgd0.000001$f)]
plot(lgd0.000001$f~t, main = "Objective Function vs Iteration, a = 0.000001", xlab = "Number of Iteration", ylab = "Objective Function")


# lgd0.00001 <- gdescent(object_function, grad_function, X, y, alpha = 0.00001, iter=50000)

fit <- glm(Y~., data=train,family=binomial())

# Part 4
best_b = as.matrix(b0.0001) # 11 x 1
predictor <- function(Input, b) {
  n = nrow(Input)
  p = rep(0, n)
  for (i in 1:n) {
    p[i] = as.double(t(Input[i,]) %*% b[2:11]) + b[1]
  }
  return(p)
}

test_X <- as.matrix(test[-c(1)]) 
test_y <- as.vector(test$Y)

pre <- predictor(test_X,best_b)
for (i in 1:length(pre)){
  if (pre[i] >= 0)
    pre[i] = 1
  else
    pre[i] = 0
}

mis = 1 - sum(pre == test_y) / 200 # 0.06