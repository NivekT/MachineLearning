#====================
# Kevin Tse
# 1/12/2017
#====================

library(readr)
library(mosaic)
library(kknn)
library(gridExtra)

#----------------

# Predict used car prices using polynomial regression, then use kNN

UsedCars <- read.csv("C:/UsedCars.csv")

n <- nrow(UsedCars)

cars_training <- sample(UsedCars, floor(n * 0.75), replace = FALSE)
cars_test <- sample(UsedCars, ceiling(n * 0.75), replace = FALSE)

car_lm <- lm(price ~ mileage, data = cars_training)
summary(car_lm)
# price = b_0 + b_1 * mileage + e
# b_0 = 55474.7755, b_1 = -0.3365

car_scatter <- ggplot(data = cars_training, aes(y = price, x = mileage)) +
  geom_point() +
  geom_smooth(method='lm',  fill = NA) + # Linear Regression
  ylim(c(0, NA))


download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R", "docv.R")
source("docv.R")
#--------------------------------------------------
#cv version for polynomial regression
docvpoly = function(x,y,d,nfold=10,doran=TRUE,verbose=TRUE) {
  return(docv(x,y,matrix(d,ncol=1),dopoly,mse,nfold=nfold,doran=doran,verbose=verbose))
}

dopoly=function(x,y,xp,d) {
  train = data.frame(x,y=y)
  test = data.frame(xp); names(test) = names(train)[1:(ncol(train)-1)]
  fit = lm(y~poly(x, degree=d[1]), train)
  return (predict.lm(fit, test))
}
#--------------------------------------------------

# Finding the optimal degree of polynomial
polys <- 1:12 # degrees that we are going to test

# Computing the cvs for each corresponding polynomial degree
cars_cv <- docvpoly(matrix(cars_training$mileage,ncol=1),cars_training$price,polys)
# Extracting the index of the min
optimal_degree <- which.min(cars_cv) 

car_poly <- ggplot(data = cars_training, aes(y = price, x = mileage)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, optimal_degree, raw=TRUE)) +  
  ylim(c(0, NA))