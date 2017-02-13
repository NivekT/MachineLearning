#====================
# Kevin Tse
# 1/12/2017
#====================

library(readr)
library(mosaic)
library(kknn)
library(ROCR)
library(gridExtra)
library(rpart)    
library(rpart.plot)
library(randomForest)

#----------------
# Teach a model to classify the type of crime in Philadelphia based on their records.

PhillyCrime <- read_csv("PhillyCrime.csv") # from https://www.opendataphilly.org/dataset/crime-incidents

# Scatter Plots
vandal <- PhillyCrime[PhillyCrime$Category == "Vandalism",]
theft <- PhillyCrime[PhillyCrime$Category == "Thefts",]

vandal_plot <- qplot(vandal$X, vandal$Y, size=I(0.3), 
      xlab="Latitude", ylab="Longitude",
      main = "Location of Vandalism in Phily")

theft_plot <- qplot(theft$X, theft$Y, size=I(0.3), 
      xlab="Latitude", ylab="Longitude",
      main = "Location of Thefts in Phily")


ggsave("vandal_plot.png", plot = vandal_plot)
ggsave("theft_plot.png", plot = theft_plot)



y <- PhillyCrime$Category
xs <- cbind(PhillyCrime$X , PhillyCrime$Y)
phi_df <- data.frame(y,xs)
colnames(phi_df) <- c("Categories","X", "Y")

# Split the data in two
set.seed(100)
ind <- sample(seq_len(nrow(phi_df)), size = 0.5*nrow(phi_df))
 
phi_train <- phi_df[ind,]
phi_valid <- phi_df[-ind,]


miss_rate = rep(0,100)
for (i in 1:100) {
  near = kknn(Categories~.,phi_train,phi_valid,k=i,kernel = "rectangular")  
  miss_rate[i] = sum(near$fitted.values != phi_valid$Categories) / nrow(phi_valid)
}

misclass_plot <- qplot(1:100, miss_rate, xlab = "k", ylab = "Misclassification Rate", 
      main = "Misclassification rate vs k")

ggsave("misclass_plot.png", plot = misclass_plot)

k_best = which.min(miss_rate) # 11
min_miss = min(miss_rate) # 0.3171


knn_best = kknn(Categories~.,phi_train,phi_valid,k=k_best,kernel = "rectangular") 

kbest_plot <- qplot(phi_valid$X, phi_valid$Y, color = knn_best$fitted.value, size=I(0.3), 
      xlab="Latitude", ylab="Longitude",
      main = "Location of Thefts and Vandalism in Phily")

ggsave("kbest_plot.png", plot = kbest_plot)

# Repeat the above 20 times to see other possible k values and their performance

k_optimal = rep(0,20)
min_20 = rep(0,20)

# From the first example we see that higher k definitely will not return min

set.seed(100)
for (t in 1:20) {
  index <- sample(seq_len(nrow(phi_df)), size = 0.5*nrow(phi_df))
  phi_train <- phi_df[index,]
  phi_valid <- phi_df[-index,]
  miss_rate = rep(0,15)
  for (i in 2:20) { 
    near = kknn(Categories~.,phi_train,phi_valid,k=i,kernel = "rectangular")  
    miss_rate[i-1] = sum(near$fitted.values != phi_valid$Categories) / nrow(phi_valid)
  }
  k_optimal[t] = which.min(miss_rate)+ 1
  min_20[t] = min(miss_rate) 
}


k_optimal # 11  7  7 12  6 10  9 10  7  7  7 18 16 13  8  5 14  6 11 10
mean(min_20) # 0.31447
sd(min_20) / sqrt(20) # standard error # 0.0007317283


# 90/10 split

set.seed(100)
index <- sample(seq_len(nrow(phi_df)), size = 0.9*nrow(phi_df))
phi_train <- phi_df[index,]
phi_valid <- phi_df[-index,]
miss_rate2 = rep(0,100)
for (i in 1:100) { 
  near = kknn(Categories~.,phi_train,phi_valid,k=i,kernel = "rectangular")  
  miss_rate2[i] = sum(near$fitted.values != phi_valid$Categories) / nrow(phi_valid)
}


# Misclassification Plot
misclass_plot2 <- qplot(1:100, miss_rate2, xlab = "k", ylab = "Misclassification Rate", 
                        main = "Misclassification rate vs k, 90/10 split")

ggsave("misclass_plot2.png", plot = misclass_plot2)

k_best_2 = which.min(miss_rate2) # 18
min_miss_2 = min(miss_rate2) #0.295


knn_best2 = kknn(Categories~.,phi_train,phi_valid,k=k_best_2,kernel = "rectangular") 

kbest2_plot <- qplot(phi_valid$X, phi_valid$Y, color = knn_best2$fitted.value, size=I(0.3), 
                    xlab="Latitude", ylab="Longitude",
                    main = "Location of Thefts and Vandalism in Phily, with 90/10 Split in Data")

ggsave("kbest_plot2.png", plot = kbest2_plot)

# Repeat 20 times

k_optimal_2 = rep(0,20)
min_20_2 = rep(0,20)

set.seed(100)
for (t in 1:20) {
  index <- sample(seq_len(nrow(phi_df)), size = 0.9*nrow(phi_df))
  phi_train <- phi_df[index,]
  phi_valid <- phi_df[-index,]
  miss_rate2 = rep(0,22)
  for (i in 6:24) { 
    near = kknn(Categories~.,phi_train,phi_valid,k=i,kernel = "rectangular")  
    miss_rate2[i-6] = sum(near$fitted.values != phi_valid$Categories) / nrow(phi_valid)
  }
  k_optimal_2[t] = which.min(miss_rate2) + 6
  min_20_2[t] = min(miss_rate2)
}


k_optimal_2 # 18 11 12 12 23 15 9 11 22 13 14  9  9 16 11 20 17 10 17  7
mean(min_20_2) # 0.298225
sd(min_20_2) / sqrt(20) # standard error # 0.001917878

# 5. 

# Done in writeout.

# 6.
set.seed(100)
ind <- sample(seq_len(nrow(phi_df)), size = 0.5*nrow(phi_df))
phi_train <- phi_df[ind,]
phi_valid <- phi_df[-ind,]
near_25 = kknn(Categories~.,phi_train,phi_valid,k=i,kernel = "rectangular")  

# Takes in prediction, actual
fv = predict(near_25, phi_valid, type="prob")
pred = prediction(fv[,2], as.numeric(phi_valid$Categories))
perf = performance(pred, measure = "tpr", x.measure = "rpp")

# Creating a ROC Curve

roc_25 <- qplot(unlist(perf@x.values),unlist(perf@y.values),
      xlim = c(0,1),  ylim = c(0,1.05),
      main = "ROC Curve for k = 25", xlab = "FPR", ylab = "TPR") +
  geom_smooth() +
  geom_abline(slope = 1, intercept = 0)

ggsave("roc_25.png", plot = roc_25)