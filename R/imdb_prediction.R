#====================
# Kevin Tse
# 2/1/2017
#====================

library(readr)
library(mosaic)
library(kknn)
library(gridExtra)

#----------------

# Given 5000 IMDB movie reviews, train a model to classifiy them integrate
# Positive (1, review score >= 7) and Negative (1, review score <= 5)
# I implemented a weight adjusted perceptron for this purpose.

# Training
moviereviews_train <- read_csv("MovieReview_train.csv")

len <- length(moviereviews_train)
positive <- moviereviews_train[moviereviews_train$sentiment == 1,] 
negative <- moviereviews_train[moviereviews_train$sentiment == 0,] 

# Finding sums of how often each word appears in positive/negative sentiments
positive_data <- rep(0,len-2)
negative_data <- rep(0,len-2)
for (i in 2:(len-1)) {
  positive_data[i-1] <- sum(positive[,i])
  negative_data[i-1] <- sum(negative[,i])
}

train_result <- rep(0,5000)

weight <- rep(0,len-2)

perceptron <- function(w,data,n) {
  
  # n is size of training, 5000 - n = size of validation
  mistakes = 0
  
  # Shuffle data
  data <- data[sample(nrow(data)),]
  
  for (i in 1:n) { #5000
    
    # Make Prediction
    x = as.numeric(data[i,2:(len-1)])
    actual = data[i,len]
    val = dot(w,x)
    pred = sign(val)
    if (pred == -1) {pred = 0}
    
    train_result[i] <<- pred
    
    # Recompute weight
    if (pred == 0 && actual == 1) {w = w + x; mistakes = mistakes + 1}
    if (pred == 1 && actual == 0) {w = w - x; mistakes = mistakes + 1}
  }
  print(paste("Training Mistakes out of 5000:", mistakes))
  
  # Validation
  # vmistakes = 0
  # for (i in n:5000) {
  #   x = data[i,2:(len-1)]
  #   actual = data[i,len]
  #   pred = sign(dot(w,x))
  #   if (pred == -1) {pred = 0}
  #   if (pred != actual) {vmistakes = vmistakes + 1}
  # }
  # print(paste("Validation Mistakes out of 500:", vmistakes))
  return(as.numeric(w))
}

# set.seed(420)
# weight_4500 <- perceptron(weight, moviereviews_train, 4500)
# weight_9000 <- perceptron(weight_4500, moviereviews_train, 4500)
# weight_13500 <- perceptron(weight_90000, moviereviews_train, 4500)
# weight_18000 <- perceptron(weight_13500, moviereviews_train,4500)
# weight_22500 <- perceptron(weight_18000, moviereviews_train,4500)
# First 4500 times, 1570 mistakes
# Second 4500 times, 1230 mistakes
# Third 4500 times, 1106 mistakes
# Fourth 4500 times, 1076 mistakes
# Fifth 4500 times, 989 mistakes

set.seed(420)
weight_5000 <- perceptron(weight, moviereviews_train,5000)
weight_10000 <- perceptron(weight_5000, moviereviews_train,5000)
weight_15000 <- perceptron(weight_10000, moviereviews_train,5000)
weight_20000 <- perceptron(weight_15000, moviereviews_train,5000)
weight_25000 <- perceptron(weight_20000, moviereviews_train,5000)
weight_30000 <- perceptron(weight_25000, moviereviews_train,5000)
weight_35000 <- perceptron(weight_30000, moviereviews_train,5000)
weight_40000 <- perceptron(weight_35000, moviereviews_train,5000)
weight_45000 <- perceptron(weight_40000, moviereviews_train,5000)
final_weight <- weight_45000

# First 5000 times, 1699 mistakes
# Second 5000 times, 1346 mistakes
# Third 5000 times, 1254 mistakes
# Fourth 5000 times, 1213 mistakes
# Fifth 5000 times, 1165 mistakes
# Sixth 5000 times, 1209 mistakes
# Seventh 5000 times, 1118 mistakes
# Eigth 5000 times, 1158 mistakes
# Ninth 5000 times, 1140 mistakes

# With cross-validation, we have determined that feeding the data to the perceptron
# 9 times give the best result.

adjustment <- (positive_data - negative_data)/(positive_data + negative_data)

adjuster <- function(w, adj) {
  for (i in 1:(len-2)) {
    if (w[i] >= 0) {w[i] <- w[i] * adj[i]}
    else {{w[i] <- w[i] * (-1 * adj[i])}}
  }
  return(w)
}

# This is the weight for perceptron that we will apply onto the test data
adjusted_weight <- adjuster(weight_45000, adjustment) #953

# test_perceptron(adjusted_weight, moviereviews_train)

## Testing the Model 
moviereviews_test <- read_csv("MovieReview_test.csv")

test_result <- rep(5,5000)
test_perceptron <- function(w, data) {
  # Printing it to a file
  sink("hw2-2-sskarste-kevintse-utran.csv")
  cat("\"sentiment\"")
  cat("\n")
  # test_mistakes = 0
  for (i in 1:5000) {
    # x = data[i,2:(len-1)]
    x = as.numeric(data[i,2:ncol(data)])
    # actual = data[i,len]
    pred = sign(dot(w,x))
    if (pred == -1) {pred = 0}
    test_result[i] <<- pred
    cat(pred)
    cat("\n")
    # if (pred != actual) {test_mistakes = test_mistakes + 1}
  }
  # print(paste("Test Mistakes out of 5000:", test_mistakes))
  sink()
}

test_perceptron(adjusted_weight, moviereviews_test)

