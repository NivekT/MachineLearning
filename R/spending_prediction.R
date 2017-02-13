#====================
# Kevin Tse
# 2/12/2017
#====================

library(readr)
library(mosaic)
library(kknn)
library(ROCR)
library(gridExtra)
library(rpart)    
library(rpart.plot)
library(randomForest)

#====================
# Use randomForest to predict purchase behavior

Tayko <- read_csv("Tayko.csv")


# Purchase Decision Prediction

train = Tayko[Tayko$Partition == "t",2:24]
valid = Tayko[Tayko$Partition == "v",2:24]
test = Tayko[Tayko$Partition == "s",2:24]

# purchase_tree = rpart(Purchase~., data=train, 
#              control=rpart.control(minsplit=5, 
#                                    cp=0.01,    
#                                    xval=0)     
# )
# rpart.plot(temp)

# Using randomForest

set.seed(402)
fit <- randomForest(Purchase~., data=train)

purchase_pred <- predict(fit, valid) # In Probabilities
# Put them into classification
purchase_pred[purchase_pred < 0.5] = 0 
purchase_pred[purchase_pred >= 0.5] = 1
correct_rate <-sum(purchase_pred == valid$Purchase)/length(valid$Purchase) # 0.8071429
 

# ROC Curve

# Takes in prediction, actual
fv2 = predict(fit, valid)
pred2 = prediction(as.numeric(fv2), as.numeric(valid$Purchase))
perf2 = performance(pred2, measure = "tpr", x.measure = "rpp")

roc_RF <- qplot(unlist(perf2@x.values),unlist(perf2@y.values),
                xlim = c(0,1),  ylim = c(0,1.05),
                main = "ROC Curve for Random Forest", xlab = "FPR", ylab = "TPR") +
  geom_abline(slope = 1, intercept = 0)

ggsave("roc_RF.png", plot = roc_RF)

# Spending Prediction

pur = Tayko[Tayko$Purchase == 1,]
pur_train = pur[pur$Partition == "t",c(2:23,25)]
pur_valid = pur[pur$Partition == "v",c(2:23,25)]
pur_test = pur[pur$Partition == "s",c(2:23,25)]

set.seed(402)
spending_fit <- randomForest(Spending~., data=pur_train)
spending_pred <- predict(spending_fit , pur_valid)
# Standard Error
se_spending <- sd(spending_pred - pur_valid$Spending)/sqrt(nrow(pur_valid)) # 8.813012

# Find expected spending

pur_pred_test <- predict(fit, test)
# pur_pred_test[pur_pred_test < 0.5] = 0
# pur_pred_test[pur_pred_test < 0.5] = 0
sp_pred_test <- predict(spending_fit , test)

expected_spending <- sp_pred_test * pur_pred_test * 0.107
avg_spend <- mean(expected_spending)

# Cumulative spending in ascending order
cs = cumsum(expected_spending[order(-expected_spending)])
es_plot <- qplot(1:500,cs, main = "Cumulative Expected Spending vs Numer of Customers", 
      xlab="Customers (Sorted by Expected Spending)",
      ylab="Cumulative Expected Spending") +
  geom_abline(slope = avg_spend)

ggsave("es_plot.png", plot = es_plot)

# Expected spending if we were to target 180,000 people

# Sum of expected spending of top 18 people
top_es <- sum(expected_spending[order(-expected_spending)][1:18]) # 925.309
grossprofit <- 925.309 * 10000