#====================
# Kevin Tse
# Chicago Booth
# Machine Learning 
# Project
# 3/4/2017
#====================

# rm(list = ls())
library(readr)
library(mosaic)
library(rpart)
library(rpart.plot)
library(randomForest)

#----------------
data2014 <- read.csv("data2014_filtered_2.csv")
data2015 <- read.csv("data2014_filtered_2.csv")

temp = rpart(FLAG_ENR~., data=data2014, 
             control=rpart.control(minsplit=5,  
                                   cp=0.01,    
                                   xval=00)
)
rpart.plot(temp)

#----------------
# Constructing example students

example = data2014[1,]
example$REGION[1] = names(which.max(table(data2014$REGION))) # MW
example$GENDER[1] = names(which.max(table(data2014$GENDER))) # F
example$ETHNIC_CD[1] = names(which.max(table(data2014$ETHNIC_CD))) # White
example$DATE_APP = median(as.Date(data2014$DATE_APP)) # 2013-12-26
example$FLAG_ROSTER_ATHLETE[1] = names(which.max(table(data2014$FLAG_ROSTER_ATHLETE))) #N
example$MAJOR1_ADMIT[1] = names(which.max(table(data2014$MAJOR1_ADMIT))) #UND
example$HSGPA[1] = median(data2014$HSGPA) # 3.573
example$HSGPASCALE[1] = median(data2014$HSGPASCALE) # 4
example$PARENT_INCOME[1] = median(data2014$PARENT_INCOME) # 80269
example$STUDENT_INCOME[1] = median(data2014$STUDENT_INCOME) # 0
example$TOTAL_FAFSA_POSITION[1] = median(data2014$TOTAL_FAFSA_POSITION) # 6.827193
example$BUDGET[1] = median(data2014$BUDGET)
example$ROOM_BOARD[1] = median(data2014$ROOM_BOARD)
example$TEST_COMP[1] = median(data2014$TEST_COMP)
example$SS = 0
example$FLAG_ENR = NULL

example[1:11,] = example[1,]
# example$GENDER[1:11] = "F"

#----------------
ex_ss = example
ex_ss$SS[1:11] = seq(0,50000,5000)
# ex_ss$SS[12:22] = seq(0,50000,5000)

#----------------
# Varying test scores
ts = seq(18,36,2)
test = ex_ss
test$TEST_COMP = 18

for (i in 2:length(ts)) {
  temp = ex_ss
  temp$TEST_COMP = ts[i]
  test = rbind(test,temp)
}

write.csv(test,"test_score.csv")
#----------------
# Varying family income
fi = c(0,27764,45745.8,65193.3,80269,96682.1,101700.28,131404.87,179682.8,313388.42)
fam_inc = ex_ss
fam_inc$PARENT_INCOME = 0

for (i in 2:length(fi)) {
  temp = ex_ss
  temp$PARENT_INCOME = fi[i]
  fam_inc = rbind(fam_inc,temp)
}

write.csv(fam_inc,"parent_income.csv")
#----------------
# Varying Region
reg = ex_ss[1:11,]
reg = rbind(reg,ex_ss[1:11,])
reg$REGION[12:22] = "W"
reg = rbind(reg,ex_ss[1:11,])
reg$REGION[23:33] = "SE"
reg = rbind(reg,ex_ss[1:11,])
reg$REGION[34:44] = "SW"
reg = rbind(reg,ex_ss[1:11,])
reg$REGION[45:55] = "NE"

write.csv(reg,"region.csv")

#----------------
# Varying Ethnicity
ethnic = ex_ss[1:11,]
ethnic = rbind(ethnic,ex_ss[1:11,])
ethnic$ETHNIC_CD[12:22] = "BLACK"
ethnic = rbind(ethnic,ex_ss[1:11,])
ethnic$ETHNIC_CD[23:33] = "HISPAN"
ethnic = rbind(ethnic,ex_ss[1:11,])
ethnic$ETHNIC_CD[34:44] = "ASIAN"

write.csv(ethnic,"ethnic.csv")
