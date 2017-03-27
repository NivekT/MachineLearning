#====================
# Kevin Tse
# Chicago Booth
# Machine Learning 
# Project
# 3/12/2017
#====================
pparent <- read.csv("parent_income_w_pred.csv")
ptest<- read.csv("test_score_w_pred.csv")
pregion <- read.csv("region_w_pred.csv") 
pethnic <- read.csv("ethnic_w_pred.csv")

#-----------

for (i in 1:nrow(ptest)) {
  ptest$Test_Score[i] = toString(ptest$TEST_COMP[i])
}

ptest2 = ptest[ptest$TEST_COMP <= 28,]
for (i in 1:nrow(ptest2)) {
  ptest2$Y[i] = ptest2$Y[i] - (ptest$TEST_COMP[i] * 2 /(36 * 10))
}


test_plot <- ggplot(data = ptest2,
                    aes(x = SS, y = Y, colour = Test_Score)) +
             geom_line() +
             ylab("Enrollment Probability") +
             xlab("Amount of Scholarship Offered by College") + xlim(0,25000) + 
             ggtitle("Enrollment Probability vs Scholarship Offered with Varying Test Scores")

ggsave("test.jpg",test_plot)
#-----------
ethnic_plot <- ggplot(data = pethnic,
                    aes(x = SS, y = Y, colour = ETHNIC_CD)) +
              geom_line() +
              ylab("Enrollment Probability") +
              xlab("Amount of Scholarship Offered by College") + xlim(0,25000) + 
              ggtitle("Enrollment Probability vs Scholarship Offered with Different Ethnicities")
ggsave("ethnic.jpg",ethnic_plot)
#-----------
pparent2 = pparent
for (i in 1:nrow(ptest2)) {
  pparent2$Parent_Income[i] = toString(pparent2$PARENT_INCOME[i])
}


parent_plot <- ggplot(data = pparent2,
                      aes(x = SS, y = Y, colour = Parent_Income)) +
                      geom_line() +
                      ylab("Enrollment Probability") +
                      xlab("Amount of Scholarship Offered by College") + xlim(0,25000) + 
                      ggtitle("Enrollment Probability vs Scholarship Offered with Varying Parent Income")

#-----------
pregion2 = pregion[pregion$REGION == "MW" ,]
pregion2 = rbind(pregion2, pregion[pregion$REGION == "W" ,])
pregion2$Region = pregion2$REGION

pregion$Region = pregion$REGION
region_plot <- ggplot(data = pregion,
                      aes(x = SS, y = Y, colour = Region)) +
                      geom_line() +
                      ylab("Enrollment Probability") +
                      xlab("Amount of Scholarship Offered by College") + xlim(0,25000) + 
                      ggtitle("Enrollment Probability vs Scholarship Offered with Geographical Regions")

ggsave("region.jpg",region_plot)
