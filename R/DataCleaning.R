#====================
# Kevin Tse
# Chicago Booth
# Machine Learning 
# Project
# 3/2/2017
#====================

# rm(list = ls())
library(readr)
library(mosaic)
library(gdata)


# Reading Data and Regional Aggregation 
### Uploading the price beliefs data
data2014 = read_excel("full_data_2014.xlsx")
data2014$REGION[data2014$STATE == "AL"]<- "SE"
data2014$REGION[data2014$STATE == "AK"]<- "W"
data2014$REGION[data2014$STATE == "AZ"]<- "W"
data2014$REGION[data2014$STATE == "AR"]<- "SE"
data2014$REGION[data2014$STATE == "CA"]<- "W"
data2014$REGION[data2014$STATE == "CO"]<- "W"
data2014$REGION[data2014$STATE == "CT"]<- "NE"
data2014$REGION[data2014$STATE == "DE"]<- "NE"
data2014$REGION[data2014$STATE == "FL"]<- "SE"
data2014$REGION[data2014$STATE == "GA"]<- "SE"
data2014$REGION[data2014$STATE == "HI"]<- "W"
data2014$REGION[data2014$STATE == "ID"]<- "W"
data2014$REGION[data2014$STATE == "IL"]<- "MW"
data2014$REGION[data2014$STATE == "IN"]<- "MW"
data2014$REGION[data2014$STATE == "IA"]<- "MW"
data2014$REGION[data2014$STATE == "KS"]<- "MW"
data2014$REGION[data2014$STATE == "KY"]<- "SE"
data2014$REGION[data2014$STATE == "LA"]<- "SE"
data2014$REGION[data2014$STATE == "ME"]<- "NE"
data2014$REGION[data2014$STATE == "MD"]<- "NE"
data2014$REGION[data2014$STATE == "MA"]<- "NE"
data2014$REGION[data2014$STATE == "MI"]<- "MW"
data2014$REGION[data2014$STATE == "MN"]<- "MW"
data2014$REGION[data2014$STATE == "MS"]<- "SE"
data2014$REGION[data2014$STATE == "MO"]<- "MW"
data2014$REGION[data2014$STATE == "MT"]<- "W"
data2014$REGION[data2014$STATE == "NE"]<- "MW"
data2014$REGION[data2014$STATE == "NV"]<- "W"
data2014$REGION[data2014$STATE == "NH"]<- "NE"
data2014$REGION[data2014$STATE == "NJ"]<- "NE"
data2014$REGION[data2014$STATE == "NM"]<- "SW"
data2014$REGION[data2014$STATE == "NY"]<- "NE"
data2014$REGION[data2014$STATE == "NC"]<- "SE"
data2014$REGION[data2014$STATE == "ND"]<- "MW"
data2014$REGION[data2014$STATE == "OH"]<- "MW"
data2014$REGION[data2014$STATE == "OK"]<- "SW"
data2014$REGION[data2014$STATE == "OR"]<- "W"
data2014$REGION[data2014$STATE == "PA"]<- "NE"
data2014$REGION[data2014$STATE == "RI"]<- "NE"
data2014$REGION[data2014$STATE == "SC"]<- "SE"
data2014$REGION[data2014$STATE == "SD"]<- "MW"
data2014$REGION[data2014$STATE == "TN"]<- "SE"
data2014$REGION[data2014$STATE == "TX"]<- "SW"
data2014$REGION[data2014$STATE == "UT"]<- "W"
data2014$REGION[data2014$STATE == "VT"]<- "NE"
data2014$REGION[data2014$STATE == "VA"]<- "SE"
data2014$REGION[data2014$STATE == "WA"]<- "W"
data2014$REGION[data2014$STATE == "WV"]<- "SE"
data2014$REGION[data2014$STATE == "WI"]<- "MW"
data2014$REGION[data2014$STATE == "WY"]<- "W"
data2014<-data2014[-(which(is.na(data2014$REGION))),] # Removing International Student
data2014<-data2014[-(which(data2014$ENTRY_STAT=="C")),] # Removing Part Time Student
         

data2015 = read_excel("full_data_2015.xlsx")
data2015$REGION[data2015$STATE == "AL"]<- "SE"
data2015$REGION[data2015$STATE == "AK"]<- "W"
data2015$REGION[data2015$STATE == "AZ"]<- "W"
data2015$REGION[data2015$STATE == "AR"]<- "SE"
data2015$REGION[data2015$STATE == "CA"]<- "W"
data2015$REGION[data2015$STATE == "CO"]<- "W"
data2015$REGION[data2015$STATE == "CT"]<- "NE"
data2015$REGION[data2015$STATE == "DE"]<- "NE"
data2015$REGION[data2015$STATE == "FL"]<- "SE"
data2015$REGION[data2015$STATE == "GA"]<- "SE"
data2015$REGION[data2015$STATE == "HI"]<- "W"
data2015$REGION[data2015$STATE == "ID"]<- "W"
data2015$REGION[data2015$STATE == "IL"]<- "MW"
data2015$REGION[data2015$STATE == "IN"]<- "MW"
data2015$REGION[data2015$STATE == "IA"]<- "MW"
data2015$REGION[data2015$STATE == "KS"]<- "MW"
data2015$REGION[data2015$STATE == "KY"]<- "SE"
data2015$REGION[data2015$STATE == "LA"]<- "SE"
data2015$REGION[data2015$STATE == "ME"]<- "NE"
data2015$REGION[data2015$STATE == "MD"]<- "NE"
data2015$REGION[data2015$STATE == "MA"]<- "NE"
data2015$REGION[data2015$STATE == "MI"]<- "MW"
data2015$REGION[data2015$STATE == "MN"]<- "MW"
data2015$REGION[data2015$STATE == "MS"]<- "SE"
data2015$REGION[data2015$STATE == "MO"]<- "MW"
data2015$REGION[data2015$STATE == "MT"]<- "W"
data2015$REGION[data2015$STATE == "NE"]<- "MW"
data2015$REGION[data2015$STATE == "NV"]<- "W"
data2015$REGION[data2015$STATE == "NH"]<- "NE"
data2015$REGION[data2015$STATE == "NJ"]<- "NE"
data2015$REGION[data2015$STATE == "NM"]<- "SW"
data2015$REGION[data2015$STATE == "NY"]<- "NE"
data2015$REGION[data2015$STATE == "NC"]<- "SE"
data2015$REGION[data2015$STATE == "ND"]<- "MW"
data2015$REGION[data2015$STATE == "OH"]<- "MW"
data2015$REGION[data2015$STATE == "OK"]<- "SW"
data2015$REGION[data2015$STATE == "OR"]<- "W"
data2015$REGION[data2015$STATE == "PA"]<- "NE"
data2015$REGION[data2015$STATE == "RI"]<- "NE"
data2015$REGION[data2015$STATE == "SC"]<- "SE"
data2015$REGION[data2015$STATE == "SD"]<- "MW"
data2015$REGION[data2015$STATE == "TN"]<- "SE"
data2015$REGION[data2015$STATE == "TX"]<- "SW"
data2015$REGION[data2015$STATE == "UT"]<- "W"
data2015$REGION[data2015$STATE == "VT"]<- "NE"
data2015$REGION[data2015$STATE == "VA"]<- "SE"
data2015$REGION[data2015$STATE == "WA"]<- "W"
data2015$REGION[data2015$STATE == "WV"]<- "SE"
data2015$REGION[data2015$STATE == "WI"]<- "MW"
data2015$REGION[data2015$STATE == "WY"]<- "W"
data2015<-data2015[-(which(is.na(data2015$REGION))),] # Removing International Student


# data = merge(data2014, data2015, by=c("STUDENTID"), all=TRUE)
# write.csv(data2014, file = "data2014.csv", row.names = TRUE)
# write.csv(data2015, file = "data2015.csv", row.names = TRUE)


# Reading Processed Data
# data2014 <- read_csv("data2014.csv")
# data2015 <- read_csv("data2015.csv")
funding <- read_excel("funding.xlsx")
colnames(funding)[4] = "type"

variables_of_interest <- c("ACADEMIC_YEAR","REGION", "GENDER", "ETHNIC_CD", "DATE_APP", "FLAG_ROSTER_ATHLETE", "MAJOR1_ADMIT", "HSGPA", "HSGPASCALE", "ACT_COMP",  "SAT_COMP", "PARENT_INCOME", "STUDENT_INCOME", "TOTAL_FAFSA_POSITION", "BUDGET", "ROOM_BOARD","FLAG_ENR")
additional_variables <- c("STATE", "FLAG_HOMESTATE", "FLAG_COUNTY", "DENOM_CD", "FLAG_INQ", "ADMIT_TYPE", "FLAG_LEGACY", "FLAG_E_DECISION", "ISOURCE_CD", "INST_RAT", "HOUSING_FINANCIALAID", "FLAG_PELL_ELIGIBLE", "FLAG_INTENT")

data2014_filtered <- data2014[,variables_of_interest]
data2015_filtered <- data2015[,variables_of_interest]

##########
# Aggregating Fundings

data2014_filtered$PG = 0 # Public Grant
data2014_filtered$SS = 0 # School Scholarship
data2014_filtered$EX = 0 # External Funding (Grant)
data2014_filtered$L = 0 # Loan
data2014_filtered$UN = 0 # Unclassifed Funding
data2014_filtered$WS = 0 # Work Study

# data2014_filtered$PL = 0 # Public Loan
# data2014_filtered$SL = 0 # School Loan
# data2014_filtered$L = 0 # Private Loan


data2015_filtered$PG = 0 # Public Grant
data2015_filtered$SS = 0 # School Scholarship
data2015_filtered$EX = 0 # External Funding (Grant)
data2015_filtered$L = 0 # Loan
data2015_filtered$UN = 0 # Unclassifed Funding
data2015_filtered$WS = 0 # Work Study

# data2015_filtered$PL = 0 # Public Loan
# data2015_filtered$SL = 0 # School Loan
# data2015_filtered$L = 0 # Private Loan

# Set all loans as L
funding$type[funding$type == "PL"] = "L"
funding$type[funding$type == "SL"] = "L"


# Aggregating on 2014 data

AWD_CD1_idx14 <- match("AWD_CD1",colnames(data2014))
type_cols_2014 <- seq(AWD_CD1_idx14,AWD_CD1_idx14+58,3)

for (i in 1:nrow(data2014_filtered)) {
  for (t in type_cols_2014) {
    tag = unlist(data2014[i,t])
    if (is.na(tag)) {break}
    idx = match(tag,funding$fund_no)
    amt = as.integer(unlist(data2014[i,t+1]))
    type = funding$type[idx]
    if (is.na(type)) {type = "UN"}
    if (type == "PG") {data2014_filtered$PG[i] = data2014_filtered$PG[i] + amt}
    else if (type == "SS") {data2014_filtered$SS[i] = data2014_filtered$SS[i] + amt}
    else if (type == "EX") {data2014_filtered$EX[i] = data2014_filtered$EX[i] + amt}
    else if (type == "L") {data2014_filtered$L[i] = data2014_filtered$L[i] + amt}
    else if (type == "UN") {data2014_filtered$UN[i] = data2014_filtered$UN[i] + amt}
    else if (type == "WS") {data2014_filtered$WS[i] = data2014_filtered$WS[i] + amt;}
  }
}

# Aggregating on 2015 data

AWD_CD1_idx15 <- match("AWD_CD1",colnames(data2015))
type_cols_2015 <- seq(AWD_CD1_idx15,AWD_CD1_idx14+58,3)

for (i in 1:nrow(data2015_filtered)) {
  for (t in type_cols_2015) {
    tag = unlist(data2015[i,t])
    if (is.na(tag)) {break}
    idx = match(tag,funding$fund_no)
    amt = as.integer(unlist(data2015[i,t+1]))
    type = funding$type[idx]
    if (is.na(type)) {type = "UN"}
    if (type == "PG") {data2015_filtered$PG[i] = data2015_filtered$PG[i] + amt}
    else if (type == "SS") {data2015_filtered$SS[i] = data2015_filtered$SS[i] + amt}
    else if (type == "EX") {data2015_filtered$EX[i] = data2015_filtered$EX[i] + amt}
    else if (type == "L") {data2015_filtered$L[i] = data2015_filtered$L[i] + amt}
    else if (type == "UN") {data2015_filtered$UN[i] = data2015_filtered$UN[i] + amt}
    else if (type == "WS") {data2015_filtered$WS[i] = data2015_filtered$WS[i] + amt;}
  }
}

# Converting SAT_to_ACT
SAT_to_ACT <- function(sat) { 
  if (sat>1590) {return(36)}
  else if (1560<=sat) {return(35)}
  else if (1520<=sat) {return(34)}
  else if (1490<=sat) {return(33)}
  else if (1450<=sat) {return(32)}
  else if (1420<=sat) {return(31)}
  else if (1390<=sat) {return(30)}
  else if (1350<=sat) {return(29)}
  else if (1310<=sat) {return(28)}
  else if (1280<=sat) {return(27)}
  else if (1240<=sat) {return(26)}
  else if (1200<=sat) {return(25)}
  else if (1160<=sat) {return(24)}
  else if (1130<=sat) {return(23)}
  else if (1100<=sat) {return(22)}
  else if (1060<=sat) {return(21)}
  else if (1020<=sat) {return(20)}
  else if (980<=sat) {return(19)}
  else if (940<=sat) {return(18)}
  else if (900<=sat) {return(17)}
  else if (860<=sat) {return(16)}
  else if (810<=sat) {return(15)}
  else if (760<=sat) {return(14)}
  else if (720<=sat) {return(13)}
  else if (630<=sat) {return(12)}
  else if (560<=sat) {return(11)}
  else {return(10)}
}

data2014_filtered$TEST_COMP = 0
data2015_filtered$TEST_COMP = 0

for (i in 1:nrow(data2014_filtered)) {
  # Both missing
  sat = data2014_filtered$SAT_COMP[i]
  act = data2014_filtered$ACT_COMP[i]
  if (is.na(sat) && is.na(act)) {data2014_filtered$TEST_COMP[i] = 20}
  else if (is.na(sat)) {data2014_filtered$TEST_COMP[i] = act}
  else if (is.na(act)) {data2014_filtered$TEST_COMP[i] = SAT_to_ACT(sat)}
  else {data2014_filtered$TEST_COMP[i] = max(SAT_to_ACT(sat),act)}
}

for (i in 1:nrow(data2015_filtered)) {
  # Both missing
  sat = data2015_filtered$SAT_COMP[i]
  act = data2015_filtered$ACT_COMP[i]
  if (is.na(sat) && is.na(act)) {data2015_filtered$TEST_COMP[i] = 20}
  else if (is.na(sat)) {data2015_filtered$TEST_COMP[i] = act}
  else if (is.na(act)) {data2015_filtered$TEST_COMP[i] = SAT_to_ACT(sat)}
  else {data2015_filtered$TEST_COMP[i] = max(SAT_to_ACT(sat),act)}
}

## Dropping SAT an ACT from data frame
drops <- c("SAT_COMP","ACT_COMP")
data2014_filtered <- data2014_filtered[ , !(names(data2014_filtered) %in% drops)]
data2015_filtered <- data2015_filtered[ , !(names(data2015_filtered) %in% drops)]

##############

# Imputing Values

# Tag NA as UNK for unknown in ETHNIC_CD
data2014_filtered$ETHNIC_CD[is.na(data2014_filtered$ETHNIC_CD)] = "UNK"
data2015_filtered$ETHNIC_CD[is.na(data2015_filtered$ETHNIC_CD)] = "UNK"

# Tag NA as 1UND for undecided in MAJOR1_ADMIT
data2014_filtered$MAJOR1_ADMIT[is.na(data2014_filtered$MAJOR1_ADMIT)] = "1UND"
data2015_filtered$MAJOR1_ADMIT[is.na(data2015_filtered$MAJOR1_ADMIT)] = "Still Deciding"

# Put Average for missing high school GPA (likely homeschooled)
avg_gpa_14 = mean(data2014_filtered$HSGPA, na.rm=TRUE)
avg_gpa_15 = mean(data2015_filtered$HSGPA, na.rm=TRUE)

data2014_filtered$HSGPA[is.na(data2014_filtered$HSGPA)] = avg_gpa_14
data2015_filtered$HSGPA[is.na(data2015_filtered$HSGPA)] = avg_gpa_15

# Put Average for missing high school GPA (likely homeschooled)
avg_total_fafsa_14 = mean(data2014_filtered$TOTAL_FAFSA_POSITION, na.rm=TRUE)

data2014_filtered$TOTAL_FAFSA_POSITION[is.na(data2014_filtered$TOTAL_FAFSA_POSITION)] = avg_total_fafsa_14

write.csv(data2014_filtered, file = "data2014_filtered.csv", row.names = FALSE)
write.csv(data2015_filtered, file = "data2015_filtered.csv", row.names = FALSE)

# Impute Parent Income for that are 0 (none is missing)
med_parent_income_14 = median(data2014_filtered$PARENT_INCOME[data2014_filtered$PARENT_INCOME != 0])

data2014_filtered$PARENT_INCOME[data2014_filtered$PARENT_INCOME == 0] = med_parent_income_14

write.csv(data2014_filtered, file = "data2014_filtered_2.csv", row.names = FALSE)
write.csv(data2015_filtered, file = "data2015_filtered_2.csv", row.names = FALSE)