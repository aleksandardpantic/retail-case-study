#load "Retail data"
retail.data <- read.csv('Retail data.csv', stringsAsFactors = F, sep = ';')


# data summary
summary(retail.data)


# check for NAs by column
apply(retail.data, 2, function(x)sum(is.na(x)))


#remove AGE_AT_ORIGINATION due to too much NAs
retail.data$AGE_AT_ORIGINATION <- NULL

#label columns
colnames(retail.data)
numeric.columns <- c("Cocunut" ,"AGE", "YEARS_WITH_BANK","CUST_INCOME","CURRENT_BALANCE_EUR")
factor.columns <- c("Mortgage_YN","MARTIAL_STATUS", "EDUCATION", "EMPLOYMENT", "GENDER")
date.columns <- c("CURRENT_ADDRESS_DATE" ,  "CURRENT_JOB_DATE"   ,    "CURRENT_WITH_BANK_DATE")


# format the columns
income <- as.numeric(sub(',','.',retail.data$CUST_INCOME))
retail.data$CUST_INCOME <- income

current.balance <- as.numeric(sub(',','.',retail.data$CURRENT_BALANCE_EUR))
retail.data$CURRENT_BALANCE_EUR <- current.balance

for (col in factor.columns) {
  retail.data[,col] <- as.factor(retail.data[,col])
}

for (col in date.columns) {
  retail.data[,col] <- as.Date(retail.data[,col])
}

