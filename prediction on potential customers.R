
rm(list = ls())

retail.data <- read.csv('Retail data clean.csv', stringsAsFactors = F)

# label strong features
numeric.columns <- c("CUST_INCOME", "CURRENT_BALANCE_EUR")
factor.columns <- c( "EDUCATION", "EMPLOYMENT","GENDER")


for (col in factor.columns) {
  retail.data[,col] <- as.factor(retail.data[,col])
}


# normalize numeric columns
normalized.retail.data <- as.data.frame(apply(retail.data[,numeric.columns], 2, FUN = function(x){scale(x)}))
normalized.retail.data[,factor.columns] <- retail.data[,factor.columns]

normalized.retail.data$Mortgage_YN <- as.factor(retail.data$Mortgage_YN)

#split data to training and test sets
library(caret)
set.seed(121)
train.ind <- createDataPartition(normalized.retail.data$Mortgage_YN,p=.8,list=F)
train.normalized.retail.data <- normalized.retail.data[train.ind,]
test.normalized.retail.data <- normalized.retail.data[-train.ind,]


#train model
library(randomForest)
set.seed(100)
random.forest.classifier = randomForest(x = train.normalized.retail.data[,-6],
                                        y = train.normalized.retail.data$Mortgage_YN,
                                        ntree = 100)


potential.customers <- read.csv('Potential Customers.csv',sep = ';', dec = '.')

which(potential.customers$EMPLOYMENT == "TEA")

# prepare potential customers for prediction
strong.features <- c("CUST_INCOME","CURRENT_BALANCE_EUR","EDUCATION", "EMPLOYMENT", "GENDER")


for (col in factor.columns) {
  potential.customers[,col] <- as.factor(potential.customers[,col])
}

# new factor not present in training data, remove 
potential.customers <- potential.customers[-which(potential.customers$EMPLOYMENT == "TEA"),]
potential.customers$EMPLOYMENT <- droplevels(potential.customers$EMPLOYMENT)

# potential customers only including columns of interest
curated.potential.customers <- potential.customers[,strong.features]

#scale  numeric
curated.potential.customers$CUST_INCOME <- as.numeric(sub(',','.',potential.customers$CUST_INCOME)) # change comma to match locale

curated.potential.customers$CUST_INCOME <- scale(curated.potential.customers$CUST_INCOME)

curated.potential.customers$CURRENT_BALANCE_EUR <- as.numeric(sub(',','.',potential.customers$CURRENT_BALANCE_EUR))

curated.potential.customers$CURRENT_BALANCE_EUR <- scale(curated.potential.customers$CURRENT_BALANCE_EUR)


# predict if potential customers would want mortgage or not

potential.customers$Predicted_Mortgage_YN <- predict(random.forest.classifier, newdata = curated.potential.customers)

# rearrange column names to be more readable
original.colnames <- c("Cocunut","Predicted_Mortgage_YN", "AGE", "YEARS_WITH_BANK", "MARTIAL_STATUS",
                       "EDUCATION", "EMPLOYMENT","GENDER" ,"CUST_INCOME" , "CURRENT_ADDRESS_DATE" , "CURRENT_JOB_DATE", 
                       "CURRENT_WITH_BANK_DATE", "CURRENT_BALANCE_EUR")

potential.customers <- potential.customers[,original.colnames]
table(potential.customers$Predicted_Mortgage_YN)
#write result to csv
write.csv(potential.customers, file = 'Potential Customers With Predictions.csv', sep = ';', row.names = F)

