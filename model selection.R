########################
# THIS SCRIPT TRAINS THE MODEL WITH DIFFERENT ALGORITHMS THAT PRODUCE THE BEST RESULT

rm(list = ls())
retail.data <- read.csv('Retail data clean.csv', stringsAsFactors = T)
# write.csv(retail.data, file = "Retail data clean.csv", row.names = F)

numeric.columns <- c("AGE", "YEARS_WITH_BANK","CUST_INCOME","CURRENT_BALANCE_EUR")
factor.columns <- c("Mortgage_YN","MARTIAL_STATUS", "EDUCATION", "EMPLOYMENT", "GENDER")
date.columns <- c("CURRENT_ADDRESS_DATE" ,  "CURRENT_JOB_DATE"   ,    "CURRENT_WITH_BANK_DATE")

# checking for discriminative factors

prop.table(table(retail.data$Mortgage_YN))

# POSITIVE CLASS Y is only 1.27% of the dataset, unbalanced data

positive.ind <- which(retail.data$Mortgage_YN == 'Y') # analyzing the data of interest group, 
# Customers with mortgage

# check for differences in large numeric columns

summary(retail.data[positive.ind,numeric.columns]) #summary of positive class records  
summary(retail.data[-positive.ind,numeric.columns])#summary of negative class records 

summary(retail.data[positive.ind,"CUST_INCOME"]) #summary of positive class colum "CUST_INCOME"  
summary(retail.data[-positive.ind,"CUST_INCOME"])#summary of negative class colum "CUST_INCOME"   

# 3rd quartile of income of customers without mortgage is approx to 1st quartile of customers with mortgage
# meaning CUST_INCOME is a VERY strong indicator



summary(retail.data[positive.ind,"CURRENT_BALANCE_EUR"]) #summary of positive class colum "CURRENT_BALANCE_EUR"  
summary(retail.data[-positive.ind,"CURRENT_BALANCE_EUR"])#summary of negative class colum "CURRENT_BALANCE_EUR" 
# same way CURRENT_BALANCE_EUR is a VERY strong indicator


quantile(retail.data$CUST_INCOME[positive.ind], seq(0,1,0.05))




library(ggplot2)

par(mfrow=c(2,2))
hist(retail.data$AGE[positive.ind], main = "Ages of customers WITH Mortgage",
     col = 'darkblue', 
     xlim = c(18,90),
     xlab="Age of customer",
     ylab= "Number of customers")



hist(retail.data$AGE[-positive.ind], main = "Ages of customers WITHOUT Mortgage",
     col = 'darkred', 
     xlim = c(18,90),
     xlab="Age of customer",
     ylab= "Number of customers")

par(mfrow=c(1,1))

hist(retail.data$YEARS_WITH_BANK[positive.ind],
     main = "Years with the bank of customers WITH mortgage", 
     col = 'darkred', 
     xlim = c(0,20),
     xlab="Years with the bank",
     ylab= "Number of customers")

barplot(table(retail.data$MARTIAL_STATUS[positive.ind]),
        main = "Marital Status of customers WITH mortgage",
        col = 'darkgreen',
        xlab = "Marital Status",
        ylab = "Number of customers",)

barplot(table(retail.data$EDUCATION[positive.ind]),
        main = "Education of customers WITH mortgage",
        col = 'brown',
        xlab = "Education level",
        ylab = "Number of customers",)

barplot(table(retail.data$GENDER[positive.ind]),
        main = "Gender of customers WITH mortgage",
        col = 'black',
        xlab = "Gender",
        ylab = "Number of customers",)

ggplot(retail.data, aes(EMPLOYMENT, fill = Mortgage_YN)) +
  geom_bar() +
  coord_flip()



#Normalize numeric variables
normalized.retail.data <- as.data.frame(apply(retail.data[,numeric.columns], 2, 
                                              FUN = function(x){scale(x)}))
normalized.retail.data[,factor.columns] <- retail.data[,factor.columns]


#based on data visualization, and  experimenting with feature combinations
# strongest predictors of positive class "Y" are Customer income, Current balance, education and employment

strong.features <- c("CUST_INCOME","CURRENT_BALANCE_EUR","EDUCATION", "EMPLOYMENT", "GENDER", "Mortgage_YN")
# training on subset without these columns

normalized.retail.data <- normalized.retail.data[,strong.features]


#install.packages("caret")
#install.packages("caTools")
#install.packages("ROCR")    
library(caret)
library(caTools)
library(e1071)
#TRAIN TEST SPLIT
set.seed(100)
train.ind <- createDataPartition(normalized.retail.data$Mortgage_YN,p=.8,list=F)
train.normalized.retail.data <- normalized.retail.data[train.ind,]
test.normalized.retail.data <- normalized.retail.data[-train.ind,]

# random forest, logistic regression and support vector machines,
# they are better at dealing with imbalanced datasets

#RANDOM FOREST CLASSIFICATION
library(randomForest)

 set.seed(100)
random.forest.classifier = randomForest(x = train.normalized.retail.data[,-6],
                          y = train.normalized.retail.data$Mortgage_YN,
                          ntree = 100)

# Predicting the Test set results
y_pred = predict(random.forest.classifier, newdata = test.normalized.retail.data[,-6])

# Making the Confusion Matrix
cm = table(true=test.normalized.retail.data$Mortgage_YN, pred=y_pred)

TP <- cm[2,2]
TN <- cm[1,1]
FN <- cm[1,2]
FP <- cm[2,1]

accuracy <- (TP+TN)/sum(cm)
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)
f1 <- 2*precision*recall/(precision+recall)
cm
random.forest.metrics <- rbind(c(accuracy=accuracy,precision=precision,recall=recall,f1=f1))
random.forest.metrics

#logistic regression classifier
set.seed(100)


logistic.regression.classifier <- glm(Mortgage_YN ~ EDUCATION + EMPLOYMENT + GENDER + CUST_INCOME + CURRENT_BALANCE_EUR,
                                      data = train.normalized.retail.data,
                                      family = "binomial")



y_pred = predict(logistic.regression.classifier, newdata = test.normalized.retail.data[,-6], type = 'response')
y_pred <- as.factor(ifelse(y_pred<0.5,'N','Y'))

cm = table(true=test.normalized.retail.data$Mortgage_YN, pred=y_pred)

TP <- cm[2,2]
TN <- cm[1,1]
FN <- cm[1,2]
FP <- cm[2,1]

accuracy <- (TP+TN)/sum(cm)
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)
f1 <- 2*precision*recall/(precision+recall)

logistic.regression.metrics <- rbind(c(accuracy=accuracy,precision=precision,recall=recall,f1=f1))
logistic.regression.metrics

#SUPPORT VECTOR MACHINES
set.seed(100)
svm.classifier <- svm(Mortgage_YN ~ EDUCATION + EMPLOYMENT+ GENDER + CUST_INCOME + CURRENT_BALANCE_EUR,
                      data = train.normalized.retail.data,
                      type = 'C-classification',
                      kernel = 'linear')

y_pred = predict(svm.classifier, newdata = test.normalized.retail.data[,-6])

# Making the Confusion Matrix
cm = table(true=test.normalized.retail.data$Mortgage_YN, pred=y_pred)

TP <- cm[2,2]
TN <- cm[1,1]
FN <- cm[1,2]
FP <- cm[2,1]

accuracy <- (TP+TN)/sum(cm)
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)
f1 <- 2*precision*recall/(precision+recall)

svm.metrics <- rbind(c(accuracy=accuracy,precision=precision,recall=recall,f1=f1))
svm.metrics

metrics <- rbind(random.forest.metrics,logistic.regression.metrics, svm.metrics)
metrics


#################################
########## KOMENTAR ##############
# accuracy is not very useful for imbalanced data

# FALSE POSITIVES: someone predicted to want to take mortgage, but doesn't
# FALSE NEGATIVES: someone predicted to NOT want to take mortgage, but DOES

# FALSE POSITIVE means we advertised to somebody that is not interested, not much loss
# FALSE NEGATIVE is much worse because we ignore customers that would pay mortgage, LARGE LOSS

# RECALL is a metric that shows how many FALSE NEGATIVES model has
# RECALL should be used for choosing the best model
# if we can afford do advertise to more customers, precision is not very important

# RANDOM FOREST CLASSIFIER has largest recall, it will be used for prediction
