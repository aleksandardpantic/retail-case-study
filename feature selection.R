#SCRIPT FOR TESTING COMBINATION OF FEATURES THAT PRODUCE THE BEST RESULT


rm(list = ls())
#load "Retail data"
retail.data <- read.csv('Retail data clean.csv', stringsAsFactors = F)

numeric.columns <- c("CUST_INCOME", "CURRENT_BALANCE_EUR")
factor.columns <- c("Mortgage_YN", "EDUCATION", "EMPLOYMENT","GENDER")


for (col in factor.columns) {
  retail.data[,col] <- as.factor(retail.data[,col])
}


normalized.retail.data <- as.data.frame(apply(retail.data[,numeric.columns], 2, FUN = function(x){scale(x)}))
normalized.retail.data[,factor.columns] <- retail.data[,factor.columns]


set.seed(11)
train.ind <- createDataPartition(normalized.retail.data$Mortgage_YN,p=.8,list=F)
train.normalized.retail.data <- normalized.retail.data[train.ind,]
test.normalized.retail.data <- normalized.retail.data[-train.ind,]

library(randomForest)

set.seed(1110)
random.forest.classifier = randomForest(x = train.normalized.retail.data[,-3],
                                        y = train.normalized.retail.data$Mortgage_YN,
                                        ntree = 100)

# Predicting the Test set results
y_pred = predict(random.forest.classifier, newdata = test.normalized.retail.data[,-3])

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

random.forest.metrics <- c(accuracy=accuracy,precision=precision,recall=recall,f1=f1)
cm
random.forest.metrics



