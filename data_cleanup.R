######################################
# THIS SCRIPTS PREPARES DATA FOR TRAINING
# THIS SCRIPT IS RAN FIRST


#load "Retail data"
rm(list = ls())
retail.data <- read.csv('Retail data.csv', stringsAsFactors = F, sep = ';')


# data summary
summary(retail.data)


# check for NAs by column
apply(retail.data, 2, function(x)sum(is.na(x)))

#label columns
colnames(retail.data)
numeric.columns <- c("AGE", "YEARS_WITH_BANK","CUST_INCOME","CURRENT_BALANCE_EUR")
factor.columns <- c("Mortgage_YN","MARTIAL_STATUS", "EDUCATION", "EMPLOYMENT", "GENDER")
date.columns <- c("CURRENT_ADDRESS_DATE" ,  "CURRENT_JOB_DATE"   ,    "CURRENT_WITH_BANK_DATE")


# format the columns
retail.data$CUST_INCOME <- as.numeric(sub(',','.',retail.data$CUST_INCOME)) # change comma to match locale


retail.data$CURRENT_BALANCE_EUR <- as.numeric(sub(',','.',retail.data$CURRENT_BALANCE_EUR))


for (col in factor.columns) {
  retail.data[,col] <- as.factor(retail.data[,col])
}

for (col in date.columns) {
  retail.data[,col] <- as.Date(retail.data[,col])
}


# feature engineering

install.packages("lubridate")
library("lubridate")  

plot(density(retail.data$AGE))
# most of or customers are between ages 30 and 60

# creating variable that extracts time working at current job
# job stability data seems valuable in determining mortgage
years.with.current.job <- time_length(difftime(Sys.Date(), retail.data$CURRENT_JOB_DATE), 'years')

bad.dates <- which(years.with.current.job < 0 ) # meaning current job is in future, bad data
years.with.current.job[bad.dates] <- NA

retail.data$YEARS_WITH_CURRENT_JOB <- years.with.current.job

hist(retail.data$YEARS_WITH_CURRENT_JOB[retail.data$AGE<30],
     main = "years on current job, younger than 30")

hist(retail.data$YEARS_WITH_CURRENT_JOB[retail.data$AGE>=30 &  retail.data$AGE<60],
     main = "years on current job,older than 30 younger than 60")


hist(retail.data$YEARS_WITH_CURRENT_JOB[retail.data$AGE>=60],
     main = "years on current job, older than 60")

# UPON VISUAL INSPECTION, if customer is older than 55, he most likely has 22 years at current job
# if customer is younger than 55, he most likely has 9 years at current job
# introduces mistakes, but slightly better than filling with overall median
# based on AGE variable only

young.customers <- which(retail.data$AGE<30)
mature.customers <- which(retail.data$AGE>=30 & retail.data$AGE<60)
senior.customers <- which(retail.data$AGE>=60)
young.median<- median(retail.data$YEARS_WITH_CURRENT_JOB[young.customers], na.rm = T)
mature.median <- median(retail.data$YEARS_WITH_CURRENT_JOB[mature.customers], na.rm = T)
senior.median <- median(retail.data$YEARS_WITH_CURRENT_JOB[senior.customers], na.rm = T)


for (i in intersect(bad.dates,young.customers)) {
  retail.data$YEARS_WITH_CURRENT_JOB[i] <- young.median
}
i <- 0
for (i in intersect(bad.dates,mature.customers)) {
  retail.data$YEARS_WITH_CURRENT_JOB[i] <- mature.median
}
i <- 0
for (i in intersect(bad.dates,senior.customers)) {
  retail.data$YEARS_WITH_CURRENT_JOB[i] <- mature.median
}
numeric.columns <- c(numeric.columns,"YEARS_WITH_CURRENT_JOB")

barplot(table(retail.data$MARTIAL_STATUS[retail.data$AGE<30]), main = "marital status, younger than 30")
barplot(table(retail.data$MARTIAL_STATUS[retail.data$AGE>=30]), main = "marital status, older than 30")



# customers younger than 30 are mostly single, older than 30 mostly married

marital.status.noval <- which(retail.data$MARTIAL_STATUS=="*noval*")

for (j in marital.status.noval) {
  if (retail.data$AGE[j]<30) {
    retail.data$MARTIAL_STATUS[j] <- "S"
  }
  retail.data$MARTIAL_STATUS[j] <- "M"
}

retail.data$MARTIAL_STATUS <- droplevels(retail.data$MARTIAL_STATUS)

write.csv(retail.data, file = "Retail data clean.csv", row.names = F)
