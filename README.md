# retail-case-study

Complete case study of bank users, using random forest classification to predict whether customer might want mortgage or not.

## data_cleanup.R

Reading raw data from **Retail data.csv** , column formatting, imputing missing values, feature extraction, writing to **Retail data clean.csv**

## model selection.R

Using clean data to create machine learning model algorithms: **random forest, linear regression, support vector machines**, and analyzing metrics to find best model.

## feature selection.R

Combining features to see best fit for model, evaluating metrics, using backwards elimination

## prediction on potential customers.R

Using best  model and best features to predict mortgage on new set of customers from **Potential Customers.csv** , then writing results to Potential **Customers With Predictions.csv**

