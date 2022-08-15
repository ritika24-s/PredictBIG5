# this script is used to load all the relevant datasets 

# set the working directory
setwd("C:/Users/24oct/OneDrive - Trinity College Dublin/PredictBIG5")

library(dplyr)
library(readr)
install.packages('dqrng')
library('dqrng')
# import dataset
data_norm_rf <- read.csv("normalized_dataset_rf.csv")
data_rf <- read.csv("final_dataset_rf.csv")
data_svm <- read.csv("final_dataset_svm.csv")

# summarize the dataset
summary(data_rf)
summary(data_svm)
summary(data_norm_rf)

# preprocess the data

# remove na values
data_rf <-na.omit(data_rf)
data_svm <- na.omit(data_svm)

# convert categorical values to factor columns
data_rf <- as.data.frame(unclass(data_rf), stringsAsFactors = TRUE)
data_svm <- as.data.frame(unclass(data_svm), stringsAsFactors = TRUE)
data_norm_rf <- as.data.frame(unclass(data_norm_rf), stringAsFactors = TRUE)

# split dataset into test train
dqset.seed(101)
size = 0.8*nrow(data_rf)
# sample data using dqsample to make it non biased
sample = dqsample.int(nrow(data_rf), size, replace =TRUE)
train <- data_rf[sample,]
test <- data_rf[-sample,]
# check gender distribution
table(data_rf$gender)/nrow(data_rf)
prop.table(table(train$gender))
prop.table(table(test$gender))