library(readr)
library(randomForest)

# Set working dir to the path of this script; only works when using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("random_forest_cleaning.R")

# Load the data produced by random_forest.sql
baskets <- read_csv("random_forest.csv")
baskets_with_multiple_cust <- read_csv("baskets_with_multiple_cust.csv")

baskets <- clean_data_for_random_forest(baskets, baskets_with_multiple_cust)

# Partition the training, eval, test sets using ratio 70:15:15
set.seed(7)
percent_train <- 7/10
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
remaining <- baskets[-train_ids, ]
percent_test <- 1/2
test_ids <- sample(1:nrow(remaining), size=percent_test*nrow(remaining) , replace=F)
valid_set <- remaining[-test_ids, ]
test_set <- remaining[test_ids, ]

# Train a random forest model
rf_1 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T)
rf_1
rf_1.valid <- predict(rf_1, valid_set)
rf_1.valid.results <- table(valid_set$Lottery, rf_1.valid)
rf_1.valid.results
rf_1.valid.error.rate  <- 1 - sum(diag(rf_1.valid.results)) / sum(rf_1.valid.results)
rf_1.valid.error.rate
varImpPlot(rf_1)

rf_2 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, cutoff=c(0.93,0.07))
rf_2
rf_2.valid <- predict(rf_2, valid_set)
rf_2.valid.results <- table(valid_set$Lottery, rf_2.valid)
rf_2.valid.results
rf_2.valid.error.rate  <- 1 - sum(diag(rf_2.valid.results)) / sum(rf_2.valid.results)
rf_2.valid.error.rate
varImpPlot(rf_2)

rf_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=2, ntree=2000, cutoff=c(0.93,0.07))
rf_3
rf_3.valid <- predict(rf_3, valid_set)
rf_3.valid.results <- table(valid_set$Lottery, rf_3.valid)
rf_3.valid.results
rf_3.valid.error.rate  <- 1 - sum(diag(rf_3.valid.results)) / sum(rf_3.valid.results)
rf_3.valid.error.rate
varImpPlot(rf_3)
