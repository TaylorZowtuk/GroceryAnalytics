library(readr)
library(randomForest)

# Set working dir to the path of this script; only works when using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("b_1_cleaning.R")

# Load the data produced by b_1.sql
baskets <- read_csv("b_1.csv")
baskets_with_multiple_cust <- read_csv("baskets_with_multiple_cust.csv")

baskets <- b_1_clean(baskets, baskets_with_multiple_cust)

# Partition the training, test sets using ratio 80:20
set.seed(7)
percent_train <- 8/10

train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

# Train a random forest model
b_1_1 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=7)
b_1_1
varImpPlot(b_1_1)

cut_offs <- list(c(0.25, 0.75), c(0.30, 0.70), c(0.35, 0.65), c(0.45, 0.55), c(0.55, 0.45), c(0.6, 0.4), c(0.7, 0.3), c(0.85, 0.15), c(0.95, 0.05), c(0.98, 0.02))
for (i in cut_offs) {
  b_1_2 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=7, cutoff=i)
  print(i)
  print(b_1_2)
}

# optimal ntree is 3500
b_1_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=7, ntree=2000, cutoff=c(0.25,0.75))
b_1_3
varImpPlot(b_1_3)

# Try adding features
source("b_2_cleaning.R")
# Load the data produced by b_2.sql
baskets <- read_csv("b_2.csv")
baskets <- b_2_clean(baskets, baskets_with_multiple_cust)

train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

b_2_1 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=11)
b_2_1
varImpPlot(b_2_1)

cut_offs <- list(c(0.25, 0.75), c(0.30, 0.70), c(0.35, 0.65), c(0.45, 0.55), c(0.55, 0.45), c(0.6, 0.4), c(0.7, 0.3), c(0.85, 0.15), c(0.95, 0.05), c(0.98, 0.02))
for (i in cut_offs) {
  b_2_2 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=11, cutoff=i)
  print(i)
  print(b_2_2)
}
varImpPlot(b_2_2)

b_2_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=11, ntree=2000, cutoff=c(0.25,0.75))
b_2_3
varImpPlot(b_2_3)

# Add more features and synthesize features by AND'ing cols
source("b_3_cleaning.R")
# Load the data produced by b_3.sql
baskets <- read_csv("b_3.csv")
baskets <- b_3_clean(baskets, baskets_with_multiple_cust)

train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

b_3_1 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=16)
b_3_1
varImpPlot(b_3_1)

cut_offs <- list(c(0.25, 0.75), c(0.30, 0.70), c(0.35, 0.65), c(0.45, 0.55), c(0.55, 0.45), c(0.6, 0.4), c(0.7, 0.3), c(0.85, 0.15), c(0.95, 0.05), c(0.98, 0.02))
for (i in cut_offs) {
  b_3_2 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=16, cutoff=i)
  print(i)
  print(b_3_2)
}
varImpPlot(b_3_2)

b_3_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=16, ntree=2000, cutoff=c(0.3, 0.7))
b_3_3
varImpPlot(b_3_3)

# Re produce the best model and evaluate on the test set
baskets <- read_csv("b_1.csv")
baskets <- b_1_clean(baskets, baskets_with_multiple_cust)
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]
b_1_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=7, ntree=4000, cutoff=c(0.25, 0.75))
b_1_3
b_1_3.pred <- predict(b_1_3, test_set)
b_1_3.pred.results <- table(test_set$Lottery, b_1_3.pred)
b_1_3.pred.results
b_1_3.test.error.rate  <- 1 - sum(diag(b_1_3.pred.results)) / sum(b_1_3.pred.results)
b_1_3.test.error.rate
varImpPlot(b_1_3)
