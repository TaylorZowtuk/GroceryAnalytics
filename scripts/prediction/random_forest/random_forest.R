library(readr)
library(randomForest)

# Set working dir to the path of this script; only works when using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("rf_1_cleaning.R")

# Load the data produced by rf_1.sql
baskets <- read_csv("rf_1.csv")
baskets_with_multiple_cust <- read_csv("baskets_with_multiple_cust.csv")

baskets <- rf_1_clean(baskets, baskets_with_multiple_cust)

# Partition the training, test sets using ratio 80:20
set.seed(7)
percent_train <- 8/10

train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

# Train a random forest model
rf_1_1 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T)
rf_1_1
varImpPlot(rf_1_1)

rf_1_2 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, cutoff=c(0.93,0.07))
rf_1_2
varImpPlot(rf_1_2)

# optimal ntree is 3500
rf_1_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=2, ntree=2000, cutoff=c(0.93,0.07))
rf_1_3
varImpPlot(rf_1_3)

# Try adding features
source("rf_2_cleaning.R")
# Load the data produced by rf_2.sql
baskets <- read_csv("rf_2.csv")
baskets <- rf_2_clean(baskets, baskets_with_multiple_cust)
#any(is.na(baskets$NUM_ITEMS_IN_BASKET))

train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

rf_2_1 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T)
rf_2_1
varImpPlot(rf_2_1)

rf_2_2 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, cutoff=c(0.55,0.45))
rf_2_2
varImpPlot(rf_2_2)

rf_2_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, mtry=4, ntree=2000, cutoff=c(0.55, 0.45))
rf_2_3
varImpPlot(rf_2_3)

# Add more features and synthesize features by AND'ing cols
source("rf_3_cleaning.R")
# Load the data produced by rf_3.sql
baskets <- read_csv("rf_3.csv")
baskets <- rf_3_clean(baskets, baskets_with_multiple_cust)

train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

rf_3_1 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T)
rf_3_1
varImpPlot(rf_3_1)

cut_offs <- list(c(0.55, 0.45), c(0.6, 0.4), c(0.7, 0.3), c(0.85, 0.15), c(0.95, 0.05), c(0.98, 0.02))
for (i in cut_offs) {
  rf_3_2 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER-Salad, data=train_set, importance=T, cutoff=i)
  print(rf_3_2)
}

rf_3_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER-Salad, data=train_set, importance=T, mtry=4, ntree=2000, cutoff=c(0.7, 0.3))
rf_3_3
varImpPlot(rf_3_3)

# Re produce the best model and evaluate on the test set
source("rf_1_cleaning.R")
baskets <- read_csv("rf_1.csv")
baskets <- rf_1_clean(baskets, baskets_with_multiple_cust)
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]
rf_1_3 <- randomForest(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, importance=T, ntree=4000, cutoff=c(0.85,0.15))
rf_1_3
rf_1_3.pred <- predict(rf_1_3, test_set)
rf_1_3.pred.results <- table(test_set$Lottery, rf_1_3.pred)
rf_1_3.pred.results
rf_1_3.test.error.rate  <- 1 - sum(diag(rf_1_3.pred.results)) / sum(rf_1_3.pred.results)
rf_1_3.test.error.rate
varImpPlot(rf_1_3)
