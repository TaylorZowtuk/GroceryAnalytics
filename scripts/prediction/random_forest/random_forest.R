library(readr)
library(randomForest)
source("random_forest_cleaning.R")

# Set working dir to the path of this script; only works when using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
valid_set <- baskets[-test_ids, ]
test_set <- baskets[test_ids, ]

# Train a random forest model


