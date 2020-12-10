setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(rpart)
library(rpart.plot)
library(rattle)

# Load the data produced by random_forest.sql
baskets <- read_csv("dt_1.csv")
baskets_with_multiple_cust <- read_csv("baskets_with_multiple_cust.csv")
source("./d_1_cleaning.R")
baskets <- d_1_clean(baskets, baskets_with_multiple_cust)


# Partition the training, eval, test sets using ratio 70:15:15
set.seed(7)
percent_train <- 7/10
percent_test <- 1/2
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids,]
rest <- baskets[-train_ids,]
test_ids <- sample(1:nrow(rest), size=percent_test*nrow(rest) , replace=F)
test_set <- rest[test_ids,]
validation_set <- rest[-test_ids,]

dt_1_1 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"))
dt_1_1
rpart.plot(dt_1_1)

predResults <- table(validation_set$Lottery, predict(dt_1_1,validation_set,type="class")) 
validation.error <- 1-sum(diag(predResults))/sum(predResults)
validation.error

dt_1_2 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=1, minbucket=1)
dt_1_2
rpart.plot(dt_1_2)

predResults <- table(validation_set$Lottery, predict(dt_1_2,validation_set,type="class")) 
validation.error <- 1-sum(diag(predResults))/sum(predResults)
validation.error

## For cross validation we use a 80:20 split
set.seed(7)
percent_train <- 8/10
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]


dt_1_3 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=2, minbucket=1, cp=-1)
dt_1_3$cptable

# the min xerror
min_xerror <- min(dt_1_3$cptable[,"xerror"])

# the corresponding xstd
corres_xstd <- dt_1_3$cptable[which.min(dt_1_3$cptable[,"xerror"]),"xstd"]
benchmark <- min_xerror + corres_xstd
RowNum <- min(which(dt_1_3$cptable[,"xerror"]< benchmark))
opt <- dt_1_3$cptable[RowNum,"CP"]
cvt_1_3 <- prune(dt_1_3,cp=opt)
cvt_1_3
rpart.plot(cvt_1_3)

predResults <- table(test_set$Lottery, predict(cvt_1_3,test_set,type="class")) 
test.error<-1-sum(diag(predResults))/sum(predResults)
test.error

source("./d_2_cleaning.R")
baskets <- read_csv("dt_2.csv")
baskets <- d_2_clean(baskets, baskets_with_multiple_cust)

# Partition the training, eval, test sets using ratio 70:15:15
set.seed(7)
percent_train <- 7/10
percent_test <- 1/2
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids,]
rest <- baskets[-train_ids,]
test_ids <- sample(1:nrow(rest), size=percent_test*nrow(rest) , replace=F)
test_set <- rest[test_ids,]
validation_set <- rest[-test_ids,]

dt_2_1 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"))
dt_2_1
rpart.plot(dt_2_1)

predResults <- table(validation_set$Lottery, predict(dt_2_1,validation_set,type="class")) 
validation.error <- 1-sum(diag(predResults))/sum(predResults)
validation.error

dt_2_2 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=1, minbucket=1)
dt_2_2
rpart.plot(dt_2_2)

predResults <- table(validation_set$Lottery, predict(dt_2_2,validation_set,type="class")) 
validation.error <- 1-sum(diag(predResults))/sum(predResults)
validation.error

## For cross validation we use a 80:20 split
set.seed(7)
percent_train <- 8/10
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

dt_2_3 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=2, minbucket=1, cp=-1)
dt_2_3$cptable

# the min xerror
min_xerror <- min(dt_2_3$cptable[,"xerror"])

# the corresponding xstd
corres_xstd <- dt_2_3$cptable[which.min(dt_2_3$cptable[,"xerror"]),"xstd"]
benchmark <- min_xerror + corres_xstd
RowNum <- min(which(dt_2_3$cptable[,"xerror"]< benchmark))
opt <- dt_2_3$cptable[RowNum,"CP"]
cvt_2_3 <- prune(dt_2_3,cp=opt)
cvt_2_3
rpart.plot(cvt_2_3)

predResults <- table(test_set$Lottery, predict(cvt_2_3,test_set,type="class")) 
test.error<-1-sum(diag(predResults))/sum(predResults)
test.error

source("./d_3_cleaning.R")
baskets <- read_csv("dt_3.csv")
baskets <- d_3_clean(baskets, baskets_with_multiple_cust)

# Partition the training, eval, test sets using ratio 70:15:15
set.seed(7)
percent_train <- 7/10
percent_test <- 1/2
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids,]
rest <- baskets[-train_ids,]
test_ids <- sample(1:nrow(rest), size=percent_test*nrow(rest) , replace=F)
test_set <- rest[test_ids,]
validation_set <- rest[-test_ids,]

dt_3_1 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"))
dt_3_1
rpart.plot(dt_3_1)

predResults <- table(validation_set$Lottery, predict(dt_3_1,validation_set,type="class")) 
validation.error <- 1-sum(diag(predResults))/sum(predResults)
validation.error

dt_3_2 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=1, minbucket=1)
dt_3_2
rpart.plot(dt_3_2)

predResults <- table(validation_set$Lottery, predict(dt_3_2,validation_set,type="class")) 
validation.error <- 1-sum(diag(predResults))/sum(predResults)
validation.error


## For cross validation we use a 80:20 split
set.seed(7)
percent_train <- 8/10
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

dt_3_3 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=2, minbucket=1, cp=-1)
dt_3_3$cptable

# the min xerror
min_xerror <- min(dt_3_3$cptable[,"xerror"])

# the corresponding xstd
corres_xstd <- dt_3_3$cptable[which.min(dt_3_3$cptable[,"xerror"]),"xstd"]
benchmark <- min_xerror + corres_xstd
RowNum <- min(which(dt_3_3$cptable[,"xerror"]< benchmark))
opt <- dt_3_3$cptable[RowNum,"CP"]
cvt_3_3 <- prune(dt_3_3,cp=opt)
cvt_3_3
rpart.plot(cvt_3_3)

predResults <- table(test_set$Lottery, predict(cvt_3_3,test_set,type="class")) 
test.error<-1-sum(diag(predResults))/sum(predResults)
test.error

