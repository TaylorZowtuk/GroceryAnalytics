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
percent_train <- 8/10
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]


dt_1_1 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=1, minbucket=1)
dt_1_1
rpart.plot(dt_1_1)

dt_1_2 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=2, minbucket=1, cp=-1)
dt_1_2$cptable

# Use CP value with minimum xerror
opt <- dt_1_2$cptable[which.min(dt_1_2$cptable[,"xerror"]),"CP"]
pruned <- prune(dt_1_2, cp = opt)
pruned
rpart.plot(pruned)

# the min xerror
min_xerror <- min(dt_1_2$cptable[,"xerror"])

# the corresponding xstd
corres_xstd <- dt_1_2$cptable[which.min(dt_1_2$cptable[,"xerror"]),"xstd"]
benchmark <- min_xerror + corres_xstd
RowNum <- min(which(dt_1_2$cptable[,"xerror"]< benchmark))
opt <- dt_1_2$cptable[RowNum,"CP"]
cvt_1_2 <- prune(dt_1_2,cp=opt)
cvt_1_2
rpart.plot(cvt_1_2)

predResults <- table(test_set$Lottery, predict(cvt_1_2,test_set,type="class")) 
test.error<-1-sum(diag(predResults))/sum(predResults)
test.error

source("./d_2_cleaning.R")
baskets <- read_csv("dt_2.csv")
baskets <- d_2_clean(baskets, baskets_with_multiple_cust)

set.seed(7)
percent_train <- 8/10
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]

dt_2_1 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=1, minbucket=1)
dt_2_1
rpart.plot(dt_2_1)

dt_2_2 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=2, minbucket=1, cp=-1)
dt_2_2$cptable

# Use CP value with minimum xerror
opt <- dt_2_2$cptable[which.min(dt_2_2$cptable[,"xerror"]),"CP"]
pruned <- prune(dt_2_2, cp = opt)
pruned
rpart.plot(pruned)

# the min xerror
min_xerror <- min(dt_2_2$cptable[,"xerror"])

# the corresponding xstd
corres_xstd <- dt_2_2$cptable[which.min(dt_2_2$cptable[,"xerror"]),"xstd"]
benchmark <- min_xerror + corres_xstd
RowNum <- min(which(dt_2_2$cptable[,"xerror"]< benchmark))
opt <- dt_2_2$cptable[RowNum,"CP"]
cvt_2_2 <- prune(dt_2_2,cp=opt)
cvt_2_2
rpart.plot(cvt_2_2)

source("./d_3_cleaning.R")
baskets <- read_csv("dt_3.csv")
baskets <- d_3_clean(baskets, baskets_with_multiple_cust)

set.seed(7)
percent_train <- 8/10
train_ids <- sample(1:nrow(baskets), size=percent_train*nrow(baskets) , replace=F)
train_set <- baskets[train_ids, ]
test_set <- baskets[-train_ids, ]


dt_3_1 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=1, minbucket=1)
dt_3_1

dt_3_2 <- rpart(Lottery~.-CUSTOMER_ID-TILL_RECEIPT_NUMBER, data=train_set, method="class", parms=list(split="information"), minsplit=2, minbucket=1, cp=-1)
dt_3_2$cptable

# Use CP value with minimum xerror
opt <- dt_3_2$cptable[which.min(dt_3_2$cptable[,"xerror"]),"CP"]
pruned <- prune(dt_3_2, cp = opt)
pruned
rpart.plot(pruned)

# the min xerror
min_xerror <- min(dt_3_2$cptable[,"xerror"])

# the corresponding xstd
corres_xstd <- dt_3_2$cptable[which.min(dt_3_2$cptable[,"xerror"]),"xstd"]
benchmark <- min_xerror + corres_xstd
RowNum <- min(which(dt_3_2$cptable[,"xerror"]< benchmark))
opt <- dt_3_2$cptable[RowNum,"CP"]
cvt_3_2 <- prune(dt_3_2,cp=opt)
cvt_3_2
rpart.plot(cvt_3_2)


