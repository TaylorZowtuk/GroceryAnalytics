library(readr)
library(ggplot2)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fspts_lottery <- read_csv("fspts_lottery.csv")

## Remove the entries that equal 0
fspts_lottery <- filter(fspts_lottery, FSPTS != 0)

## Check for outliers
ggplot(fspts_lottery) + geom_boxplot(aes(y = FSPTS))
summary(fspts_lottery)
upper_limit <- 845 + 1.5*(845-251)
lower_limit <- 251 - 1.5*(845-251)

## Remove the outliers
fspts_lottery <- filter(fspts_lottery, FSPTS < upper_limit)
fspts_lottery <- filter(fspts_lottery, FSPTS > lower_limit)

## Plot with bindwidth of 1
ggplot(fspts_lottery) + geom_histogram(aes(x = FSPTS), binwidth = 1)

