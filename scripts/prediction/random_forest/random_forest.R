library(readr)
source("random_forest_cleaning.R")

# Set working dir to the path of this script; only works when using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data produced by random_forest.sql
baskets <- read_csv("random_forest.csv")
baskets_with_multiple_cust <- read_csv("baskets_with_multiple_cust.csv")

baskets <- clean_data_for_random_forest(baskets, baskets_with_multiple_cust)
