library(readr)

# Set working dir to the path of this script; only works when using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data produced by itemized_cost_profit_per_unit.sql
file_csv <- read_csv("itemized_cost_profit_per_unit.csv") 
