library(readr)
library(ggplot2)

# Set working dir to the path of this script; only works when using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data produced by itemized_cost_profit_per_unit.sql
data_csv <- read_csv("itemized_cost_profit_per_unit.csv")
ggplot(data = data_csv) +
  geom_bin2d(mapping = aes(x = COST_PER_ITEM, y = PROFIT_PER_ITEM))
ggplot(data = data_csv) +
  geom_point(mapping = aes(x = COST_PER_ITEM, y = PROFIT_PER_ITEM))

# Reveals items with negative cost
# Reveals items with very large cost
# Reveals items with very large profit

# Clean data by removing items with negative cost_per_item
data_csv <- data_csv[data_csv$COST_PER_ITEM >= 0, ]
ggplot(data = data_csv) +
  geom_bin2d(mapping = aes(x = COST_PER_ITEM, y = PROFIT_PER_ITEM))
ggplot(data = data_csv) +
  geom_point(mapping = aes(x = COST_PER_ITEM, y = PROFIT_PER_ITEM))

# Clean data by removing invalid UPC's
# Determine what length UPC's are present in the data
#sizes = c()
#for (i in 1:nrow(data_csv["UPC"])) {
#  upc = data_csv[i, "UPC"]
#  upc_length = (floor(log10(abs(upc))) + 1)
#  contained = upc_length %in% sizes
#  if (!contained) {
#    sizes <- c(sizes, upc_length)
#  }
#}
#sizes # 10, 11, 4, 12, 3, 2, 13, 1, 5, 7, 9, 8
# Remove all rows with UPC's that are less than 10 digits in length
data_csv <- data_csv[data_csv$UPC >= 1000000000, ]
ggplot(data = data_csv) +
  geom_bin2d(mapping = aes(x = COST_PER_ITEM, y = PROFIT_PER_ITEM))
ggplot(data = data_csv) +
  geom_point(mapping = aes(x = COST_PER_ITEM, y = PROFIT_PER_ITEM), alpha = 1 / 10)
