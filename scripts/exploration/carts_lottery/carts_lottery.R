## Import subdept Lottery
carts <- read_csv("CartMatrix.csv")

## How many carts have lottery items?
sum(!is.na(carts$Lottery))
## 2924

## Remove carts without lottery items
contains_lottery <- carts[!is.na(carts$Lottery), ]

## Get the total count for each dept
contains_lottery_no_lottery_till <- select(contains_lottery, -TILL_RECEIPT_NUMBER, -Lottery)
dept_totals <- tibble(Count=apply(contains_lottery_no_lottery_till, 2, sum, na.rm=TRUE))
dept_totals$Dept = colnames(contains_lottery_no_lottery_till)
ggplot(dept_totals, aes(x = Dept, y = Count)) + geom_bar(stat='identity')

## Remove the columns with 0
dept_totals_no_zero <- dept_totals[dept_totals$Count !=0, ]
ggplot(dept_totals_no_zero, aes(x = Dept, y = Count)) + geom_bar(stat='identity')

## Focus in on the grocery subdepartments
grocery_subdept_carts <- read_csv("GrocerySubdept.csv")
## Take out carts that don't contain a lottery product
grocery_subdept_carts <- grocery_subdept_carts[!is.na(grocery_subdept_carts$Lottery),]
## Take out the till number and lottery before graphing
grocery_subdept_carts <- select(grocery_subdept_carts, -TILL_RECEIPT_NUMBER, -Lottery)
## Count the number of carts containing each grocery subdept
grocery_subdept_totals <- tibble(Count=apply(grocery_subdept_carts, 2, sum, na.rm=TRUE))
## Add the names of the subdepts
grocery_subdept_totals$Dept = colnames(grocery_subdept_carts)
ggplot(grocery_subdept_totals, aes(x = Dept, y = Count)) + geom_bar(stat='identity')

## Focus in on the deli subdepartments
deli_subdept_carts <- read_csv("DeliSubdept.csv")
## Take out carts that don't contain a lottery product
deli_subdept_carts <- deli_subdept_carts[!is.na(deli_subdept_carts$Lottery),]
## Take out the till number and lottery before graphing
deli_subdept_carts <- select(deli_subdept_carts, -TILL_RECEIPT_NUMBER, -Lottery)
## Count the number of carts containing each grocery subdept
deli_subdept_totals <- tibble(Count=apply(deli_subdept_carts, 2, sum, na.rm=TRUE))
## Add the names of the subdepts
deli_subdept_totals$Dept = colnames(deli_subdept_carts)
ggplot(deli_subdept_totals, aes(x = Dept, y = Count)) + geom_bar(stat='identity')

## Focus in on the tobacco subdepartments
tobacco_subdept_carts <- read_csv("TobaccoSubdept.csv")
## Take out carts that don't contain a lottery product
tobacco_subdept_carts <- tobacco_subdept_carts[!is.na(tobacco_subdept_carts$Lottery),]
## Take out the till number and lottery before graphing
tobacco_subdept_carts <- select(tobacco_subdept_carts, -TILL_RECEIPT_NUMBER, -Lottery)
## Count the number of carts containing each grocery subdept
tobacco_subdept_totals <- tibble(Count=apply(tobacco_subdept_carts, 2, sum, na.rm=TRUE))
## Add the names of the subdepts
tobacco_subdept_totals$Dept = colnames(tobacco_subdept_carts)
ggplot(tobacco_subdept_totals, aes(x = Dept, y = Count)) + geom_bar(stat='identity')


