SELECT DISTINCT ITEM_DESCRIPTION, UPC, COST_AMOUNT/QUANTITY_SOLD AS COST_PER_ITEM, (SALES_VALUE - COST_AMOUNT)/QUANTITY_SOLD AS PROFIT_PER_ITEM
FROM transactionsT
WHERE QUANTITY_SOLD > 0 AND SPECIAL_TYPE = 0;