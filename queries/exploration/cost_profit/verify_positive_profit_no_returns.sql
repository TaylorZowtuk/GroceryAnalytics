SELECT ID, QUANTITY_SOLD, SALES_VALUE, COST_AMOUNT
FROM transactionsT
WHERE QUANTITY_SOLD > 0 AND (SALES_VALUE < COST_AMOUNT);
