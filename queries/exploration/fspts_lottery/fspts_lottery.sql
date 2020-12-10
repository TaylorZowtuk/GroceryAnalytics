SELECT FSPTS
FROM customersT
INNER JOIN transactionsT
ON transactionsT.CUSTOMER_ID = customersT.CUSTOMER_ID
WHERE transactionsT.SUB_DEPARTMENT IN (161, 160, 201);