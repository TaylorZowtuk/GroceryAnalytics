library(dplyr)

b_3_clean <- function(baskets, baskets_with_multiple_cust) {
  # Synthesize features by AND'ing cols
  baskets <- mutate(baskets, Prod_and_groc=Produce&Grocery)
  
  # Replace NA's in number of items in basket with 0
  baskets$NUM_ITEMS_IN_BASKET[is.na(baskets$NUM_ITEMS_IN_BASKET)] <- 0
  
  # Fix baskets with two customer ids
  basket_cust <- baskets[baskets$TILL_RECEIPT_NUMBER %in% baskets_with_multiple_cust$TILL_RECEIPT_NUMBER, ]
  baskets_valid_ids <- filter(basket_cust, CUSTOMER_ID!=999999999999999)
  baskets_invalid_ids <- filter(basket_cust, CUSTOMER_ID==999999999999999)
  
  # Update the basket row with a valid customer id by doing the logical OR with the basket with an invalid customer id
  baskets_valid_ids <- baskets_valid_ids %>% left_join(baskets_invalid_ids, by=c("TILL_RECEIPT_NUMBER")) %>% 
    transmute(TILL_RECEIPT_NUMBER, CUSTOMER_ID=CUSTOMER_ID.x, CU_AGE_RANGE=CU_AGE_RANGE.x, CU_GENDER=CU_GENDER.x, CUSHOM=CUSHOM.x, 
              FSPTS=FSPTS.x, FSRDM=FSRDM.x, NUM_ITEMS_IN_BASKET=NUM_ITEMS_IN_BASKET.x+NUM_ITEMS_IN_BASKET.y,
              Grocery=Grocery.x|Grocery.y, Deli=Deli.x|Deli.y, Tobacco=Tobacco.x|Tobacco.y, Produce=Produce.x|Produce.y, Bakery=Bakery.x|Bakery.y,
              Salad=Salad.x|Salad.y, Cigarettes=Cigarettes.x|Cigarettes.y, Bottle_deposit=Bottle_deposit.x|Bottle_deposit.y, Banana=Banana.x|Banana.y,
              Prod_and_groc=Prod_and_groc.x|Prod_and_groc.y,
              Lottery=Lottery.x|Lottery.y)
  # Fix the random NUM_ITEMS_IN_BASKET.y being na even though we cleaned the data...
  baskets_valid_ids$NUM_ITEMS_IN_BASKET[is.na(baskets_valid_ids$NUM_ITEMS_IN_BASKET)] <- 0
  
  # Remove baskets which have invalid customer ids; because weve already collected the information they contain
  # in the baskets with valid customer ids
  baskets <- baskets %>% anti_join(baskets_invalid_ids)
  # Now update the baskets with valid customer ids using our fixed 'baskets_valid_ids'
  baskets <- baskets %>% left_join(baskets_valid_ids, by=c("TILL_RECEIPT_NUMBER")) %>% 
    transmute(TILL_RECEIPT_NUMBER, CUSTOMER_ID=CUSTOMER_ID.x, CU_AGE_RANGE=CU_AGE_RANGE.x, CU_GENDER=CU_GENDER.x, CUSHOM=CUSHOM.x, 
              FSPTS=FSPTS.x, FSRDM=FSRDM.x, NUM_ITEMS_IN_BASKET=ifelse(is.na(NUM_ITEMS_IN_BASKET.y), NUM_ITEMS_IN_BASKET.x, NUM_ITEMS_IN_BASKET.y),
              Grocery=Grocery.x|Grocery.y, Deli=Deli.x|Deli.y, Tobacco=Tobacco.x|Tobacco.y, Produce=Produce.x|Produce.y, Bakery=Bakery.x|Bakery.y,
              Salad=Salad.x|Salad.y, Cigarettes=Cigarettes.x|Cigarettes.y, Bottle_deposit=Bottle_deposit.x|Bottle_deposit.y, Banana=Banana.x|Banana.y,
              Prod_and_groc=Prod_and_groc.x|Prod_and_groc.y,
              Lottery=Lottery.x|Lottery.y)
  
  # Replace NA's in our cart contains department cols
  baskets$Grocery[is.na(baskets$Grocery)] <- FALSE
  baskets$Tobacco[is.na(baskets$Tobacco)] <- FALSE
  baskets$Deli[is.na(baskets$Deli)] <- FALSE
  baskets$Produce[is.na(baskets$Produce)] <- FALSE
  baskets$Bakery[is.na(baskets$Bakery)] <- FALSE
  baskets$Salad[is.na(baskets$Salad)] <- FALSE
  baskets$Cigarettes[is.na(baskets$Cigarettes)] <- FALSE
  baskets$Bottle_deposit[is.na(baskets$Bottle_deposit)] <- FALSE
  baskets$Banana[is.na(baskets$Banana)] <- FALSE
  baskets$Prod_and_groc[is.na(baskets$Prod_and_groc)] <- FALSE
  baskets$Lottery[is.na(baskets$Lottery)] <- FALSE
  
  # Replace NA's in age range with "Unspecified"
  baskets$CU_AGE_RANGE[is.na(baskets$CU_AGE_RANGE)] <- "Unspecified"
  
  # Drop patrickbig
  baskets <- filter(baskets, CUSTOMER_ID!=7809708985) # bye patrickbig
  
  # Change the default gender from being M to "unknown" for default customers
  baskets[baskets$CUSTOMER_ID==999999999999999, ]$CU_GENDER <- "U"
  # Tidy up the genders before factoring
  baskets[baskets$CU_GENDER=="\r\n", ]$CU_GENDER <- "U"
  baskets[baskets$CU_GENDER=="F\r\n", ]$CU_GENDER <- "F"
  baskets[baskets$CU_GENDER=="M\r\n", ]$CU_GENDER <- "M"
  
  # Factor columns that contain char type data; age and gender
  baskets <- mutate_if(baskets, is.character, as.factor)
  
  # Factor CUSHOM
  baskets[,"CUSHOM"] <- lapply(baskets[, "CUSHOM"], as.factor)
  
  # Factor the "basket contains item from" boolean cols
  baskets <- mutate_if(baskets, is.logical, as.factor)
  
  return(baskets)
}
