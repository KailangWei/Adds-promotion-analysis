# Adds-promotion-analysis
Research on how different methods of addvertising promotion imapct on sale volume

Problem
We verify the effectiveness of its promotions and marketing partnership with Pernalonga.  The client is interested in identifying promotion and marketing activities that drive significant incremental sales for continuation into 2020.  The timing is perfect, your Data Science team recently concluded a recommender systems project and a pricing project with Pernalonga, so you are well-versed with Pernalonga’s data.  
We also have the following insights:
-Seasonality of the Beer category
-National holidays in Lunitunia in 2016 and 2017
-TV advertisements have an 8-week half-life, and GRPs map into target audience 2+ Reach via the following formula
-Radio advertisements have a 4-week half-life, and GRPs map into target audience 2+ Reach via the following formula
With the above insights and supplemental transaction and product data from Pernalonga, you are ready to demonstrate your capabilities in marketing analytics and help the client identify promotion and marketing vehicles that drive significant results.

Available Data

1.transaction_table.csv contains transaction history in 2016 and 2017 for close to 8,000 customers
cust_id – Customer ID
tran_id – Transaction ID
tran_dt – Transaction Date
store_id – Store ID
prod_id – Product ID
prod_unit – Product unit of measure: CT for count and KG for kilograms
prod_unit_price – Unit price of the product
tran_prod_sale_qty – Quantity/units of the product in the transaction
tran_prod_sale_amt – Sales amount for the product before discounts in the transaction
tran_prod_discount_amt – Total amount of discounts applied to the product in the transaction
tran_prod_offer_cts – Total number of offers on the product resulting in the total amount of discounts in the transaction
tran_prod_paid_amt – Amount paid for the product after discounts are applied in the transaction

2.product_table.csv contains the product to subcategory and category mapping and descriptions for about 11,000 products
prod_id – Product ID
subcategory_id – Subcategory ID
category_id – Category ID
sub_category_desc – Subcategory name (in Portuguese)
category_desc – Category name (in Portuguese)
category_desc_eng – Category name (in English)
brand_desc – Brand of the product, including NO LABEL and PRIVATE LABEL

3.transaction_table_supp.csv contains supplementary transaction history for San Miguel products in 2016 and 2017 with same fields as transaction_table.csv
4.product_table_supp.csv contains supplementary records for San Miguel products with same fields as product_table.csv

5.promo_ad.csv contains the promotion and advertising activity records
tran_wk – the date of the Sunday representing the week
vehicle – the promo or advertising vehicle
amount – the amount of advertising in units; a “1” means the promotion (Flyer or Store Display) is on for the week
unit – the unit indicated by the amount
prod_assoc – the Product ID of the product featured in the promo or advertisement; “ALL” means all San Miguel products

6.seasonality.csv contains the seasonality index for products in the Beer Category
tran_wk – the date of the Sunday representing the week
seas_index – a number indicating seasonality index for the week

7.holiday.csv contains a list of national holidays celebrated in Lunitunia in 2016 and 2017
tran_wk – the date of the Sunday representing the week
holiday – the name of the holiday; a “Pr” prefix before a holiday indicates a week before the holiday

*Note that customer, store, product and promo and marketing information beyond what is available above are not provided.
