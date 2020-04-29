##### MA Project 4

setwd("C:/Users/14702/OneDrive/Desktop/Emory/Macketing Analysis/Project 4 - Marketing Models")

# import library 
library(data.table)
library(dummies)

# read the dataset 
trans = fread("Data/transaction_table_supp.csv", header = TRUE)
prod = fread("Data/product_table_supp.csv", header = TRUE)
# merge the dataset together to 
trans = merge(trans, prod[, .(prod_id, category_desc_eng)], all.x = TRUE, by = "prod_id")

# add weekly information 
week = data.table(unique(trans[, tran_dt]))
colnames(week) <- 'tran_dt'
week[, tran_wk := strftime(tran_dt, format = "%Y%V")]
week = week[order(tran_dt)]
week = week[3:dim(week)[1], ] 
week[, wk_index := as.integer(strftime(tran_dt, format = "%V"))]
week[, wk_index := ifelse(strftime(tran_dt, format = "%Y")==2017,wk_index+52,wk_index)]
data = merge(trans, week, by = 'tran_dt')

# add advertising information 
ad = fread('promo_measures.csv',header = TRUE)
ad = ad[-which(tran_wk=='2015-12-27'|tran_wk=='2017-12-31'),]
# all tv data
tv = ad[,c("tran_wk","amount")][ad$vehicle=='TV']
names(tv)[2] = "TV"
# all Radio data
Radio = ad[,c("tran_wk","amount")][ad$vehicle=='Radio']
names(Radio)[2] = "Radio"
# all Flyer data
Flyer = unique(ad[,c("tran_wk","amount")][ad$vehicle=='Flyer'])
names(Flyer)[2] = "Flyer"
# all Email data
Email = unique(ad[,c("tran_wk","amount")][ad$vehicle=='Email'])
names(Email)[2] = "Email"
# all Paid Search data
Paid_Search = unique(ad[,c("tran_wk","amount")][ad$vehicle=='Paid Search'])
names(Paid_Search)[2] = "Paid Search"
# all Store Display data
Store_Display= unique(ad[,c("tran_wk","amount")][ad$vehicle=='Store Display'])
names(Store_Display)[2] = "Store Display"
# all Web Display data
Web_Display= unique(ad[,c("tran_wk","amount")][ad$vehicle=='Web Display'])
names(Web_Display)[2] = "Web Display"
# merge advertising data together
advertising = merge(tv,Radio,all.x = TRUE, by = "tran_wk")
advertising = merge(advertising, Flyer, all.x = TRUE, by = "tran_wk")
advertising = merge(advertising, Email, all.x = TRUE, by = "tran_wk")
advertising = merge(advertising, Paid_Search, all.x = TRUE, by = "tran_wk")
advertising = merge(advertising, Store_Display, all.x = TRUE, by = "tran_wk")
advertising = merge(advertising, Web_Display, all.x = TRUE, by = "tran_wk")

# add seasonality 
seasonality = fread("Data/seasonality.csv",header = TRUE)
seasonality = seasonality[-which(tran_wk=='2015-12-27'|tran_wk=='2017-12-31'),]
attributes = merge(advertising, seasonality, all.x = TRUE, by = "tran_wk")

# add holiday
holiday = fread("Data/holiday.csv",header = TRUE)
holiday = holiday[-which(tran_wk=='2015-12-27'|tran_wk=='2017-12-31'),]
holiday[,holiday_index:=1]
attributes = merge(attributes, unique(holiday[,.(tran_wk,holiday_index)]), all.x = TRUE, by = "tran_wk")
attributes[is.na(holiday_index),c("holiday_index")]=0

# merge attributes to the large dataset
attributes$wk_index = order(attributes$tran_wk)
dt = merge(data, attributes, all.x = TRUE, by = "wk_index")

# export the data
write.csv(dt, 'dt_attributes.csv',row.names = FALSE)



