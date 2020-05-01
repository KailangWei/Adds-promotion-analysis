# load the data set-------------------------------------------------------------------------------------------------------------

library(data.table)
data = fread(file='promo_ad.csv', header=TRUE)

# convert the tran_wk column into date format
data$tran_wk = as.Date(data$tran_wk)

# find the data on TV and Radio promotions and their GRPs
tvdata = data[vehicle=='TV',]
radiodata = data[vehicle=='Radio',]

# remove the TV and Radio GRP information from the original data set because they need to be decayed and converted into reach
data = data[vehicle!='TV'&vehicle!='Radio',]

# calculate GRPs and 2+ Reach for TV advertisements--------------------------------------------------------------------------

# calculate alpha by week for 8 weeks half life
alpha = 1-0.5^(1/8)

# create the index by week from 2015-12-27 to 2017-12-31
tran_wk = seq(from = min(data$tran_wk), to = max(data$tran_wk), by = 'week')
index = as.data.table(tran_wk)

# merge the tvdata with index
tvdata = merge(tvdata, index, all.y = TRUE)
tvdata[is.na(amount),'amount'] = 0

# initiate the adstock column
tvdata$adstock = 0

# calculate the adstocks for each week
for (i in 24:106){
  tvdata$adstock[i] = tvdata$amount[i]*alpha + (1-alpha)*tvdata$adstock[i-1]
}

# calculate the reach for each week
tvdata$reach = 0.95*(1-(exp(1))^(-0.02*tvdata$adstock))

# append the reach data to the original data set
tvreachdata <- tvdata
tvreachdata$amount = tvreachdata$reach
tvreachdata$vehicle = 'TV'
tvreachdata$unit = 'reach'
tvreachdata$prod_assoc = 'ALL'
tvreachdata = tvreachdata[,c(1:5)]

data <- rbind(data, tvreachdata)

# calculate GRPs and 2+ Reach for Radio advertisements--------------------------------------------------------------------------

# calculate alpha by week for 4 weeks half life
alpha = 1-0.5^(1/4)

# merge the radiodata with index
radiodata = merge(radiodata, index, all.y = TRUE)
radiodata[is.na(amount),'amount'] = 0

# initiate the adstock column
radiodata$adstock = 0

# calculate the adstocks for each week
for (i in 24:106){
  radiodata$adstock[i] = radiodata$amount[i]*alpha + (1-alpha)*radiodata$adstock[i-1]
}

# calculate the reach for each week
radiodata$reach = 0.90*(1-(exp(1))^(-0.025*radiodata$adstock))

# append the reach data to the original data set
radioreachdata <- radiodata
radioreachdata$amount = radioreachdata$reach
radioreachdata$vehicle = 'Radio'
radioreachdata$unit = 'reach'
radioreachdata$prod_assoc = 'ALL'
radioreachdata = radioreachdata[,c(1:5)]

data <- rbind(data, radioreachdata)

# export the promotion measures data
write.csv(data, 'promo_measures.csv')

# data preparation for modeling------------------------------------------------------------------------------------------------------

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

# split the whole dataset into datasets of the three products
prod_1 = data[prod_id=="138936951"]
prod_2 = data[prod_id=="138936952"]
prod_3 = data[prod_id=="138936953"]

attributes = attributes[,-1]

# product 138936951 modeling--------------------------------------------------------------------------------------------------

# weekly sale volume
prod_1[,wkly_volume := sum(tran_prod_sale_qty), by = .(wk_index)]
# the theoretical maximum volume is assumed to be 15% more than the maximum historical volume
# prod_1[, max_wkly_volume := max(wkly_volume) * 1.15] 
# prod_1[, T_volume := log(wkly_volume/(max_wkly_volume - wkly_volume))]

# weekly price
prod_1[,wkly_price := mean(prod_unit_price), by = .(wk_index)]
# weekly discount
prod_1[,wkly_dct := -sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty), by = .(wk_index)]

# retrieve data and merge to attributes
data1 = unique(prod_1[,c("tran_wk", "wk_index", "wkly_volume", "wkly_price", "wkly_dct")])
data1 = merge(data1,attributes, all.x = TRUE, by = "wk_index")

data1[, avg_volume := mean(wkly_volume)]

# exclude flyer imapct 
data1[, flyer_volume := wkly_volume*Flyer]
data1[, flyer_volume_avg := ifelse(is.na(flyer_volume),0,mean(flyer_volume, na.rm = TRUE)-avg_volume)]

# exclude email imapct 
data1[, email_volume := wkly_volume*Email/200000]
data1[, email_volume_avg := ifelse(is.na(email_volume),0,mean(email_volume, na.rm = TRUE)-avg_volume)]

# exclude Store Display imapct 
data1[, sd_volume := wkly_volume*data1$`Store Display`]
data1[, sd_volume_avg := ifelse(is.na(sd_volume),0,mean(sd_volume, na.rm = TRUE)-avg_volume)]

# exclude Web Display imapct 
data1[, wd_volume := wkly_volume*data1$`Web Display`/280000]
data1[, wd_volume_avg := ifelse(is.na(wd_volume),0,mean(wd_volume, na.rm = TRUE)-avg_volume)]

# final target variable
data1[, volume := wkly_volume-flyer_volume_avg-email_volume_avg-sd_volume_avg-wd_volume_avg]
# data1[, volume := wkly_volume-email_volume_avg-sd_volume_avg-wd_volume_avg]
# data1[,flyer_01 := ifelse(is.na(Flyer),0,1)]

formula = volume ~ wkly_price + wkly_dct + wk_index + TV + Radio + data1$`Paid Search` + seas_index + holiday_index
lm <- lm(formula, data = data1)
summary(lm)

# according to prediction, get other advertising impact
data1[, tv_volume := 114.7*TV]
data1[, radio_volume := -85.45*Radio]
data1[, ps_volume := 0.0003962*data1$`Paid Search`]

# final duetos outcomes
data_1 = data1[,c("tran_wk","wkly_volume","tv_volume","radio_volume","ps_volume",
                  "flyer_volume_avg","email_volume_avg","sd_volume_avg","wd_volume_avg")]
# export the data
write.csv(data_1, 'data_138936951.csv',row.names = FALSE)

# product 138936952 modeling--------------------------------------------------------------------------------------------------

# weekly sale volume
prod_2[,wkly_volume := sum(tran_prod_sale_qty), by = .(wk_index)]
# the theoretical maximum volume is assumed to be 15% more than the maximum historical volume
# prod_2[, max_wkly_volume := max(wkly_volume) * 1.15] 
# prod_2[, T_volume := log(wkly_volume/(max_wkly_volume - wkly_volume))]

# weekly price
prod_2[,wkly_price := mean(prod_unit_price), by = .(wk_index)]
# weekly discount
prod_2[,wkly_dct := -sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty), by = .(wk_index)]

# retrieve data and merge to attributes
data2 = unique(prod_2[,c("tran_wk", "wk_index", "wkly_volume", "wkly_price", "wkly_dct")])
data2 = merge(data2,attributes, all.x = TRUE, by = "wk_index")

data2[, avg_volume := mean(wkly_volume)]

# exclude flyer imapct 
data2[, flyer_volume := wkly_volume*Flyer]
data2[, flyer_volume_avg := ifelse(is.na(flyer_volume),0,mean(flyer_volume, na.rm = TRUE)-avg_volume)]

# exclude email imapct 
data2[, email_volume := wkly_volume*Email/200000]
data2[, email_volume_avg := ifelse(is.na(email_volume),0,mean(email_volume, na.rm = TRUE)-avg_volume)]

# exclude Store Display imapct 
data2[, sd_volume := wkly_volume*data2$`Store Display`]
data2[, sd_volume_avg := ifelse(is.na(sd_volume),0,mean(sd_volume, na.rm = TRUE)-avg_volume)]

# exclude Web Display imapct 
data2[, wd_volume := wkly_volume*data2$`Web Display`/280000]
data2[, wd_volume_avg := ifelse(is.na(wd_volume),0,mean(wd_volume, na.rm = TRUE)-avg_volume)]

# final target variable
data2[, volume := wkly_volume-flyer_volume_avg-email_volume_avg-sd_volume_avg-wd_volume_avg]
# data2[, volume := wkly_volume-email_volume_avg-sd_volume_avg-wd_volume_avg]
# data2[,flyer_01 := ifelse(is.na(Flyer),0,1)]

formula = volume ~ wkly_price + wkly_dct + wk_index + TV + Radio + data2$`Paid Search` + seas_index + holiday_index
lm <- lm(formula, data = data2)
summary(lm)

# according to prediction, get other advertising impact
data2[, tv_volume := 14.14*TV]
data2[, radio_volume := -8.215*Radio]
data2[, ps_volume := 0.0002218*data2$`Paid Search`]

# final duetos outcomes
data_2 = data2[,c("tran_wk","wkly_volume","tv_volume","radio_volume","ps_volume",
                  "flyer_volume_avg","email_volume_avg","sd_volume_avg","wd_volume_avg")]
# export the data
write.csv(data_2, 'data_138936952.csv',row.names = FALSE)

# product 138936953 modeling-----------------------------------------------------------------------------------------------------

# weekly sale volume
prod_3[,wkly_volume := sum(tran_prod_sale_qty), by = .(wk_index)]
# the theoretical maximum volume is assumed to be 15% more than the maximum historical volume
# prod_3[, max_wkly_volume := max(wkly_volume) * 1.15] 
# prod_3[, T_volume := log(wkly_volume/(max_wkly_volume - wkly_volume))]

# weekly price
prod_3[,wkly_price := mean(prod_unit_price), by = .(wk_index)]
# weekly discount
prod_3[,wkly_dct := -sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty), by = .(wk_index)]

# retrieve data and merge to attributes
data3 = unique(prod_3[,c("tran_wk", "wk_index", "wkly_volume", "wkly_price", "wkly_dct")])
data3 = merge(data3,attributes, all.x = TRUE, by = "wk_index")

data3[, avg_volume := mean(wkly_volume)]

# exclude flyer imapct 
data3[, flyer_volume := wkly_volume*Flyer]
data3[, flyer_volume_avg := ifelse(is.na(flyer_volume),0,mean(flyer_volume, na.rm = TRUE)-avg_volume)]

# exclude email imapct 
data3[, email_volume := wkly_volume*Email/200000]
data3[, email_volume_avg := ifelse(is.na(email_volume),0,mean(email_volume, na.rm = TRUE)-avg_volume)]

# exclude Store Display imapct 
data3[, sd_volume := wkly_volume*data3$`Store Display`]
data3[, sd_volume_avg := ifelse(is.na(sd_volume),0,mean(sd_volume, na.rm = TRUE)-avg_volume)]

# exclude Web Display imapct 
data3[, wd_volume := wkly_volume*data3$`Web Display`/280000]
data3[, wd_volume_avg := ifelse(is.na(wd_volume),0,mean(wd_volume, na.rm = TRUE)-avg_volume)]

# final target variable
# data3[, volume := wkly_volume-flyer_volume_avg-email_volume_avg-sd_volume_avg-wd_volume_avg]
data3[, volume := wkly_volume-email_volume_avg-sd_volume_avg-wd_volume_avg]
data3[,flyer_01 := ifelse(is.na(Flyer),0,1)]

formula = volume ~ wkly_price + wkly_dct + wk_index + flyer_01 + TV + Radio + data3$`Paid Search` + seas_index + holiday_index
lm <- lm(formula, data = data3)
summary(lm)

# according to prediction, get other advertising impact
data3[, tv_volume := 39.68*TV]
data3[, radio_volume := -34.26*Radio]
data3[, ps_volume := 0.00001219*data3$`Paid Search`]
data3[, flyer_01_volume := 6.211*flyer_01]

# final duetos outcomes
data_3 = data3[,c("tran_wk","wkly_volume","tv_volume","radio_volume","ps_volume",
                  "flyer_01_volume","email_volume_avg","sd_volume_avg","wd_volume_avg")]
# export the data
write.csv(data_3, 'data_138936953.csv',row.names = FALSE)

# end-------------------------------------------------------------------------------------------------------------------------------
