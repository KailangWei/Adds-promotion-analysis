##### MA Project 4: DueTos

# split the whole dataset into datasets of the three products
prod_1 = data[prod_id=="138936951"]
prod_2 = data[prod_id=="138936952"]
prod_3 = data[prod_id=="138936953"]

attributes = attributes[,-1]


######## product 138936951

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


######## product 138936952

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


######## product 138936953

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







