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

# end ------------------------------------------------------------------------------------------------------------------------






