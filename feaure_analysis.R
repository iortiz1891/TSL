#-------------------
library(tidyr)
library(dplyr)
#-------------------

#Uncomment the following line if you don't have saved the data frame in your workspace
#df <- read.csv("Data/AG41995-1999.csv")

#Casting df 
casted_data <- spread(df, key="commodity_code", value = "trade_value_usd")

zeroes<-aggregate(casted_data[3:length(casted_data)], by=list(year=casted_data$year), 
                 FUN= function(x){sum(is.na(x))} )
