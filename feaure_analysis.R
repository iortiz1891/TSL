#-------------------
library(tidyr)
library(dplyr)
library(ggplot2)
#-------------------

#Uncomment the following line if you don't have saved the data frame in your workspace
#df <- read.csv("Data/AG41995-1999.csv")

#Casting df 
casted_data <- spread(df, key="commodity_code", value = "trade_value_usd")

zeroes<-aggregate(casted_data[3:length(casted_data)], by=list(year=casted_data$year), 
                 FUN= function(x){sum(is.na(x))} )

#zeroes_melted <- gather(zeroes,2:length(zeroes) ,key = "commodity_code", value="zeroes")
#To plot zeroes_melted, reduce its dimension!!!
#ggplot(zeroes_melted) + geom_line(aes(x=year,y=zeroes,color=commodity_code))

diff_zeroes <- (zeroes[2:nrow(zeroes),2:length(zeroes)])-(zeroes[1:nrow(zeroes)-1,2:length(zeroes)])
media <- apply(diff_zeroes, 2, mean )
ggplot()+ geom_histogram(aes(x=media))

k=round(runif(1, min = 1, max = ncol(log_data)))

log_data <- log(casted_data[c(-1,-2)])
log_data[is.na(log_data)] <- 0
ggplot()+ geom_histogram(aes(x=log_data[[k]]))

export_frac <- apply(casted_data[-1], 1, sum)

ggplot()+ geom_histogram(aes(x=log(casted_data[[k]])))




