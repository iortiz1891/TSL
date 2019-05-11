#-------------------
library(tidyr)
library(dplyr)
library(ggplot2)
#-------------------

# Import data
df <- read.csv("Data/AG41995-1999.csv", colClasses = c("character", "integer", "character", "numeric"))
df <- bind_rows(df, read.csv("Data/AG42000-2004.csv", colClasses = c("character", "integer", "character", "numeric")))
df$reporter_iso <- as.factor(df$reporter_iso)
df$commodity_code <- as.factor(df$commodity_code)

#Casting df 
casted_data <- spread(df, key="commodity_code", value = "trade_value_usd")

# Compute Number of zeros by product
zeroes_product <-aggregate(casted_data[3:length(casted_data)], by=list(year=casted_data$year), 
                 FUN= function(x){sum(is.na(x))} )

diff_zeroes_product <- (zeroes_product[2:nrow(zeroes_product),2:length(zeroes_product)]) - 
  (zeroes_product[1:nrow(zeroes_product)-1,2:length(zeroes_product)])
mean_change_zeroes_product <- apply(diff_zeroes_product, 2, mean )

# Plot the histogram of average changes in number of zeros by product over 10 years
ggplot() + geom_histogram(aes(x = mean_change_zeroes_product), binwidth = 0.5, fill = "blue") +
  labs(x = "Mean difference by product", title = "Distribution of changes in number of zero exports over 10 years") + theme_bw()

# Compute number of zeros by country
zeroes_country <- casted_data[1:2]
zeroes_country["Count"] <- apply(casted_data[3:length(casted_data)], 1, FUN= function(x){sum(is.na(x))}) / 
  length(levels(df$commodity_code))

# Plot the histogram of percentage of zero exports per country
ggplot(zeroes_country) + geom_histogram(aes(x = Count)) +
  labs(x = "Percentage of zero exports", title = "Distribution of percentage of zero exports per country") + theme_bw()

ggplot(zeroes_country) + geom_density(aes(x = Count, group = year, color = as.factor(year)), bw = 0.075) +
  labs(x = "Percentage of zero exports", title = "Distribution of percentage of zero exports per country") + theme_bw()


# Explore feature distributions
k=round(runif(1, min = 1, max = ncol(log_data)))

log_data <- log(casted_data[c(-1,-2)])
log_data[is.na(log_data)] <- 0
ggplot()+ geom_histogram(aes(x=log_data[[k]]))

export_frac <- apply(casted_data[-1], 1, sum)

ggplot()+ geom_histogram(aes(x=log(casted_data[[k]])))

