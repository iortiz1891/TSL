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
zeroes_product <- aggregate(casted_data[3:length(casted_data)], by=list(year=casted_data$year), 
                 FUN = function(x){sum(is.na(x))} )

diff_zeroes_product <- (zeroes_product[2:nrow(zeroes_product),2:length(zeroes_product)]) - 
  (zeroes_product[1:nrow(zeroes_product)-1,2:length(zeroes_product)])
mean_change_zeroes_product <- apply(diff_zeroes_product, 2, mean )

# Plot the histogram of average changes in number of zeros by product over 10 years
ggplot() + geom_histogram(aes(x = mean_change_zeroes_product), binwidth = 0.5, fill = "blue") +
  labs(x = "Mean difference by product", title = "Distribution of changes in number of zero exports over 10 years") + theme_bw()

# Compute number of zeros by country
zeroes_country <- casted_data[1:2]
zeroes_country["Count"] <- apply(casted_data[3:length(casted_data)], 1, FUN = function(x){sum(is.na(x))}) / 
  length(levels(df$commodity_code))

# Plot the histogram of percentage of zero exports per country
ggplot(zeroes_country) + geom_density(aes(x = Count, group = year, color = as.factor(year)), bw = 0.075) +
  labs(x = "Percentage of zero exports", title = "Distribution of percentage of zero exports per country") + 
  scale_color_brewer(palette = "RdBu") + theme_bw()

# Explore feature distributions
filled_data <- casted_data
filled_data[is.na(filled_data)] <- 0

# Plot distribution of some random products
ggplot(gather(filled_data, 3:ncol(filled_data), key = "commodity_code", value = "trade_value_usd")) + 
  geom_density(aes(x = log(trade_value_usd + 1), group = commodity_code)) + theme_bw()

# Total exports 
tot_exp <- read.csv("Data/total1995-1999.csv", colClasses = c("character", "integer", "numeric"))
tot_exp <- bind_rows(tot_exp, read.csv("Data/total2000-2004.csv", colClasses = c("character", "integer", "numeric")))
tot_exp$reporter_iso <- as.factor(tot_exp$reporter_iso)

# Plot the histogram of total exports per country
ggplot(tot_exp) + geom_density(aes(x = trade_value_usd, group = as.factor(year), color = as.factor(year)), bw = 5e9) +
  labs(x = "Total exports", title = "Distribution of total exports per country per year") + 
  scale_color_brewer(palette = "RdBu") + theme_bw()

ggplot(tot_exp) + geom_density(aes(x = log(trade_value_usd), group = as.factor(year), color = as.factor(year)), bw = 0.5) +
  labs(x = "Total exports", title = "Distribution of total exports per country per year") + 
  scale_color_brewer(palette = "RdBu") + theme_bw()

# Plot GDP vs Tot Exp
# Selecting only the interesting columns in GDP: !!!has to be made general!!!
selected_gdp <- select(gdp, 1, "1995":"2004")

# Making the GDP dataframe coherent with the Exports' one
selected_gdp <- gather(selected_gdp, 2:length(selected_gdp), key="year", value = "gdp")
colnames(selected_gdp)[1] <- "reporter_iso"

# Removing NA raws, i.e. countries and years for which we don't have the GDP information
selected_gdp <- na.omit(selected_gdp)

# casted_data and selected_gdp have very different number of observation: the following is done to fix this problem
selected_gdp$year <- as.factor(selected_gdp$year) 
filled_data$year <- as.factor(filled_data$year)
data <- na.omit(left_join(filled_data, selected_gdp, by=c("reporter_iso","year")))

ggplot(data = data) + geom_point(aes(x = `2204`, y = gdp, group = year, colour = year)) + theme_bw()

ggplot(data = data) + geom_point(aes(x = log(`2204` + 1), y = log(gdp + 1), group = year, colour = year)) + theme_bw()

tot_exp$year <- as.factor(tot_exp$year)
tot_exp_gdp <- na.omit(left_join(tot_exp, selected_gdp, by=c("reporter_iso","year")))

ggplot(tot_exp_gdp) + geom_point(aes(x = trade_value_usd, y = gdp, group = year, colour = year)) + 
  labs(x = "Total exports", title = "Total exports vs gdp") + theme_bw()

ggplot(tot_exp_gdp) + geom_point(aes(x = log(trade_value_usd + 1), y = log(gdp + 1), group = year, colour = year)) + 
  labs(x = "Total exports", title = "Total exports vs gdp in log") + theme_bw()

# Plot GDP growth vs Tot Exp
grw <- (- gdp[2:(ncol(gdp)-1)] + gdp[3:ncol(gdp)]) / gdp[2:(ncol(gdp)-1)]
grw["reporter_iso"] <- gdp$Country
grw <- na.omit(gather(grw, 1:(ncol(grw)-1), key = "year", value = "growth"))

grw$year <- as.factor(grw$year) 
data_grw <- na.omit(left_join(filled_data, grw, by=c("reporter_iso","year")))
tot_exp_grw <- na.omit(left_join(tot_exp, grw, by=c("reporter_iso","year")))

ggplot(tot_exp_grw) + geom_point(aes(x = trade_value_usd, y = growth, group = year, colour = year)) + theme_bw()

ggplot(tot_exp_grw) + geom_point(aes(x = log(trade_value_usd + 1), y = growth, group = year, colour = year)) + theme_bw()

ggplot(data_grw) + geom_point(aes(x = `2204`, y = growth, group = year, colour = year)) + theme_bw()

ggplot(data_grw) + geom_point(aes(x = log(`2204` + 1), y = growth, group = year, colour = year)) + theme_bw()
