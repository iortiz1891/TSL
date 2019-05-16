#-------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmnet)
library(stringr)
library(gglasso)
#-------------------

# Import data from csv
df <- read.csv("Data/AG41995-1999.csv", colClasses = c("character", "integer", "character", "numeric"))
df <- bind_rows(df, read.csv("Data/AG42000-2004.csv", colClasses = c("character", "integer", "character", "numeric")))

# Cast data to wide form in order to fill the NA with 0
filled_data <- spread(df, key="commodity_code", value = "trade_value_usd")
filled_data[is.na(filled_data)] <- 0

# Select gdp data for the same period
selected_gdp <- select(gdp, 1, "1995":"2004")
selected_gdp <- gather(selected_gdp, 2:length(selected_gdp), key="year", value = "gdp")
colnames(selected_gdp)[1] <- "reporter_iso"
selected_gdp <- na.omit(selected_gdp)

# Compute growth data
grw <- (- gdp[2:(ncol(gdp)-1)] + gdp[3:ncol(gdp)]) / gdp[2:(ncol(gdp)-1)]
grw["reporter_iso"] <- gdp$Country
grw <- grw[c(length(grw), 1:length(grw)-1)]
selected_grw <- select(grw, 1, "1995":"2004")
selected_grw <- na.omit(gather(selected_grw, 2:ncol(selected_grw), key = "year", value = "growth"))

# Creating a dataframe with the same structure of filled_data (dummy variables).
# Each entry is 0 if the corresponding one in filled_data is != 0, 1 otherwise (this is the "1-theta").
dummies <- filled_data[3:ncol(filled_data)]
dummies[filled_data[3:ncol(filled_data)] == 0] <- 1
dummies[filled_data[3:ncol(filled_data)] != 0] <- 0
colnames(dummies) <- paste("1-theta", colnames(dummies), sep = "_")
#dummies[] <- lapply(dummies, as.factor)

# We merge these two datasets, and normalize by taking the logarithm
filled_data[3:ncol(filled_data)]<- log(filled_data[3:ncol(filled_data)] + 1)
filled_data_theta <- bind_cols(filled_data, dummies)
selected_gdp$gdp <- log(selected_gdp$gdp)

# data_theta_gdp is structured as follows:
# [1:2] reporter_iso and year
# [3:1340] commodity_code (es "0101", "2304"...)
# [1341:2678] corresponing 1-theta (es "1-theta_0101", "1-theta_2304")
# [2679] gdp
selected_gdp$year <- as.factor(selected_gdp$year)
selected_gdp$reporter_iso <- as.character(selected_gdp$reporter_iso)
filled_data_theta$year <- as.factor(filled_data_theta$year)
data_theta_gdp <- na.omit(left_join(filled_data_theta, selected_gdp, by=c("reporter_iso","year")))

# data_theta_grw is structured as data_theta_gdp with "grw" instead of "gdp" in the last column
selected_grw$year <- as.factor(selected_grw$year) 
selected_grw$reporter_iso <- as.character(selected_grw$reporter_iso)
data_theta_grw <- na.omit(left_join(filled_data_theta, selected_grw, by=c("reporter_iso","year")))

#Here we create the group condition for GGLASSO
x <- c(str_extract(colnames(data_theta_gdp[3:1340]), "\\d{1,2}"), 
       str_extract(colnames(data_theta_gdp[1341: (ncol(data_theta_gdp)-1)]), "\\d\\D{1,7}\\d{1,2}" ))
group <- x
group[1]=1
j=as.integer(group[1])
for (i in (2:(length(x)))) {
  if(x[i]==x[i-1]){
    group[i]=j
  } 
  else {
    j=j+1
    group[i]=j
  }
}
group<-as.integer(group)


#Now, GGLASSO
#GDP
xtrain_gdp <- as.matrix(select(data_theta_gdp, -c("reporter_iso","year","gdp")))
ytrain_gdp <- as.matrix(select(data_theta_gdp, c("gdp")))
if (TRUE){
  xtrain_gdp <- log(xtrain_gdp + 1)
  ytrain_gdp <- log(ytrain_gdp + 1)
 # grid=10^seq(2, -5,length=100) #Grid of lambdas
} else{
 # grid=10^seq(10, 1,length=100) #Grid of lambdas
}
gglasso_gdp = gglasso(xtrain_gdp, ytrain_gdp, group=group)
cv_gdp=cv.gglasso(xtrain_gdp, ytrain_gdp, group=group, nfold=5)
prova=gglasso(xtrain_gdp, ytrain_gdp, group=group, lambda = cv_gdp$lambda.1se)

print(prova)
k<-unique(str_extract(rownames(prova$beta)[(1:ncol(xtrain_gdp))[prova$beta!=0]], "\\d{1,2}"))
print(k)

#print(prova)

plot(cv_gdp)

