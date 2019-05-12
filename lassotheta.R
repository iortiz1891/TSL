#-------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmnet)
#-------------------

##########
#This piece of code has been copy-pasted from feature_analysis: it generates the variables we need
df <- read.csv("Data/AG41995-1999.csv", colClasses = c("character", "integer", "character", "numeric"))
df <- bind_rows(df, read.csv("Data/AG42000-2004.csv", colClasses = c("character", "integer", "character", "numeric")))
df$reporter_iso <- as.factor(df$reporter_iso)
df$commodity_code <- as.factor(df$commodity_code)
casted_data <- spread(df, key="commodity_code", value = "trade_value_usd")
filled_data <- casted_data
filled_data[is.na(filled_data)] <- 0
selected_gdp <- select(gdp, 1, "1995":"2004")
selected_gdp <- gather(selected_gdp, 2:length(selected_gdp), key="year", value = "gdp")
colnames(selected_gdp)[1] <- "reporter_iso"
selected_gdp <- na.omit(selected_gdp)
grw <- (- gdp[2:(ncol(gdp)-1)] + gdp[3:ncol(gdp)]) / gdp[2:(ncol(gdp)-1)]
grw["reporter_iso"] <- gdp$Country
grw <- na.omit(gather(grw, 1:(ncol(grw)-1), key = "year", value = "growth"))
##########

#Creating a dataframe with the same structure of filled_data.
#Each entry is 0 if the corresponding one in filled_data is =/= 0, 1 otherwise (this is the "1-theta").
x<-filled_data[3:ncol(filled_data)]
x[filled_data[3:ncol(filled_data)]==0]<-1
x[filled_data[3:ncol(filled_data)]!=0]<-0
colnames(x)<-paste("1-theta", colnames(x), sep = "_")

#We merge these two datasets, after the logarithm of the first one has been taken
log_filled_data<-filled_data
log_filled_data[3:ncol(log_filled_data)]<- log(log_filled_data[3:ncol(log_filled_data)]+1)
filled_data_theta <- bind_cols(filled_data,x)

#data_theta_gdp is structured as follows:
#[1:2] reporter_iso and year
#[3:1340] commodity_code (es "0101", "2304"...)
#[1341:2678] corresponing 1-theta (es "1-theta_0101", "1-theta_2304")
#[2679] gdp
selected_gdp$year <- as.factor(selected_gdp$year)
filled_data_theta$year <- as.factor(filled_data_theta$year)
data_theta_gdp <- na.omit(left_join(filled_data_theta, selected_gdp, by=c("reporter_iso","year")))

#data_theta_grw is structured as data_theta_gdp with "grw" instead of "gdp" in the last column
grw$year <- as.factor(grw$year) 
data_theta_grw <- na.omit(left_join(filled_data_theta, grw, by=c("reporter_iso","year")))

#Now, LASSO
#GDP
xtrain_gdp <- select(data_theta_gdp, -c("reporter_iso","year"))
xtrain_gdp <- as.matrix(xtrain_gdp)
ytrain_gdp <- data.matrix(log(data_theta_gdp[[ncol(data_theta_gdp)]]))
grid=10^seq(10,-2,length=100) #Grid of lambdas
lasso_gdp=glmnet(xtrain_gdp,ytrain_gdp, alpha=1,lambda=grid) #Estimating betas, at varius lambdas
plot(lasso_gdp)
cv_gdp <- cv.glmnet(xtrain_gdp,ytrain_gdp,alpha=1) #Do CV
plot(cv_gdp)
bestlam_gdp=cv_gdp$lambda.min
lasso_coef_gdp=predict(lasso_gdp,type="coefficients",s=bestlam_gdp)[1:ncol(xtrain_gdp),]

#GRW (not working? cannot take log of negative grw)
xtrain_grw <- select(data_theta_grw, -c("reporter_iso","year"))
xtrain_grw <- as.matrix(xtrain_grw)
ytrain_grw <- data.matrix(data_theta_grw[[ncol(data_theta_grw)]])
grid=10^seq(10,-2,length=100) #Grid of lambdas
lasso.grw=glmnet(xtrain_grw,ytrain_grw, alpha=1,lambda=grid) #Estimating betas from the train subset, at varius lambdas
plot(lasso.grw)





