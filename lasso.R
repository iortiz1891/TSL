#-------------------
library(glmnet)
library(tidyr)
library(dplyr)
#-------------------


#Uncomment the following line if you don't have saved the data frame in your workspace
#df <- read.csv("Data/AG41995-1999.csv")

#Casting df 
casted_data <- spread(df, key="commodity_code", value = "trade_value_usd")
#Joining reporter_iso and year into one column
casted_data <- unite(casted_data, "reporter_iso", "year", col = "reporter_iso", sep = "_")


#Selecting only the interesting columns in GDP: !!!has to be made general!!!
selected_gdp <- select(gdp, 1, grep("1995", colnames(gdp)):grep("1999", colnames(gdp)))

#Making the GDP dataframe coherent with the Exports' one
selected_gdp <- gather(selected_gdp, 2:length(selected_gdp), key="year", value = "gdp")
selected_gdp <- unite(selected_gdp, "Country", "year", col = "reporter_iso", sep = "_")

#Removing NA raws, i.e. countries and years for which we don't have the GDP information
selected_gdp <- na.omit(selected_gdp)

#casted_data and selected_gdp have very different number of observation: the following is done to fix this problem
row.names(selected_gdp)<- selected_gdp$reporter_iso
row.names(casted_data)<- casted_data$reporter_iso
common_observation <- intersect(casted_data[,1],selected_gdp[,1])


xtrain <- select(casted_data[common_observation, ], -c("reporter_iso"))
ytrain <- select(selected_gdp[common_observation, ], gdp)
#As far as i have understood, glmnet takes matrix instead of dataframes as argument
xtrain <- data.matrix(xtrain)
ytrain <- data.matrix(ytrain)
#Setting NA in predictors at 0 (NA means that that good as not been exported that year by that country)
xtrain[is.na(xtrain)] <- 0

#Scaling of xtrain in the naive way: centering columns at zero and dividing them by stddev
xtrain_scaled <- scale(xtrain, center = TRUE, scale = TRUE)


#!!!!!--FROM NOW LASSO--!!!!!


#Dividing our train dataset in "train" and "test", at random. We need this to pick lambda.
#Later, we will use this whole dataset as training set, and the "test" ones will be the future years
set.seed(10)
train=sample(1:nrow(xtrain_scaled), nrow(xtrain_scaled)/2) #Dividing dataset in two subsets of equal number of observations, at random
test=(-train)

grid=10^seq(10,-2,length=100) #Grid of lambdas
lasso.mod=glmnet(xtrain_scaled[train,],ytrain[train,],alpha=1,lambda=grid) #Estimating betas from the train subset, at varius lambdas
#plot(lasso.mod)

cv.out <- cv.glmnet(xtrain_scaled[train,],ytrain[train,],alpha=1) #Do CV on the train subset
#plot(cv.out)
bestlam=cv.out$lambda.min

lasso.pred <- predict(lasso.mod,s=bestlam,newx=xtrain_scaled[test,]) #Predicts on the test subset
#mean((lasso.pred-ytrain[test,])^2)

#Not so clear here
out=glmnet(xtrain_scaled,ytrain,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(xtrain_scaled),]
lasso.coef
lasso.coef[lasso.coef!=0]




