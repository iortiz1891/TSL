#-------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmnet)
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

# We merge these two datasets
filled_data_theta <- bind_cols(filled_data, dummies)

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

#Now, LASSO
#GDP
xtrain_gdp <- as.matrix(select(data_theta_gdp, -c("reporter_iso","year","gdp")))
ytrain_gdp <- as.matrix(select(data_theta_gdp, c("gdp")))

if (TRUE){
  xtrain_gdp <- log(xtrain_gdp + 1)
  ytrain_gdp <- log(ytrain_gdp + 1)
  grid=10^seq(2, -5,length=100) #Grid of lambdas
} else{
  grid=10^seq(10, 1,length=100) #Grid of lambdas
}

lasso_gdp = glmnet(xtrain_gdp, ytrain_gdp, alpha=1, lambda=grid, standardize = FALSE) #Estimating betas, at varius lambdas
plot(lasso_gdp, "norm", label = TRUE)
plot(lasso_gdp, "lambda", label = TRUE)
plot(lasso_gdp, "dev", label = TRUE)

cv_gdp <- cv.glmnet(xtrain_gdp, ytrain_gdp, alpha=1, standardize = FALSE) #Do CV
plot(cv_gdp)

bestlam_gdp=cv_gdp$lambda.1se
lasso_coef_gdp=predict(cv_gdp,type="coefficients",s=bestlam_gdp)[1:ncol(xtrain_gdp),]
print(lasso_coef_gdp[lasso_coef_gdp != 0])
lasso_gdp = glmnet(xtrain_gdp, ytrain_gdp, alpha=1, lambda=bestlam_gdp, standardize = FALSE) 
print(lasso_gdp)

# GRW
xtrain_grw <- as.matrix(select(data_theta_grw, -c("reporter_iso","year","growth")))
ytrain_grw <- as.matrix(select(data_theta_grw, c("growth")))
grid = 10^seq(2, -6,length=100) #Grid of lambdas
lasso_grw = glmnet(xtrain_grw, ytrain_grw, alpha=1, lambda=grid, standardize = TRUE) #Estimating betas, at varius lambdas
plot(lasso_grw, "norm", label = TRUE)
plot(lasso_grw, "lambda", label = TRUE)
plot(lasso_grw, "dev", label = TRUE)

cv_grw <- cv.glmnet(xtrain_grw, ytrain_grw, alpha=1, standardize = TRUE, nfolds = 5) #Do CV
plot(cv_grw)

bestlam_grw <- cv_grw$lambda.1se
lasso_coef_grw <- predict(lasso_grw, type="coefficients", s=bestlam_grw)[1:ncol(xtrain_grw),]
print(lasso_coef_grw[lasso_coef_grw != 0])




