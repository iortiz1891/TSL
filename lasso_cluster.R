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

# Get cluster information
clust <- read.csv("Data/OOUT.csv", col.names = c("reporter_iso", "cluster"))
data_clust <- na.omit(left_join(clust, data_theta_gdp, by = "reporter_iso"))

# LASSO
for (c in unique(clust$cluster)){
  
  # Drop features of zero variance
  data <- filter(data_clust, cluster == c)
  #data <- select(data, var(select(data, -c("reporter_iso", "cluster","year","gdp"))) == 0)
  
  xtrain_gdp <- as.matrix(select(data, -c("reporter_iso", "cluster","year","gdp")))
  ytrain_gdp <- as.matrix(select(data, c("gdp")))
  
  if (TRUE){
    xtrain_gdp <- log(xtrain_gdp + 1)
    ytrain_gdp <- log(ytrain_gdp + 1)
    grid=10^seq(2, -5,length=100) #Grid of lambdas
  } else{
    grid=10^seq(10, 1,length=100) #Grid of lambdas
  }
  
  lasso_gdp = glmnet(xtrain_gdp, ytrain_gdp, alpha=1, lambda=grid, standardize = FALSE) #Estimating betas, at varius lambdas
  plot(lasso_gdp, "lambda", label = TRUE)
  
  cv_gdp <- cv.glmnet(xtrain_gdp, ytrain_gdp, alpha=1, standardize = FALSE) #Do CV
  plot(cv_gdp)
  
  bestlam_gdp=cv_gdp$lambda.1se
  lasso_coef_gdp=predict(cv_gdp,type="coefficients",s=bestlam_gdp)[1:ncol(xtrain_gdp),]
  print(lasso_coef_gdp[lasso_coef_gdp != 0])
  lasso_gdp = glmnet(xtrain_gdp, ytrain_gdp, alpha=1, lambda=bestlam_gdp, standardize = FALSE) 
  print(lasso_gdp)
  
}


