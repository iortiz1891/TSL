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

# Select gdp data for the same period and normalize by log
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

# Normalize by taking the logarithm
filled_data[3:ncol(filled_data)]<- log(filled_data[3:ncol(filled_data)] + 1)
selected_gdp$gdp <- log(selected_gdp$gdp + 1)

# Join gdp and trade data
selected_gdp$year <- as.factor(selected_gdp$year)
selected_gdp$reporter_iso <- as.character(selected_gdp$reporter_iso)
filled_data$year <- as.factor(filled_data$year)
data_gdp <- na.omit(left_join(filled_data, selected_gdp, by=c("reporter_iso","year")))

selected_grw$year <- as.factor(selected_grw$year) 
selected_grw$reporter_iso <- as.character(selected_grw$reporter_iso)
data_grw <- na.omit(left_join(filled_data, selected_grw, by=c("reporter_iso","year")))

# Now, LASSO
# GDP
xtrain_gdp <- as.matrix(select(data_gdp, -c("reporter_iso","year","gdp")))
ytrain_gdp <- as.matrix(select(data_gdp, "gdp"))
grid=10^seq(3, -5,length=100) #Grid of lambdas
lasso_gdp=glmnet(xtrain_gdp, ytrain_gdp, alpha=1, lambda=grid, standardize = TRUE) #Estimating betas, at varius lambdas
plot(lasso_gdp, "norm", label = TRUE)
plot(lasso_gdp, "lambda", label = TRUE)
plot(lasso_gdp, "dev", label = TRUE)

cv_gdp <- cv.glmnet(xtrain_gdp, ytrain_gdp, alpha=1, standardize = TRUE, nfolds = 10) #Do CV
plot(cv_gdp)

bestlam_gdp=cv_gdp$lambda.1se
lasso_coef_gdp=predict(lasso_gdp,type="coefficients",s=bestlam_gdp)[1:ncol(xtrain_gdp),]
print(lasso_coef_gdp[lasso_coef_gdp != 0])

# GRW
xtrain_grw <- as.matrix(select(data_grw, -c("reporter_iso","year","growth")))
ytrain_grw <- as.matrix(select(data_grw, c("growth")))
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



