#-------------------
library(tidyr)
library(dplyr)
#-------------------

# Import from csv
gdp <- read.csv("Data/gdp_data.csv", na.strings = c("", ".."))
names(gdp) <- c("Series Name", "Series Code", "Country Name", "Country", 1960:2018)
gdp <- select(gdp, -c("Series Name", "Series Code", "Country Name"))
gdp <- gdp[!(is.na(gdp$Country)),]
