#-------------------
library(comtradr)
library(reshape)
library(dplyr)
#-------------------

# Selection varibles
years = seq(from = 1960, to = 2015)
country = "Italy"

# Extract raw data from UN Comtrade API
for (y in years){
  data_year <- ct_search(reporters = country, 
                        partners = "World", 
                        trade_direction = "export",
                        start_date = y,
                        end_date = y,
                        commod_codes = "AG4")
  
  # Drop the columns of the dataframe
  data_year <- select(data_year, c(year, commodity_code, trade_value_usd))

}

