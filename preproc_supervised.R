#-------------------
library(comtradr)
library(reshape)
library(dplyr)
#-------------------

# Selection varibles
years = seq(from = 2000, to = 2000)
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
   dummy <- melt.data.frame(data_year)
   dummy <- cast(dummy, variable ~ commodity_code)
}
