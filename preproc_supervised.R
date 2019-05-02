#-------------------
library(comtradr)
library(reshape)
library(dplyr)
#-------------------

# Selection varibles
years = seq(from = 1995, to = 2017)
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
   
  # At the end of the "for" cycle, "df" will be the same of "data_year", with the difference that
  # it will include all the years that we want
   if (y==min(years)){
     df <- data_year
   }
   if(y>min(years)){
     df <- full_join(df,data_year) # This attaches the new "data_year" to the existing "df"
   }
}

# We obtain the dataset as we want it by casting df.
# Notice that if a good [column], has no value for some year [row], the corresponing cell is NA
data_alltimes <- cast(df, year ~ commodity_code)

# Labeling row names with the corresponding year and removing the first column
row.names(data_alltimes)<- data_alltimes$year
data_alltimes <- data_alltimes[,2:length(data_alltimes)]

