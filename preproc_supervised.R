#-------------------
library(comtradr)
library(tidyr)
library(dplyr)
library(reshape)
#-------------------

# Selection variables
first_year <- 1995
last_year <- first_year + 4 # Max years in a query is 5

total_exports <- ct_search(reporters = "all", 
                       partners = "World", 
                       trade_direction = "export",
                       start_date = first_year,
                       end_date = last_year)

# Build list of countries
countries <- distinct(total_exports, reporter_iso, reporter)

# Extract raw data from UN Comtrade API
for (y in years){
  
  # Check you are not going over the query limit (100 per hour)
  if (ct_get_remaining_hourly_queries() < 5){
    stop(paste('WARNING: Code stopped because close to hourly limit! Reset time =', ct_get_reset_time() ))
  }
  
  data_year <- ct_search(reporters = "all", 
                        partners = "World", 
                        trade_direction = "export",
                        start_date = y,
                        end_date = y,
                        commod_codes = "AG2")
  
  # Build list of codes descriptions
  commodities <- distinct(data_year, commodity_code, commodity)
  
  # Drop the columns of the dataframe
  data_year <- select(data_year, c(reporter_iso, year, commodity_code, trade_value_usd))
   
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

# ONE COUNTRY, MANY YEARS:
# data_alltimes <- cast(df, year ~ commodity_code)

# ALL COUNTRIES:
data_alltimes <- cast(df, reporter_iso+year ~ commodity_code)

# Labeling row names with the corresponding year and removing the first column

# row.names(data_alltimes)<- data_alltimes$year
# data_alltimes <- data_alltimes[,2:length(data_alltimes)]

# WHEN YOU HAVE ALL COUNTRIES:
row.names(data_alltimes)<- paste(data_alltimes$reporter_iso, data_alltimes$year, sep="_")
data_alltimes <- data_alltimes[,3:length(data_alltimes)]



