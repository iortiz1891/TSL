#-------------------
library(comtradr)
library(tidyr)
library(dplyr)
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
print("Got for the following countries:")
for(i in seq(from=1,to=nrow(countries), by=5)){
  
  # Check you are not going over the query limit (100 per hour)
  if (ct_get_remaining_hourly_queries() < 5){
    stop(paste('WARNING: Code stopped because close to hourly limit! Reset time =', ct_get_reset_time() ))
  }
  
  data <- ct_search(reporters = countries[i:min(i+4,nrow(countries)),2], 
                    partners = "World", 
                    trade_direction = "export",
                    start_date = first_year,
                    end_date = last_year,
                    commod_codes = "AG4")

  # Drop the columns of the dataframe
  data <- select(data, c(reporter_iso, year, commodity_code, commodity, trade_value_usd))
   
  # At the end of the "for" cycle, "df" will be the same of "data_year", with the difference that
  # it will include all the years that we want
   if (i==1){
     df <- data
   } else {
     df <- full_join(df,data) # This attaches the new "data_year" to the existing "df"
   }
  
  # Status update
  print(countries[i:min(i+4,nrow(countries)),2])
}

# Build list of codes descriptions
commodities <- distinct(df, commodity_code, commodity)
df <- select(df, -(commodity_code))

# Save df to csv
write.csv(df, file = paste("Data/AG4", first_year, "-", last_year, ".csv", sep = ""))
  
# We obtain the dataset as we want it by casting df.
# Notice that if a good [column], has no value for some year [row], the corresponing cell is NA

# ONE COUNTRY, MANY YEARS:
# data_alltimes <- cast(df, year ~ commodity_code)

# ALL COUNTRIES:
# data_alltimes <- spread(df, reporter_iso + year ~ commodity_code)

# Labeling row names with the corresponding year and removing the first column

# row.names(data_alltimes)<- data_alltimes$year
# data_alltimes <- data_alltimes[,2:length(data_alltimes)]

# WHEN YOU HAVE ALL COUNTRIES:
# row.names(data_alltimes)<- paste(data_alltimes$reporter_iso, data_alltimes$year, sep="_")
# data_alltimes <- data_alltimes[,3:length(data_alltimes)]



