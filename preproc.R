
library(comtradr)
library(reshape)
#-------------------

# Extract raw data from UN Comtrade API
data_AG2 <- ct_search(reporters = "all", 
                      partners = "World", 
                      trade_direction = "export",
                      start_date = 2010,
                      end_date = 2010,
                      commod_codes = "AG2")

# Extract reporters list and code/name association
reporters <- unique(data.frame(data_AG2$reporter, data_AG2$reporter_code))
names(reporters) <- c("name", "code")
reporters <- reporters[order(reporters$code),]
write.table(reporters, file = "reporters.txt")

x <- data.frame(q1_AG2$reporter,q1_AG2$commodity_code,q1_AG2$trade_value_usd)
X <- cast(x, q1_AG2.reporter ~ q1_AG2.commodity_code)

#write.csv(X , file = "raw_data.csv")
