
library(comtradr)
library(reshape)
#-------------------


# Extract raw data from UN Comtrade API
for (i in 1990:2005){
data_AG2 <- ct_search(reporters = "all", 
                      partners = "World", 
                      trade_direction = "export",
                      start_date = i,
                      end_date = i,
                      commod_codes = "AG2")

# Extract reporters list and code/name association
reporters <- unique(data.frame(data_AG2$reporter, data_AG2$reporter_code))
names(reporters) <- c("name", "code")
reporters <- reporters[order(reporters$code),]
#write.table(reporters, file = "reporters.txt")

#assign(paste("x", i, sep = "_"), data.frame(q1_AG2$reporter,q1_AG2$commodity_code,q1_AG2$trade_value_usd)) 
assign(paste("X", i, sep = "_"), cast(x, q1_AG2.reporter ~ q1_AG2.commodity_code))

}


row.names(X)<- X$q1_AG2.reporter
X <- X[,3:99]
X [is.na(X)] <- 0


#write.csv(X , file = "raw_data.csv")
#paste("x", i, sep = "_")
#print(paste("x", i, sep = "_"))

      