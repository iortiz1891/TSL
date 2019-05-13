#-------------------
library(comtradr)
library(reshape)
#-------------------

# Extract raw data from UN Comtrade API
C_ALL <-c()
for (i in 1996:2005){
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
#commodity <- unique(data.frame(data_AG2$commodity, data_AG2$commodity_code))

q <- data.frame(data_AG2$reporter,data_AG2$commodity_code,data_AG2 $trade_value_usd)

C_ALL<-append(C_ALL,unique(data_AG2$reporter))
X <- assign(paste("X", i, sep = "_"), cast(q, data_AG2.reporter ~ data_AG2.commodity_code))

#write.csv(X , file = paste(i, "x.csv", sep = "_"))
}



#row.names(X)<- X$q1_AG2.reporter
#X <- X[,3:99]
#X [is.na(X)] <- 0
#write.csv(X , file = "raw_data.csv")
#paste("x", i, sep = "_")
#print(paste("x", i, sep = "_"))

n_new <- data.frame(c("Sweden","Switzerland","Thailand","Togo","Trinidad and Tobago","Tunisia","Turkey","Uganda","North Macedonia","Egypt","United Kingdom","USA","Burkina Faso","Uruguay","Venezuela","Zambia","Italy","Côte d'Ivoire","Japan","Kazakhstan","Rep. of Korea","Latvia","Lithuania","China, Macao SAR","Madagascar","Malawi","Malaysia","Maldives","Malta","Mauritius","Mexico","Rep. of Moldova","Morocco","Oman","Netherlands","New Zealand","Nicaragua","Niger","Norway","Panama","Paraguay","Peru","Poland","Portugal","Romania","Saint Lucia","Saint Vincent and the Grenadines","Seychelles","India","Singapore","Slovakia","Slovenia","Spain","Fmr Sudan","Algeria","Andorra","Argentina","Australia","Austria","Bolivia (Plurinational State of)","Brazil","Belize","Burundi","Canada","Central African Rep.","Chile","China","Colombia","Comoros","Costa Rica","Croatia","Cyprus","Estonia","Finland","France","Gambia","Germany","Greece","Greenland","Grenada","Guatemala","Honduras","China, Hong Kong SAR","Hungary","Iceland","Indonesia","Ireland","Israel","Albania","Bulgaria","Faeroe Isds","French Polynesia","Senegal","Azerbaijan","Gabon","Georgia","Jamaica","Mali","Mongolia","Philippines","Russian Federation","Ukraine"))
names(n_new)[1]<-"n"


C_ALL <- c(X_1995$data_AG2.reporter, X_1996$data_AG2.reporter)
print(n <- unique(C_ALL))    
new_X_1996 <- merge(X_1996, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_1997 <- merge(X_1997, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_1998 <- merge(X_1998, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_1999 <- merge(X_1999, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_2000 <- merge(X_2000, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_2001 <- merge(X_2001, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_2002 <- merge(X_2002, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_2003 <- merge(X_2003, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_2004 <- merge(X_2004, n_new, by.x = "data_AG2.reporter", by.y = "n")
new_X_2005 <- merge(X_2005, n_new, by.x = "data_AG2.reporter", by.y = "n")


