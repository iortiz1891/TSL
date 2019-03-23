
year <- list(X_1994, X_1995, X_1996, X_1997, X_1998, X_1999, X_2000)
counter=0
tot_withinss_vec=list()
for (y in year){
counter=counter+1
db <- y
row.names(db)<- db$data_AG2.reporter
db <- db[,2:ncol(db)]
db [is.na(db)] <- 0

log_Scaled=TRUE
if (log_Scaled){
  db<-log(db+1)
  for (i in 1:ncol(db))
    db[,i] <- (db[,i] - mean(db[,i]))/sd(db[,i])
}else{
  for (i in 1:ncol(dbok)){
    db[,i] <- (db[,i] / sum(db[,i]))
  }}

model_km<-kmeans(db, centers=6)
clust_km2<- model_km$cluster
db1_km2<- mutate(db, cluster=clust_km2)
model_km

#total withinss  --  elbow
tot_withinss <-map_dbl(1:10, function(k){
  model<-kmeans(x=db,centers=k)
  model$tot.withinss
})

elbow_df<-data.frame(
  k=1:10,
  tot_withinss=tot_withinss
)

#assign(paste("withinss", y, sep = "_"), tot_withinss)

#rbind(tot_withinss[i], tot_withinss)
#print(tot_withinss)

tot_withinss_vec[[counter]]=tot_withinss

}

plot(c(1:10),tot_withinss_vec[[1]],type="l",col="red")
lines(c(1:10),tot_withinss_vec[[2]],col="green")
lines(c(1:10),tot_withinss_vec[[3]],col="green")
lines(c(1:10),tot_withinss_vec[[4]],col="green")
lines(c(1:10),tot_withinss_vec[[5]],col="green")
lines(c(1:10),tot_withinss_vec[[6]],col="green")
lines(c(1:10),tot_withinss_vec[[7]],col="green")

print(elbow_df)

ggplot(elbow_df, aes(x =k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
