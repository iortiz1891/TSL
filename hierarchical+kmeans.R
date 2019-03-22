
#-------------------
#Libraries
library(dplyr)
library(ggplot2)
library(dendextend)
library(purrr)
library(cluster)
#-------------------

#DATA Normalization
db <- read.csv("raw_data.csv")
row.names(db)<- db$q1_AG2.reporter
db <- db[,3:99]
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

 

##Hierarchical clustering
distances<-dist(db)
hc_complete<-hclust(distances, method="complete")
hc_single<-hclust(distances, method="single")
hc_average<-hclust(distances, method="average")

plot(hc_complete,labels=row.names(db), main = 'Complete Linkage', cex=0.5)
plot(hc_single, labels=row.names(db), main = 'Single Linkage', cex=0.5)
plot(hc_average, labels=row.names(db), main = 'Average Linkage', cex=0.5) 
dend<- as.dendrogram(hc_complete)
dend_coloredz<- color_branches (dend, h=2)
par(mfrow = c(1,1))
plot(dend_coloredz)

hist(db[1:100,3])

##Kmeans
model_km<-kmeans(db, centers=5)
clust_km2<- model_km$cluster
db1_km2<- mutate(db, cluster=clust_km2)

#total withinss  --  elbow
tot_withinss <-map_dbl(1:10, function(k){
  model<-kmeans(x=db,centers=k)
  model$tot.withinss
})

elbow_df<-data.frame(
  k=1:10,
  tot_withinss=tot_withinss
)
print(elbow_df)

ggplot(elbow_df, aes(x =k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

#silhouette
pam_db<- pam(db, k=5)
pam_db$silinfo$widths
silplot<-silhouette(pam_db)
plot(silplot)






