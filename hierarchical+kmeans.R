setwd("~/Dropbox/tsl-project/TSL")
db<-read.table("2004indicadores.csv", header= T, sep=",", dec=".")
names(db)
dbok<- na.omit(db)
db1scaled<-as.data.frame(scale(dbok[ ,2:8]))

distances<-dist(db1scaled)
hc_complete<-hclust(distances, method="complete")
hc_single<-hclust(distances, method="single")
hc_average<-hclust(distances, method="average")
par(mfrow = c(1,3))
plot(hc_complete,labels=dbok$country, main = 'Complete Linkage', cex=0.5)
plot(hc_single, labels=dbok$country, main = 'Single Linkage', cex=0.5)
plot(hc_average, labels=dbok$country, main = 'Average Linkage', cex=0.5) 
library(dendextend)
dend<- as.dendrogram(hc_complete)
dend_coloredz<- color_branches (dend, h=2)
par(mfrow = c(1,1))
plot(dend_coloredz)



library(dplyr)
library(ggplot2)

Kmeans
model_km<-kmeans(db1scaled, centers=5)
clust_km2<- model_km$cluster
db1_km2<- mutate(db1scaled, cluster=clust_km2)


ggplot(db1_km2, aes(x=Country.Code, y= X1990..YR1990., color= factor(cluster))) + geom_point()


#total withinss
library(purrr)
tot_withinss <-map_dbl(1:10, function(k){
  model<-kmeans(x=db1scaled,centers=k)
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

library(cluster)
pam_db<- pam(db1scaled, k=5)
pam_db$silinfo$widths

silplot<-silhouette(pam_db)
plot(silplot)


model<-kmeans(x=db1scaled,centers=2)



