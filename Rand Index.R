
memory.size(max = FALSE)
memory.limit(size = FALSE)

###Data loading
db <- new_X_1996
row.names(db)<- db$q1_AG2.reporter
db <- db[,2:ncol(db)]
db [is.na(db)] <- 0

#DATA Normalization
log_Scaled=FALSE
if (log_Scaled){
  db<-log(db+1)
  for (i in 1:ncol(db))
    db[,i] <- (db[,i] - mean(db[,i]))/sd(db[,i])
}else{
  for (i in 1:ncol(dbok)){
    db[,i] <- (db[,i] / sum(db[,i]))
  }}

##Kmeans for 1st
model_km_1<-kmeans(db, centers=5)
clust_km_1<- model_km_1$cluster
db_km_1<- mutate(db, cluster=clust_km_1)
M_1 <- c(db_km_1[1:102,"cluster"])
#identity_matrix_km_1 <- diag(M_1)

#n_new[,"n"]
  
  ###Data loading
  db <- new_X_2005
  row.names(db)<- db$q1_AG2.reporter
  db <- db[,2:ncol(db)]
  db [is.na(db)] <- 0
  
  #DATA Normalization
  log_Scaled=TRUE
  if (log_Scaled){
    db<-log(db+1)
    for (i in 1:ncol(db))
      db[,i] <- (db[,i] - mean(db[,i]))/sd(db[,i])
  }else{
    for (i in 1:ncol(dbok)){
      db[,i] <- (db[,i] / sum(db[,i]))
    }}
  
##Kmeans for 2st
model_km_2<-kmeans(db, centers=5)
clust_km_2<- model_km_2$cluster
db_km_2<- mutate(db, cluster=clust_km_2)
M_2 <- c(db_km_2[1:102,"cluster"])
#M_2 <- c(db_km_2["cluster"])
#identity_matrix_km_2 <- diag(M_2)
print(db_km_2)

###Rand Index calculation
#RI <- rand.index(identity_matrix_km_1, identity_matrix_km_2)
#ARI <- adj.rand.index(identity_matrix_km_1, identity_matrix_km_2)
RI <- rand.index(M_1, M_2)
ARI <- adj.rand.index(M_1, M_2)
print(RI)
print(ARI)



g1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
g2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2)
rand.index(g1, g2)
