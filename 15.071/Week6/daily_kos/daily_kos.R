
dailykos = read.csv("dailykos.csv")

#Calculate distances
distances = dist(dailykos, method = "euclidean")

cluster = hclust(distances, method = "ward.D")
plot(cluster)

clusterGroups = cutree(cluster, k=7)

cluster1 = subset(dailykos, clusterGroups==1)
cluster2 = subset(dailykos, clusterGroups==2)
cluster3 = subset(dailykos, clusterGroups==3)
cluster4 = subset(dailykos, clusterGroups==4)
cluster5 = subset(dailykos, clusterGroups==5)
cluster6 = subset(dailykos, clusterGroups==6)
cluster7 = subset(dailykos, clusterGroups==7)

# number of rows in clusters
nrow(cluster1) # 1266
nrow(cluster2) # 321
nrow(cluster3) # 374
nrow(cluster4) # 139
nrow(cluster5) # 407
nrow(cluster6) # 714
nrow(cluster7) # 209

# top 6 words in each cluster
tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))
