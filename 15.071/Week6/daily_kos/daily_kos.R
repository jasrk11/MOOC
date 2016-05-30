
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
#     state republican       poll   democrat      kerry       bush 
# 0.7575039  0.7590837  0.9036335  0.9194313  1.0624013  1.7053712 
tail(sort(colMeans(cluster2)))
#     bush  democrat challenge      vote      poll  november 
# 2.847352  2.850467  4.096573  4.398754  4.847352 10.339564 
tail(sort(colMeans(cluster3)))
#    elect    parties      state republican   democrat       bush 
# 1.647059   1.665775   2.320856   2.524064   3.823529   4.406417 
tail(sort(colMeans(cluster4)))
# campaign    voter presided     poll     bush    kerry 
# 1.431655 1.539568 1.625899 3.589928 7.834532 8.438849 
tail(sort(colMeans(cluster5)))
# american       presided administration            war           iraq           bush 
# 1.090909       1.120393       1.230958       1.776413       2.427518       3.941032 
tail(sort(colMeans(cluster6)))
#      race      bush     kerry     elect  democrat      poll 
# 0.4579832 0.4887955 0.5168067 0.5350140 0.5644258 0.5812325 
tail(sort(colMeans(cluster7)))
# democrat    clark   edward     poll    kerry     dean 
# 2.148325 2.497608 2.607656 2.765550 3.952153 5.803828 

# K-Means Clustering
set.seed(1000)
kCluster = kmeans(dailykos, centers = 7)
# Subset data into clusters
Kcluster1 = subset(dailykos, kCluster$cluster==1)
Kcluster2 = subset(dailykos, kCluster$cluster==2)
Kcluster3 = subset(dailykos, kCluster$cluster==3)
Kcluster4 = subset(dailykos, kCluster$cluster==4)
Kcluster5 = subset(dailykos, kCluster$cluster==5)
Kcluster6 = subset(dailykos, kCluster$cluster==6)
Kcluster7 = subset(dailykos, kCluster$cluster==7)

nrow(Kcluster1)  # 146
nrow(Kcluster2)  # 144
nrow(Kcluster3)  # 277
nrow(Kcluster4)  # 2063
nrow(Kcluster5)  # 163
nrow(Kcluster6)  # 329
nrow(Kcluster7)  # 308

# top 6 words in each cluster
tail(sort(colMeans(Kcluster1)))
#    state           iraq          kerry administration       presided           bush 
# 1.609589       1.616438       1.636986       2.664384       2.767123      11.431507 
tail(sort(colMeans(Kcluster2)))
# primaries  democrat    edward     clark     kerry      dean 
#  2.319444  2.694444  2.798611  3.090278  4.979167  8.277778 
tail(sort(colMeans(Kcluster3)))
# administration          iraqi       american           bush            war           iraq 
#       1.389892       1.610108       1.685921       2.610108       3.025271       4.093863 
tail(sort(colMeans(Kcluster4)))
#     elect republican      kerry       poll   democrat       bush 
# 0.6010664  0.6175473  0.6495395  0.7474552  0.7891420  1.1473582 
tail(sort(colMeans(Kcluster5)))
#     race     senate      state    parties republican   democrat 
# 2.484663   2.650307   3.521472   3.619632   4.638037   6.993865 
tail(sort(colMeans(Kcluster6)))
# democrat      bush challenge      vote      poll  november 
# 2.899696  2.960486  4.121581  4.446809  4.872340 10.370821 
tail(sort(colMeans(Kcluster7)))
# presided    voter campaign     poll     bush    kerry 
# 1.324675 1.334416 1.383117 2.788961 5.970779 6.480519 