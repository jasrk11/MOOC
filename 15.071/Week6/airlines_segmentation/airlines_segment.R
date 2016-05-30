airlines = read.csv("AirlinesCluster.csv")

summary(airlines)

# Normalizing the data
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)

# Build a hclust

distances = dist(airlinesNorm, method = "euclidean")
cluster = hclust(distances, method = "ward.D")

plot(cluster)

# Number of clusters = 5
clusterGroups = cutree(cluster, k = 5)

length(subset(cluster, clusterGroups==1))  # 776

# compare the average values in each of the variables for the 5 clusters
tapply(airlines$Balance, clusterGroups, mean)
#        1         2         3         4         5 
# 57866.90 110669.27 198191.57  52335.91  36255.91 
tapply(airlines$QualMiles, clusterGroups, mean)
#         1            2            3            4            5 
# 0.6443299 1065.9826590   30.3461538    4.8479263    2.5111773 
tapply(airlines$BonusMiles, clusterGroups, mean)
#         1         2         3         4         5 
# 10360.124 22881.763 55795.860 20788.766  2264.788 
tapply(airlines$BonusTrans, clusterGroups, mean)
#         1         2         3         4         5 
# 10.823454 18.229287 19.663968 17.087558  2.973174 
tapply(airlines$FlightMiles, clusterGroups, mean)
#        1          2          3          4          5 
# 83.18428 2613.41811  327.67611  111.57373  119.32191 
tapply(airlines$FlightTrans, clusterGroups, mean)
#         1         2         3         4         5 
# 0.3028351 7.4026975 1.0688259 0.3444700 0.4388972 
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
#        1        2        3        4        5 
# 6235.365 4402.414 5615.709 2840.823 3060.081 

# K-Means clustering
set.seed(88)
Kcluster = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
Kclust1 = (subset(airlines, Kcluster$cluster==1)) #408
Kclust2 = (subset(airlines, Kcluster$cluster==2)) #141
Kclust3 = (subset(airlines, Kcluster$cluster==3)) #993
Kclust4 = (subset(airlines, Kcluster$cluster==4)) #1182
Kclust5 = (subset(airlines, Kcluster$cluster==5)) #1275

tapply(airlines$Balance, Kcluster$cluster, mean)
#         1         2         3         4         5 
# 219161.40 174431.51  67977.44  60166.18  32706.67 
tapply(airlines$QualMiles, Kcluster$cluster, mean)
#         1         2         3         4         5 
# 539.57843 673.16312  34.99396  55.20812 126.46667 
tapply(airlines$BonusMiles, Kcluster$cluster, mean)
#         1         2         3         4         5 
# 62474.483 31985.085 24490.019  8709.712  3097.478 
tapply(airlines$BonusTrans, Kcluster$cluster, mean)
#         1         2         3         4         5 
# 21.524510 28.134752 18.429003  8.362098  4.284706 
tapply(airlines$FlightMiles, Kcluster$cluster, mean)
#        1         2         3         4         5 
# 623.8725 5859.2340  289.4713  203.2589  181.4698 
tapply(airlines$FlightTrans, Kcluster$cluster, mean)
#         1          2          3          4          5 
# 1.9215686 17.0000000  0.8851964  0.6294416  0.5403922 
tapply(airlines$DaysSinceEnroll, Kcluster$cluster, mean)
#        1        2        3        4        5 
# 5605.051 4684.901 3416.783 6109.540 2281.055 
