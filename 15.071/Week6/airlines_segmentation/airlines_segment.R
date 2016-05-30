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

length(subset(cluster, clusterGroups==1))

# compare the average values in each of the variables for the 5 clusters
tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
