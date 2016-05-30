rm(list=ls())

stocks = read.csv("StocksCluster.csv")

# proportion of the observations that have positive returns in December
length(which(stocks$PositiveDec==1))/nrow(stocks)
# 0.546114

# correlation between variables
tail(sort(cor(stocks[1:11])), 15)
# [1] 0.1699945 0.1699945 0.1916728 0.1916728 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
# [10] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000

# largest & smallest mean return across all observations in the dataset
colMeans(stocks[1:11])
min(colMeans(stocks[1:11]))
max(colMeans(stocks[1:11]))
# ReturnJan    ReturnFeb    ReturnMar    ReturnApr    ReturnMay   ReturnJune   ReturnJuly 
# 0.012631602 -0.007604784  0.019402336  0.026308147  0.024736591  0.005937902  0.003050863 
# ReturnAug    ReturnSep    ReturnOct    ReturnNov 
# 0.016198265 -0.014720768  0.005650844  0.011387440 

# Initial Logistic Regression Model 
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec~., data = stocksTrain, family = binomial)
# Accuracy on training set
table(stocksTrain$PositiveDec, predict(StocksModel, type="response") > 0.5)
#   FALSE TRUE
# 0   990 2689
# 1   787 3640
(990+3640)/nrow(stocksTrain)
# 0.5711818

# Accuracy on test set
table(stocksTest$PositiveDec, predict(StocksModel, newdata = stocksTest, type="response") > 0.5)
#   FALSE TRUE
# 0   417 1160
# 1   344 1553
(417+1553)/nrow(stocksTest)
# 0.5670697

# baseline model accuracy
table(stocksTest$PositiveDec)
#    0    1 
# 1577 1897 
(1897)/nrow(stocksTest)
# 0.5460564


# Clustering Stocks 
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# normalizing data
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)
