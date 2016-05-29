rm(list=ls())

census = read.csv("census.csv")
library(caTools)
set.seed(2000)

split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split == TRUE)
test = subset(census, split == FALSE)

LogModel = glm(over50k ~., data = train, family = binomial)
summary(LogModel)

predictLog = predict(LogModel, newdata=test, type="response")
table(test$over50k, predictLog>=0.5)
#       FALSE TRUE
# <=50K  9051  662
# >50K   1190 1888
(9051+1888)/(nrow(test))
# 0.8552107
# Baseline
(9051+662)/nrow(test)
# 0.7593621

library(ROCR)
# AUC
plot(performance(prediction(predictLog, test$over50k), "tpr", "fpr"))
as.numeric(performance(prediction(predictLog, test$over50k), "auc")@y.values)
# 0.9061597

####################################################################################
# Problem 2 - A CART Model 
library(rpart)
library(rpart.plot)
CARTmodel = rpart(over50k ~., data = train, method = "class")
prp(CARTmodel)

# What is the accuracy of the model on the testing set? Use a threshold of 0.5
CARTpred <- predict(CARTmodel, newdata = test, type = "class")
table(test$over50k, CARTpred)
#         <=50K  >50K
# <=50K   9243   470
# >50K    1482  1596
(9243+1596)/nrow(test)

# Calculating the AUC values
CARTpred_probaility = predict(CARTmodel, newdata = test)
ROCpred = prediction(CARTpred_probaility[,2], test$over50k)
plot(performance(ROCpred, "tpr", "fpr"))
performance(ROCpred, "auc")@y.values
# 0.8470256

########################################################################
# PROBLEM 3 - A RANDOM FOREST MODEL
# Building random forest
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

library(randomForest)
set.seed(1)
forest <- randomForest(over50k ~., data = trainSmall)
forestPred = predict(forest, newdata = test)
table(test$over50k, forestPred)
#         <=50K  >50K
# <=50K   9586   127
# >50K    1985  1093
(9586+1093)/nrow(test)

# number of times, aggregated over all of the trees in the random forest model, 
# that a certain variable is selected for a split
vu = varUsed(forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))

varImpPlot(forest)

############################################################################
# Problem 4 - Selecting cp by Cross-Validation
library(caret)
library(e1071)
set.seed(2)
numFolds = trainControl(method= 'cv', number = 10)
cpGrid = expand.grid(.cp = seq(0.002,0.1,0.002))
train(over50k ~., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
# cp = 0.002

# fitting CART using this cp value
CARTmodel2 <- rpart(over50k ~., data = train, cp = 0.002, method = "class")
CARTmodel2Predict <- predict(CARTmodel2, newdata=test, type="class")
table(test$over50k, CARTmodel2Predict)
#         <=50K  >50K
# <=50K   9178   535
# >50K    1240  1838
(9178+1838)/nrow(test)
# 0.8612306

prp(CARTmodel2)
