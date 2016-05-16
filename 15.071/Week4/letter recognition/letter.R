setwd("/home/musigma/MOOC/15.071/Week4/letter recognition/")
letters = read.csv("letters_ABPR.csv")
str(letters)
letters$isB = as.factor(letters$letter == "B")
library(caTools)

set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split = TRUE)
test = subset(letters, split = FALSE)
table(train$isB)
2350/(2350+766)
# 0.754172

# Problem 1.2 - Predicting B or not B 
CARTb = rpart(isB ~ . - letter, data=train, method="class")
pred = predict(CARTb, newdata = test, type = "class")
table(train$isB, pred)
#       FALSE TRUE
# FALSE  2280   70
# TRUE    101  665
(2280+665)/(2280+665+70+101)

# Problem 1.3 - Predicting B or Not B using RandomForests
library(randomForest)
set.seed(1000)
isBforest = randomForest(isB ~ . - letter, data=train)
predForest = predict(isBforest, newdata = test)
table(test$isB, predForest)
#       FALSE TRUE
# FALSE  2350    0
# TRUE      0  766

# Problem 2.1 - Predicting the letters A, B, P, R 
letters$letter = as.factor( letters$letter ) 
set.seed(2000)
split2 = sample.split(letters$letter, SplitRatio = 0.5)
trainMulti = subset(letters, split2=TRUE)
testMulti = subset(letters, split2=FALSE)
table(trainMulti$letter)
#   A   B   P   R 
# 789 766 803 758 
803/(nrow(trainMulti))

# Building a multi class classification tree
CARTmulti = rpart(letter~.-isB, data = trainMulti, method = "class")
predCARTmulti = predict(CARTmulti, newdata = testMulti, type = "class")
table(testMulti$letter, predCARTmulti)
#     A   B   P   R
# A 703   8   0  78
# B  19 596  17 134
# P  10  37 732  24
# R  11  38   8 701

(703+596+732+701)/nrow(test)

# Building a multi class random forest
set.seed(1000)
multiForest = randomForest(letter~.-isB, data = trainMulti)
multiForestPred = predict(multiForest, newdata = testMulti)
table(testMulti$letter, multiForestPred)
#     A   B   P   R
# A 789   0   0   0
# B   0 766   0   0
# P   0   0 803   0
# R   0   0   0 758

# You should find this value rather striking, for several reasons. 
# The first is that it is significantly higher than the value for CART, 
# highlighting the gain in accuracy that is possible from using random forest models. 
# The second is that while the accuracy of CART decreased significantly as we 
# transitioned from the problem of predicting B/not B (a relatively simple problem) 
# to the problem of predicting the four letters (certainly a harder problem), 
# the accuracy of the random forest model decreased by a tiny amount.