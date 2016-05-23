rm(list=ls())
setwd("/home/musigma/MOOC/15.071/Week5/emails/")
library(tm)
library(SnowballC)
emails = read.csv('emails.csv', stringsAsFactors = FALSE)

table(emails$spam)

max(nchar(emails$text))
which(nchar(emails$text) == min(nchar(emails$text)))

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, sparse = 0.95)
spdtm

emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam
sum(colSums(emailsSparse[which(emailsSparse$spam == 0),]) > 5000)
# or #
sort(colSums(subset(emailsSparse, spam == 0)))

sort(colSums(subset(emailsSparse, spam == 1)))

# Problem 3.1 - Building machine learning models
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

# Building logistic, CART, random forest

# Logistic
spamLog = glm(spam~., data = train, family = "binomial")
predictLog = predict(spamLog, type = "response")
# How many of the training set predicted probabilities from spamLog are less than 0.00001?
sum(predictLog<=0.99999 & predictLog >= 0.00001)
# significant variables in predictLog
summary(spamLog)
# the training set accuracy
table(train$spam, predictLog>0.5)
#   FALSE TRUE
# 0  3052    0
# 1     4  954
(nrow(train)-4)/(nrow(train))
# 0.9990025
# AUC
rocrTrainLog = as.numeric(performance(prediction(predictLog, train$spam), "auc")@y.values)
# 0.9999959

# CART
spamCART = rpart(spam~., data = train, method = "class")
predictCART = predict(spamCART)
prp(spamCART)
# the training set accuracy
table(train$spam, predictCART[,2]>0.5)
#   FALSE TRUE
# 0  2885  167
# 1    64  894
(2885+894)/(2885+894+64+167)
# 0.942394
# AUC
rocrTrainCART = as.numeric(performance(prediction(predictCART[,2], train$spam), "auc")@y.values)
# 0.9696044


# RF
set.seed(123)
spamRF = randomForest(spam~., data = train)
predictRF = predict(spamRF, type = "prob")
# the training set accuracy
table(train$spam, predictRF[,2]>0.5)
#   FALSE TRUE
# 0  3013   39
# 1    44  914
(3013+914)/(3013+914+44+39)
#0.9793017
# AUC
rocrTrainRF = as.numeric(performance(prediction(predictRF[,2], train$spam), "auc")@y.values)
# 0.9979116


# Test set accuracy

# Log
predictLogTest = predict(spamLog, newdata = test)
table(test$spam, predictLogTest > 0.5)
#   FALSE TRUE
# 0  1258   50
# 1    34  376
(1258+376)/nrow(test)
# 0.9511059
# AUC
as.numeric(performance(prediction(predictLogTest, test$spam), "auc")@y.values)
# 0.9767994

# CART
predictCARTTest = predict(spamCART, newdata = test)
table(test$spam, predictCARTTest[,2]>0.5)
#   FALSE TRUE
# 0  1228   80
# 1    24  386
(1228+386)/nrow(test)
# 0.9394645
as.numeric(performance(prediction(predictCARTTest[,2], test$spam), "auc")@y.values)
# 0.963176

# RF
predictRFTest = predict(spamRF, newdata = test, type = "prob")
table(test$spam, predictRFTest[,2]>0.5)
#   FALSE TRUE
# 0  1290   18
# 1    25  385
(1290+385)/nrow(test)
# 0.9749709
as.numeric(performance(prediction(predictRFTest[,2], test$spam), "auc")@y.values)
# 0.9975656