rm(list=ls())
setwd("/home/musigma/MOOC/15.071/Week5/clinical trial/")
library(tm)
trials <- read.csv('clinical_trial.csv', stringsAsFactors = FALSE)

summary(trials)

# #characters are there in the longest abstract
max(nchar(trials$abstract))

table(nchar(trials$abstract) == 0)

trials$title[nchar(trials$title) == min(nchar(trials$title))]
# A decade of letrozole: FACE.
# to corpus
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
# to lower case
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
# Punctuation
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
# remove stop words
corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english")))
corpusAbstract = tm_map(corpusAbstract, removeWords, c(stopwords("english")))
# stem words
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
#Document term matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
# limit to terms with sparseness of at most 95%
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# convert to dataframe
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# most frequent word
which(colSums(dtmAbstract) == max(colSums(dtmAbstract)))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio = 0.7)
trialTrain = subset(dtm, spl == TRUE)
trialTest = subset(dtm, spl == FALSE)
table(trialTrain$trial)
1043/nrow(trialTrain)

trialCART = rpart(trial~., data = trialTrain, method = 'class')
prp(trialCART)
predicTrain = predict(trialCART, type = 'class')
table(trialTrain$trial, predicTrain)
#     0   1
# 0 631  99
# 1 131 441
(631+441)/(nrow(trialTrain))
# sensitivity
441/(441+131)
# specificity
631/(631+99)
# predict on testing set
predTest = predict(trialCART, newdata = trialTest, type = 'class')
table(trialTest$trial, predTest)
#     0   1
# 0 261  52
# 1  83 162
(261+162)/(261+162+52+83)

# AUC
library(ROCR)
predTest = predict(trialCART, newdata = trialTest)
predROCR = prediction(predTest[,2], trialTest$trial)
auc = as.numeric(performance(predROCR, "auc")@y.values)
