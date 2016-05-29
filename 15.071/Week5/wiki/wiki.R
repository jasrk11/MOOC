rm(list=ls())
library(tm)
library(SnowballC)


wiki <- read.csv('wiki.csv', stringsAsFactors = F)
wiki$Vandal <- as.factor(wiki$Vandal)

# cases of vandalism
table(wiki$Vandal)

# corpus for added words
corpusAdded = Corpus(VectorSource(wiki$Added))
# Stopwords
corpusAdded = tm_map(corpusAdded, removeWords, c(stopwords("english")))
# stemming the words
corpusAdded = tm_map(corpusAdded, stemDocument)

# DocumentTermMatrix
dtmAdded = DocumentTermMatrix(corpusAdded)
# Terms-6675
findFreqTerms(dtmAdded, lowfreq = 20)
inspect(dtmAdded[2000:2010, 4000:4010])

# Filter out sparse terms by keeping only terms that appear 
# in 0.3% or more of the revision
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# Repeat all the steps for removed words
corpusRemoved = Corpus(VectorSource(wiki$Removed))
# Stopwords
corpusRemoved = tm_map(corpusRemoved, removeWords, c(stopwords("english")))
# Stem words
corpusRemoved = tm_map(corpusRemoved, stemDocument)

# DocumenttemrMatrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)

# Filter out sparse terms by keeping only terms that appear 
# in 0.3% or more of the revision
sparseRemoved = removeSparseTerms(dtmRemoved, sparse = 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

# Combine the two data frames into a data frame 
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
trainWiki = subset(wikiWords, split = TRUE)
testWiki = subset(wikiWords, split = FALSE)
table(testWiki$Vandal)
2061/(1815+2061)
# 0.5317337

# Bulid a CART model
library(rpart)
library(rpart.plot)
wikiCART <- rpart(Vandal~., data = trainWiki, method = "class")
prp(wikiCART)

#Accuracy
predictCART = predict(wikiCART, newdata = testWiki, type = "class")
table(testWiki$Vandal, predictCART)
#      0    1
# 0 2061    0
# 1 1770   45
(2061+45)/(2061+45+1+1770)
0.5432035

# baseline
table(testWiki$Vandal)
2061/(2061+1815)
0.5317337

# Problem-specific Knowledge
# two techniques - identifying a key class of words, and counting words
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
# new CART
wiki2CART <- rpart(Vandal~., data = wikiTrain2, method = "class")
prp(wiki2CART)

#Accuracy
predict2CART = predict(wiki2CART, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, predict2CART)
#     0   1
# 0 609   9
# 1 488  57
#Accuracy
(609+57)/(609+57+9+488)

# Word count
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)
# Recreate CART
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
# new CART
wiki3CART <- rpart(Vandal~., data = wikiTrain2, method = "class")
prp(wiki3CART)

#Accuracy
predict3CART = predict(wiki3CART, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, predict3CART)
#     0   1
# 0 514 104
# 1 297 248
#Accuracy
(514+248)/(514+248+104+297)


# Problem 3.1 - Using Non-Textual Data
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
# Recreate CART model
wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)
# new CART
wiki3CART <- rpart(Vandal~., data = wikiTrain3, method = "class")
prp(wiki3CART)

#Accuracy
predict3CART = predict(wiki3CART, newdata = wikiTest3, type = "class")
table(wikiTest3$Vandal, predict3CART)
#     0   1
# 0 595  23
# 1 304 241
#Accuracy
(595+241)/(595+241+23+304)
