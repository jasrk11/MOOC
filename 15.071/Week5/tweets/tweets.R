tweets = read.csv("tweets.csv", stringsAsFactors = F)
str(tweets)

# For detecting only strongly negative tweets
tweets$Negative <- as.factor(tweets$Avg <= -1) 

library(tm)
library(SnowballC)

# Create corpus
corpus = Corpus(VectorSource(tweets$Tweet))
corpus


corpus = tm_map(corpus, tolower)      # All chars to lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)      # remove punctuation

stopwords("english")[1:10]      # first 10 stopwords

# Removing all stopwords & the word "apple" since all tweets have the word apple
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

# Stemming
corpus = tm_map(corpus, stemDocument)

# document term matrix where each row is a tweet and each column is frequency of the word
frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])

# this is sparse data, i.e., we have lot of zeroes in our data

findFreqTerms(frequencies, lowfreq = 20)

# remove less frequent terms
# Reason1: Computationally expensive
# Reason2: More independent variables = Less generalized model

sparse = removeSparseTerms(frequencies, 0.995)
sparse

tweetsSparse = as.data.frame(as.matrix(sparse))

# R struggles with variables that starts with number
# Change word names to appropriate names
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable
tweetsSparse$Negative <- tweets$Negative

# Split data into train & test
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)


# Build model using CART & Logisitic Regression
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative~., data = trainSparse, method = "class")
prp(tweetCART)

# predict on test data
predictCART = predict(tweetCART, newdata = testSparse, type = "class")
table(testSparse$Negative, predictCART)
#       predictCART
#       FALSE TRUE
# FALSE   294    6
# TRUE     37   18

# Accuracy
(294+18)/(294+6+37+18)
0.8788732

# Baseline model
table(testSparse$Negative)
# FALSE  TRUE 
# 300    55 
(300)/(300+55)
# 0.8450704

# Random forest
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative~., data = trainSparse)
predictRF = predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predictRF)
#         FALSE TRUE
# FALSE   293    7
# TRUE     34   21
(293+21)/(293+21+7+34)
# 0.884507


# Building Logisitic regression model
library(glm)
tweetLog = glm(Negative~., data = trainSparse, family = binomial)
predictions = predict(tweetLog, newdata = testSparse, type = "response")
table(testSparse$Negative, predictions>0.5)
#         FALSE TRUE
# FALSE   253   47
# TRUE     27   28
(253+28)/(253+28+47+27)
