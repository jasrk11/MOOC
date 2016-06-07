tweets = read.csv('tweets.csv', stringsAsFactors = F)

library(tm)
library(SnowballC)

# Create corpus
corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)      # All chars to lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)      # remove punctuation


# Removing all stopwords & the word "apple" since all tweets have the word apple
corpus = tm_map(corpus, removeWords, c(stopwords("english")))

# document term matrix where each row is a tweet and each column is frequency of the word
frequencies <- DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

library(wordcloud)

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, .1))

# this time removing the most frequent word in addition to all elements of stopwords("english")
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)      # All chars to lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)      # remove punctuation


# Removing all stopwords & the word "apple" since all tweets have the word apple
corpus = tm_map(corpus, removeWords, c('apple', stopwords("english")))

# document term matrix where each row is a tweet and each column is frequency of the word
frequencies <- DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, .1))

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, .1))

library('RColorBrewer')
brewer.pal()
display.brewer.all()

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, .1), colors=brewer.pal(9, "Blues"))
