#https://dataaspirant.com/2018/03/22/twitter-sentiment-analysis-using-r/ <-- głównie z tego korzystam

#https://medium.com/swlh/analyzing-trumps-tweets-5368528d2c90
#https://www.r-bloggers.com/twitter-sentiment-analysis-with-r/

#https://towardsdatascience.com/elon-musk-twitter-adf324120b3f
#jest pakiet twitter do analizy twittera

library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")

data <- read.csv('data_elonmusk.csv', fill=TRUE, header=FALSE, quote="", sep=",", encoding="UTF-8")
View(data)
#wyswietlam sobie przykladowy tweet, zeby zobaczyc w jakiej jest formie
data[2,]
data[3,]
data[50,]
#widac, ze w tresci tweetow pojawiają się linki, małpy, emotki itp. Trzeba to wyczyscic.

#usuwam z ramki danych kolumny inne niż treść tweetów
data2 <- data[-1,-1]
data2 <- data2[,-2:-5]

library(stringr)

##Cleaning & normalizing text data
usableText=str_replace_all(data2,"[^[:graph:]]", " ") 

removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x) 
removeUser <- function(x) gsub("@[[:alnum:][:punct:]]*", "", x) 

usableText <- removeURL(usableText)
usableText <- removeUser(usableText)
usableText <- tolower(usableText)
usableText <- tm::removePunctuation(usableText)
usableText <- tm::removeNumbers(usableText)
usableText <- tm::stripWhitespace(usableText)

############### SENTIMENT ANALYSIS
#‘Syuzhet’ breaks the emotion into 10 different emotions – anger, 
#anticipation, disgust, fear, joy, sadness, surprise, trust, negative and positive.

word.df <- as.vector(usableText)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(usableText, emotion.df)

View(emotion.df2)
sentiment.value <- get_sentiment(word.df)

most.positive <- word.df[sentiment.value == max(sentiment.value)]
most.positive

most.negative <- word.df[sentiment.value <= min(sentiment.value)] 
most.negative

sentiment.value

positive.tweets <- word.df[sentiment.value > 0]
head(positive.tweets)
negative.tweets <- word.df[sentiment.value < 0]
head(negative.tweets)
neutral.tweets <- word.df[sentiment.value == 0]
head(neutral.tweets)

anticipation <- emotion.df2[,1:3]
anticipation <- anticipation[,-2]
anticipation<- anticipation[order(-anticipation$anticipation),]


trust <- emotion.df2[,1:9]
trust <- trust[,-2:-8]
trust<- trust[order(-trust$trust),]


category_senti <- ifelse(sentiment.value < 0, "Negative", ifelse(sentiment.value > 0, "Positive", "Neutral"))
head(category_senti)

table(category_senti)

sentiment_sum<- data.frame(colSums(emotion.df))
sentiment <- rownames(sentiment_sum)

library(ggplot2)

ggplot(sentiment_sum) +
  geom_col(aes(x=sentiment,y = sentiment_sum$colSums.emotion.df., fill = sentiment), position = "stack")

barplot(table(category_senti))

###Word clouds
library(wordcloud)
set.seed(123)

corpus <- Corpus(VectorSource(usableText))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
tdm<-TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
head(v,30)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word, d$freq,min.freq=70)

wordcloud(names(v), v, min.freq=50,scale=c(3, .5), colors=brewer.pal(6, "Dark2"))

barplot(head(v,30))
