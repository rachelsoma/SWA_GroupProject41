library("ROAuth")
library("tm")
library("wordcloud")
library("rtweet")
library("twitter")
tweets=read.csv("SWAProject.csv")
tweets$text[1]
corpus=Corpus(VectorSource(tweets$text))
corpus = tm_map(corpus, function(x) iconv(x, to = 'UTF8', sub = 'byte'))
corpus = tm_map(corpus, function(x) iconv(x, to = 'ASCII', sub = ' '))
corpus = tm_map(corpus,removeNumbers)
corpus = tm_map(corpus, removeWords,c(stopwords(),"J.K. Rowling","https", "t.co"))
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, stemDocument)
corpus$content[1]
dtm = DocumentTermMatrix(corpus)
tweet.wdtm = weightTfIdf(dtm)
tweet.matrix = as.matrix(tweet.wdtm)

## remove empty tweets
#empties = which(rowSums(abs(tweet.matrix)) == 0)
#tweet.matrix = tweet.matrix[-empties, ]

freqs=colSums(tweet.matrix)


wordcloud(names(freqs),freqs,scale=c(2,0.5),random.order = FALSE,use.r.layout=FALSE,
                  colors=brewer.pal(8, "Dark2"),min.freq = 3)


id=order(freqs,decreasing = TRUE)[1:20]
topWords=colnames(tweet.matrix)[id]









