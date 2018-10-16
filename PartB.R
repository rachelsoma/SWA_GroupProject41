library("tm")
library("rtweet")
library("twitteR")
library("igraph")
tweets=read.csv("SWAProject.csv")
corpus=Corpus(VectorSource(tweets$text))
corpus = tm_map(corpus, function(x) iconv(x, to = 'UTF8', sub = 'byte'))
corpus = tm_map(corpus, function(x) iconv(x, to = 'ASCII', sub = ' '))
corpus = tm_map(corpus,removeNumbers)
corpus = tm_map(corpus, removeWords,c(stopwords(),"J.K. Rowling","https", "t.co"))
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords,c(stopwords(),"jk","https", "t.co","rowl","jkrowl",
                                      "will","like","just","doe","far","quot","look",
                                      "take","make","jaqkcpmtg","ygorfremo","ask","peopl",
                                      "next","twitter","peopl","new","write","charact","got",
                                      "ohhgmljqrm","say","defend","respond", "vrmgsnrsa",
                                      "bbepgeik","nkuwxgsn","novrijqr","nlrbgguhxt"))
dtm = DocumentTermMatrix(corpus)
tweet.wdtm = weightTfIdf(dtm)
tweet.matrix = as.matrix(tweet.wdtm)
#Find similarity matrix
S=tweet.matrix%*%t(tweet.matrix)
norm.tweet.matrix = diag(1/sqrt(rowSums(tweet.matrix^2))) %*% tweet.matrix
CS=norm.tweet.matrix%*%t(norm.tweet.matrix)
plot(S)
plot(CS)
compare.matrix(S,CS,nbins=30)
#8.4.2
retweetCount=tweets$retweet_count
retweetedIds=which(tweets$retweet_count !=0)
retweetedTweets=tweets[retweetedIds,]
hist(retweetedTweets$retweet_count)
hist(log(retweetedTweets$retweet_count))
