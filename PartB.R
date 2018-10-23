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
#to find the cosine similarity matrix we need  the normalised tweet matrix.
norm.tweet.matrix = diag(1/sqrt(rowSums(tweet.matrix^2))) %*% tweet.matrix
#cosine similarity
CS=norm.tweet.matrix%*%t(norm.tweet.matrix)

head(S)[1:20]
head(CS)[1:20]

#8.4.2
retweetCount=tweets$retweet_count
hist(retweetCount)
#As the histogram of retwet count is exponenetial 
#we take logarithm of retweet counts
#we take the log of retweet count+1 
#so as to eliminate log 0 from the data
hist(log(retweetCount+1))

#8.4.3
graph=graph.adjacency(S,weighted = TRUE)
plot(graph,layout = layout.drl,vertex.size=log(retweetCount+1))
#How to remove the tweets from the graph
#to remove self loops decide later
#coords <- layout.auto(graph1)
#plot(simplify(graph1), layout = coords) # remove loops and multiple edges

#8.5.1
#degree centrality
degree(graph)
topTweetsDegree=order(degree(graph),decreasing = TRUE)[1:10]
tweets$text[topTweetsDegree]
#closeness centrality 
closeness(graph)
topTweetsCloseness=order(closeness(graph),decreasing = TRUE)[1:10]
tweets$text[topTweetsCloseness]

#betweenness centrality
betweenness(graph)
topTweetsBetweenness=order(betweenness(graph),decreasing = TRUE)[1:10]
tweets$text[topTweetsBetweenness]

#8.5.2 Page rank to calculate the most influential tweets
adjacency.to.probability = function(A) {
  cols = ncol(A)
  for (a in 1:cols) {
    A[, a] = normalise(A[, a])
  }
  return(A)
}
normalise=function(x){
  if(sum(x)!=0){
    x=x/sum(x)
  }else{
    x=x
  }
  return(x)
}
T = adjacency.to.probability(S)
J = matrix(rep(1/1000, 1000 * 1000), 1000, 1000)
alpha=0.8
M = alpha * T + (1 - alpha) * J
M = adjacency.to.probability(M)
#Power method to find stationary distribution
#function to calculate Euclidean distance between two vectors.
differenceEuc=function(x,y){
  return(sqrt(sum((x-y)^2)))
}
stationary.distribution = function(T) {
  # first create the initial state distribution
  n = ncol(T)
  p = rep(0, n)
  p[1] = 1
  
  # now take a random walk until the state distribution reaches the
  # stationary distribution.
  p.old = rep(0, n)
  while (differenceEuc(p, p.old) > 1e-06) {
    p.old = p
    p = T %*% p.old
  }
  return(p)
}

p = stationary.distribution(M)
influentialTweets=order(p,decreasing = TRUE)[1:11]
#took 11 to get 10 distinct users as one user was repeat in top10.
tweets$text[influentialTweets]
users=tweets$screen_name[influentialTweets]
users

#8.5.4 influence ratio
InfluenceRatio= tweets$followers_count[influentialTweets]/
                tweets$friends_count[influentialTweets]
influence <- data.frame(users,InfluenceRatio)
names(influence) <- c("User Names","Influence Ratio")
influence
#8.5.4 Activity measures
ActivityMeasure=tweets$statuses_count[influentialTweets]
activity <- data.frame(users,ActivityMeasure)
names(influence) <- c("User Names","Activity Measure")
activity
#8.5.5
plot(InfluenceRatio,ActivityMeasure,col="brown",pch=20,
     text(InfluenceRatio,ActivityMeasure,labels = users,cex = 0.5,pos=3))








