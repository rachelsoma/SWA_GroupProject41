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

head(S)[1:20]
head(CS)[1:20]

#8.4.2
retweetCount=tweets$retweet_count
hist(retweetCount)
hist(log(retweetCount+1))

# sort(retweetCount)
# sort(retweetedTweets$retweet_count,decreasing = TRUE)
# retweetedIds=which(tweets$retweet_count !=0)
# retweetedTweets=tweets[retweetedIds,]
# hist(retweetedTweets$retweet_count)
# hist(log(retweetedTweets$retweet_count))

#8.4.3
graph=graph.adjacency(S)
plot(graph,layout = layout.drl,vertex.size=log(retweetCount+1))
#How to remove the tweets from the graph
#to remove self loops decide later
#coords <- layout.auto(graph1)
#plot(simplify(graph1), layout = coords) # remove loops and multiple edges

#8.5
#degree centrality
degree(graph)
which(degree(graph)==max(degree(graph)))
#closeness centrality is not working.. needs to be checked
closeness(graph)
which(closeness(graph)==max(closeness(graph)))
degree(graph)

#betweenness centrality
betweenness(graph)
order(betweenness(graph),decreasing = TRUE)# gives the locations.
sort(betweenness(graph),decreasing = TRUE) # gives the values

#8.5.2
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
#function to calculate Euclidean distance between two vectors.
differenceEuc=function(x,y){
  return(sqrt(sum((x-y)^2)))
}
p = stationary.distribution(M)
influentialTweets=order(p,decreasing = TRUE)[1:10]
tweets$text[influentialTweets]
users=tweets$screen_name[influentialTweets]
users
InfluenceRatio=
  tweets$followers_count[influentialTweets]/tweets$friends_count[influentialTweets]
ActivityMeasure=tweets$statuses_count[influentialTweets]
ActivityMeasure
plot(InfluenceRatio,ActivityMeasure,col="brown",pch=20,
     text(InfluenceRatio,ActivityMeasure,labels = users,cex = 0.5,pos=1))



# #8.4.3 new dont use
# #retweetedIds1=which(tweets$retweet_count >200)
# stest=S[retweetedIds1,retweetedIds1]
# graph1=graph.adjacency(stest, mode = "undirected")
# ?graph.adjacency
# plot(graph1,vertex.size=retweetCount)
# coords <- layout.auto(graph1)
# plot(simplify(graph1), layout = coords) # remove loops and multiple edges
# 
# 
# 
# 
# 
# 
# #old trials dont use
# Graph=graph_from_adjacency_matrix(S,"undirected",weighted = TRUE )
# V(Graph)
# E(Graph)
# ?graph_from_adjacency_matrix
# tkplot(Graph,canvas.width = 1000,canvas.height = 1000)
# plot(Graph,vertex.color = "steelblue4", edge.width = 1, 
#      vertex.label = NA, edge.color = "darkgrey", layout =layout_with_fr(Graph))
# ??DiagrammeR
# DiagrammeR(Graph)
# ?igraph





