id2 = order(freqs, decreasing = TRUE)[1:50]
topWords.clus = colnames(tweet.matrix)[id2]

D = dist(freqs[id2])
h.complete = hclust(D)
plot(h.complete)

h.single = hclust(D, method="single")
plot(h.single)

plot(h.single)
plot(h.complete)

# The complete linkage one performs better clustering in comparison to the single linkage method. 
# Complete linkage clustering shows a broader seleciton of groups of higher distinction between clusters
# The biggest cluster is with the words Harry Potter while the other two major clusters are about the political opinions
# of J.K Rowlings and it's influence on her recent works
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#