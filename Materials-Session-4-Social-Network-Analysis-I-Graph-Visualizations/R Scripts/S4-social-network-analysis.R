##################################################
#####     SOCIAL NETWORK ANALYSIS (Zhao)     #####
##################################################

# We begin with examples of social network analysis 
# with R, specifically, with  the package igraph.
# We use Twitter text data putting it in a general
# scenario of social networks. The terms can be taken 
# as people and the tweets as groups on LinkedIn, and 
# the term-document matrix can then be taken as the 
# group membership of people.

# We first build a network of terms based on their 
# co-occurrence in the same tweets, and then build a 
# network of tweets based on the terms shared by them. 

# And lastly, we build a two-mode network composed of 
# both terms and tweets. We also demonstrate some tricks
# to plot nice network graphs. 

### NETWORK OF TERMS

# free memory
rm(list = ls())
gc()

# At first, a term-document matrix, termDocMatrix.rdata, 
# is loaded into R. After that, it is transformed into 
# a term-term adjacency matrix, based on which a graph 
# is built. Then we plot the graph to show the relationship
# between frequent terms, and also make the graph more 
# readable by setting colors, font sizes and transparency 
# of vertices and edges.

# load termDocMatrix.rdata
load(file.choose())

# inspect part of the matrix
termDocMatrix[5:10,1:20]

# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
termDocMatrix[5:10,1:20]

# %*% is an operator for the product of 
# two matrices, and t() transposes a
# matrix. Here we build a term-term 
# adjacency matrix, where the rows and columns 
# represent terms, and every entry is the number 
# of concurrences of two terms.

# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]

# Now we build a graph with the graph.adjacency() 
# function from package igraph.

library(igraph)

# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
plot(g)

# remove loops
g <- simplify(g)
plot(g)

# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
V(g)
plot(g)

# After that, we plot the network with 
# layout.fruchterman.reingold

# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

# A different layout can be generated with the first 
# line of code below. The second line produces an
# interactive plot, which allows us to manually 
# rearrange the layout. Details about other layout
# options can be obtained by running ?igraph::layout in R.
?igraph::layout
plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)

# We can also save the network graph into a .PDF file with:
# to set the directory
setwd(choose.dir())

# check it
getwd()

# save as a pdf
pdf("term-network.pdf")
plot(g, layout=layout.fruchterman.reingold) 
dev.off()

# Next, we set the label size of vertices based on 
# their degrees, to make important terms stand out.
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
plot(g)

# Function rgb(red, green, blue, alpha) defines a color,
# with an alpha transparency.
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
plot(g)

# We also set the width and transparency of edges 
# based on their weights. This is useful in applications
# where graphs are crowded with many vertices and edges. 
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)

### NETWORK OF TWEETS

# We can also build a graph of tweets base on the 
# number of terms that they have in common. 

# Because most tweets contain one or more words from
# "r", data" and "mining", most tweets are connected 
# with others and the graph of tweets is very crowded. 

# To simplify the graph and to find relationship 
# between tweets beyond the above three keywords, we
# remove the three words before building a graph.

# remove "r", "data" and "mining"
idx <- which(dimnames(termDocMatrix)$Terms %in% c("r", "data", "mining"))
M <- termDocMatrix[-idx,]
M[5:10,1:20]

# build a tweet-tweet adjacency matrix
tweetMatrix <- t(M) %*% M
tweetMatrix[5:10,1:20]

# really do not need to do this again unless just
# starting here
library(igraph)
g <- graph.adjacency(tweetMatrix, weighted=T, 
                     mode = "undirected")
V(g)$degree <- degree(g)
g <- simplify(g)
plot(g)

# set labels of vertices to tweet IDs
V(g)$label <- V(g)$name
V(g)$label.cex <- 1
V(g)$label.color <- rgb(.4, 0, 0, .7)
V(g)$size <- 2
V(g)$frame.color <- NA
plot(g)

# Next, we have a look at the distribution of 
# degree of vertices. We can see that there are 
# around 40 isolated vertices (with a degree of 
# zero). Note that most of them are caused by the 
# removal of the three keywords, "r", 
# "data" and "mining".

barplot(table(V(g)$degree))

# With this code, we set vertex colors based 
# on degree, and set labels of isolated vertices
# to tweet IDs and the first 20 characters of 
# every tweet. The labels of other vertices are 
# set to tweet IDs only, so that the graph will 
# not be overcrowded with labels. We also set 
# the color and width of edges based on their
# weights.
idx <- V(g)$degree == 0
V(g)$label.color[idx] <- rgb(0, 0, .3, .7)

# load twitter text (must install it first)
library(twitteR)

# load file "rdmTweets.RData"
load(file.choose())

# convert tweets to a data frame
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

# set labels to the IDs and the first 20 characters of tweets
V(g)$label[idx] <- paste(V(g)$name[idx], substr(df$text[idx], 1, 20), sep=": ")
egam <- (log(E(g)$weight)+.2) / max(log(E(g)$weight)+.2)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam

# then run the plot
set.seed(3152)
layout2 <- layout.fruchterman.reingold(g)
plot(g, layout=layout2) 

# The vertices in crescent are isolated from all 
# others, and next we remove them from graph with
# function delete.vertices() and re-plot the graph.
g2 <- delete.vertices(g, V(g)[degree(g)==0])

# run plot again
plot(g2, layout=layout.fruchterman.reingold) 

# Similarly, we can also remove edges with low degrees
# to simplify the graph. Below with function
# delete.edges(), we remove edges which have weight 
# of one. After removing edges, some vertices
# become isolated and are also removed.
g3 <- delete.edges(g, E(g)[E(g)$weight <= 1])
g3 <- delete.vertices(g3, V(g3)[degree(g3) == 0])

# run the plot
plot(g3, layout=layout.fruchterman.reingold) 

# there are some groups (or cliques) of tweets. 
# Let's have a look at the group in the middle 
# left of the figure.
df$text[c(7,12,6,9,8,3,4)]

# easier with a for statement
for (i in c(7,12,6,9,8,3,4)) {
  cat(paste("[", i, "] ", sep=""))
  writeLines(strwrap(df$text[i], 76))
  cat("\n")
}

# We can see that tweets 7, 12, 6, 9, 8, 3, 4 are 
# on parallel Computing with R. We can also see
# some other groups below:
# Tweets 4, 33, 94, 29, 18 and 92: tutorials for R;
#ˆ Tweets 4, 5, 154 and 71: R packages;
#ˆ Tweets 126, 128, 108, 136, 127, 116, 114 and 96:
# time series analysis;
#ˆ Tweets 112, 129, 119, 105, 108 and 136:
# R code examples; and
#ˆ Tweets 27, 24, 22,153, 79, 69, 31, 80, 21, 29,
# 16, 20, 18, 19 and 30: social network analysis.
# Tweet 4 lies between multiple groups, because 
# it contains keywords "parallel computing", "tutorial"
# and "package".


### TWO-MODE NETWORK

# Here we will build a two-mode network, which 
# is composed of two types of vertices: tweets
# and terms. At first, we generate a graph g 
# directly from termDocMatrix. Then different
# colors and sizes are assigned to term vertices 
# and tweet vertices. We also set the width and 
# color of edges. The graph is then plotted with 
# layout.fruchterman.reingold.

# create a graph
g <- graph.incidence(termDocMatrix, mode=c("all"))

# get index for term vertices and tweet vertices
nTerms <- nrow(M)
nDocs <- ncol(M)
idx.terms <- 1:nTerms
idx.docs <- (nTerms+1):(nTerms+nDocs)

# set colors and sizes for vertices
V(g)$degree <- degree(g)
V(g)$color[idx.terms] <- rgb(0, 1, 0, .5)
V(g)$size[idx.terms] <- 6
V(g)$color[idx.docs] <- rgb(1, 0, 0, .4)
V(g)$size[idx.docs] <- 4
V(g)$frame.color <- NA

# set vertex labels and their colors and sizes
V(g)$label <- V(g)$name
V(g)$label.color <- rgb(0, 0, 0, 0.5)
V(g)$label.cex <- 1.4*V(g)$degree/max(V(g)$degree) + 1

# set edge width and color
E(g)$width <- .3
E(g)$color <- rgb(.5, .5, 0, .3)

# set seed for replicability
set.seed(958)

# run the plot
plot(g, layout=layout.fruchterman.reingold)

# The plot shows that most tweets are around two 
# centers, "r" and "data mining". Next, let's
# have a look at which tweets are about "r".

# In the code below, nei("r") returns all vertices
# which are neighbors of vertex "r".
V(g)[nei("r")]

# An alternative way is using function 
# neighborhood()
V(g)[neighborhood(g, order=1, "r")[[1]]]

# We can also have a further look at which 
# tweets contain all three terms: "r", "data" 
# and "mining".
(rdmVertices <- V(g)[nei("r") & nei("data") & nei("mining")])

# To make it short, only the first 10 tweets are 
# displayed in the above result. In the above code,
# df is a data frame which keeps tweets of 
# RDataMining,
df$text[as.numeric(rdmVertices$label)]

# easier like this
for (i in as.numeric(rdmVertices$label)[1:10]) {
  cat(paste("[", i, "] ", sep=""))
  writeLines(strwrap(df$text[i], 76))
  cat("\n")
}

# Next, we remove "r", "data" and "mining" to show 
# the relationship between tweets with other words.
# Isolated vertices are also deleted from graph.

idx <- which(V(g)$name %in% c("r", "data", "mining"))
g2 <- delete.vertices(g, V(g)[idx-1])
g2 <- delete.vertices(g2, V(g2)[degree(g2)==0])
set.seed(209)
plot(g2, layout=layout.fruchterman.reingold)

# we can clearly see groups of tweets and their 
# keywords, such as time series, social network 
# analysis, parallel computing and postdoctoral 
# and research positions, which are similar to 
# the result presented previously120yesMost owww97.50.
