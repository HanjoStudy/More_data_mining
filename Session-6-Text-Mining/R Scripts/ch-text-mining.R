###################################################
######     DATA MINING II: TEXT MINING I     ######
###################################################
# free memory
rm(list = ls())
gc()

# We will mine text from Twitter. The extracted
# text will then be transformed to build a 
# document-term matrix. After that, frequent words
# and associations are found from the matrix. 

# A word cloud is used to present important words
# in documents. In the end, words and tweets are 
# clustered to find groups of words and also groups
# of tweets. The terms "tweet" and "document" are
# used interchangeably, so are "word" and "term".

# Note that the Twitter API requires authentication
# since March 2013. Before running the code, please
# complete authentication by following instructions
# in "Section 3: Authentication with OAuth" in 
# the twitteR vignettes (http://cran.r-project.org/web/packages/twitteR/vignettes/twitteR.pdf).

# Get twitter data
library(twitteR)
# load data file rdmTweets.RData"
load(file.choose())
(nDocs <- length(rdmTweets))

# look at the five tweets numbered 11 to 15.
rdmTweets[11:15]

# Each tweet above is printed in one single line, 
# which may exceed the boundary of paper so
# we use the following code to print the five tweets
# by wrapping the text to fit the width of paper. 

# The same method is used to print tweets in other
# codes in this session
for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(rdmTweets[[i]]$getText(), width=73))
} 

### Transforming the Text
# The tweets are first converted to a data frame 
# and then to a corpus, which is a collection of
# text documents. Then the corpus can be processed
# with functions provided in package tm [Feiner, 2012].

# convert tweets to a data frame
df <- do.call("rbind", lapply(rdmTweets, 
                              as.data.frame))
dim(df)

library(tm)  
# build a corpus, and specify the source
# to be character vectors of df$text
myCorpus <- Corpus(VectorSource(df$text))
str(myCorpus)

# We perform some transformations to the Corpus, 
# including changing letters to lower case and
# removing punctuations, numbers and stop words. 

# The general English stop-word list is tailored here
# by adding "available" and "via" and removing "r" and 
# "big" (for big data). We also remove hyperlinks.

# convert to lower case  
myCorpus <- tm_map(myCorpus, tolower)  
# remove punctuation  
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers  
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)  

## STEMMING WORDS

# In many applications, words need to be stemmed to 
# retrieve their radicals, so that various forms
# derived from a stem would be taken as the same 
# when counting word frequency. For instance, the
# words "update", "updated" and "updating" would all
# be stemmed to "updat". Word stemming can be done
# with the snowball stemmer, which requires packages 
# Snowball , RWeka, rJava and RWekajars. After that, 
# we can complete the stems to their original forms, 
# i.e., "update" for the above example, so that the
# words would look normal. This can be achieved 
# with function stemCompletion().

install.packages("SnowballC")

# keep a copy of corpus to use later as a 
# dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect documents (tweets) numbered 11 to 15
# inspect(myCorpus[11:15])
# The code below is used for to make text fit for paper width
for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=73))
}

# After that, we use stemCompletion() to complete
# the stems with the unstemmed corpus myCorpusCopy
# as a dictionary. With the default setting,
# it takes the most frequent match in dictionary
# as completion.

# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)

# have a look at the documents numbered 
# 11 to 15 in the built corpus.
inspect(myCorpus[11:15])

# so it will fit
for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=73))
}  

# As we can see from the above results, 
# there are issues in the above stemming
# and completion.

# 1. In both the stemmed corpus and the 
# completed one, "memoryand" is derived from 
# "... memory, and ..." in original tweet 11.

# 2. In tweet 11, word "bigmemory" is stemmed 
# to "bigmemori", and then is removed during
# stem completion.

# 3. Word "mining" in tweets 12, 13 & 15 is first
# stemmed to "mine" and then completed to "miners".

# 4. "Laboratory" in tweet 14 is stemmed to "laboratori" 
# and then also disappears after completion.

# In the above issues, point 1 is caused by 
# the missing of a space after the comma. 
# It can be easily fixed by replacing comma 
# with space before removing punctuation marks.

# For points 2 & 4, we don't know why it happened like that. 
# Fortunately, the words involved in points 1, 2 & 4
# are not important in @RDataMining tweets and
# ignoring them does not harm this demonstration of text mining.

# count frequency of "mining"
miningCases <- tm_map(myCorpusCopy, grep, pattern="\\<mining")
sum(unlist(miningCases))
# count frequency of "miners"
minerCases <- tm_map(myCorpusCopy, grep, pattern="\\<miners")
sum(unlist(minerCases))
# replace "miners" with "mining"
myCorpus <- tm_map(myCorpus, gsub, pattern="miners", replacement="mining")

# In the first call of function tm_map() above, 
# grep() is applied to every document (tweet)
# with argument "pattern="\\<mining"". 

# The pattern matches words starting with "mining",
# where \\<" matches the empty string at the beginning 
# of a word. This ensures that text "rdatamining" 
# would not contribute to the above
# counting of "mining".

## BUILDING A TERM-TEXT DOCUMENT

# A term-document matrix represents the relationship 
# between terms and documents, where each row stands
# for a term and each column for a document, 
# and an entry is the number of occurrences of
# the term in the document. 

# Alternatively, one can also build a 
# document-term matrix by swapping row and
# column. Here we build a term-document matrix
# from the above processed corpus with function
# TermDocumentMatrix(). 

# With the default setting, terms with less
# than three characters are discarded. 

# To keep "r" in the matrix, we set the 
# range of wordLengths in the example below.

myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
myTdm
inspect(myTdm)

# As we can see from the above result, the 
# term-document matrix is composed of 479 terms
# and 154 documents. 

# It is very sparse, with 98% of the entries
# being zero. We then have a look at the first
# six terms starting with "r" and tweets
# numbered 101 to 110.
idx <- which(dimnames(myTdm)$Terms == "r")
inspect(myTdm[idx+(0:5),101:110])

# Can get list of terms can be retrieved with
rownames(myTdm)

### FREQUENT TERMS AND ASSOCIATIONS

# We look at the popular words and the 
# association between words. Note that 
# there are 154 tweets in total.

# inspect frequent words
findFreqTerms(myTdm, lowfreq=10)

# In the code above, findFreqTerms() finds
# frequent terms with frequency no less than ten.

# Note that they are ordered alphabetically, 
# instead of by frequency or popularity.

# To show the top frequent words visually,
# we next make a barplot for them. From the 
# termdocument matrix, we can derive the 
# frequency of terms with rowSums(). 

# Then we select terms that appears in ten
# or more documents and shown them with a 
# barplot using package ggplot2.

# In the code below, geom="bar" specifies 
# a barplot and coord_flip() swaps x- and y-axis.

# The barplot shows that the three most 
# frequent words are "r", "data" and "mining".
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
library(ggplot2)
qplot(names(termFrequency), termFrequency, 
      geom="bar", xlab="Terms")  + coord_flip()

# Alternatively, the above plot can also be drawn
# with barplot() as below, where las sets the
# direction of x-axis labels to be vertical.
barplot(termFrequency, las=2)

# We can also find what are highly associated 
# with a word with function findAssocs(). Here
# we try to find terms occur with 
# "r" (or "mining") with correlation no less
# than 0.25, and the words are ordered by 
# their correlation with "r" (or "mining").

# which words are associated with "r"? 
findAssocs(myTdm, 'r', 0.25)  
# which words are associated with "mining"?
findAssocs(myTdm, 'mining', 0.25)

### WORD CLOUD

# After building a term-document matrix, we can 
# show the importance of words with a word cloud
# (also known as a tag cloud), which can be 
# easily produced with package wordcloud.

# In this code below, we first convert the 
# term-document matrix to a normal matrix, and
# then calculate word frequencies.

# Then we set gray levels based on word frequency
# and use wordcloud() to make a plot for it. 

# With wordcloud(), the first two parameters 
# give a list of words and their frequencies. 

# Words with frequency below three are not plotted,
# as specified by min.freq=3. 

# By setting random.order=F, frequent words 
# are plotted first, which makes them appear
# in the center of cloud. We also set the colors
# to gray levels based on frequency. A colorful
# cloud can be generated by setting colors with 
# rainbow().

library(wordcloud)
m <- as.matrix(myTdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,
          colors=grayLevels)

# The above word cloud clearly shows again that "r",
# "data" and "mining" are the top three words which
# validates that the @RDataMining tweets present 
# information on R and data mining.

# Some other important words are "analysis", 
# "examples", "slides", "tutorial" and "package", 
# which shows that it focuses on documents and
# examples on analysis and R packages. 

# Another set of frequent words, "research", 
# "postdoctoral" and "positions", are from 
# tweets about vacancies on post-doctoral and 
# research positions. 

# There are also some tweets on the topic of 
# social network analysis, as indicated by words 
# network" and "social" in the cloud.

#### CLUSTERING WORDS

# We then find clusters of words with hierarchical
# clustering. Sparse terms are removed, so that
# the plot of clustering will not be crowded with
# words. Then the distances between terms are
# calculated with dist() after scaling. 

# After that, the terms are clustered with 
# hclust() and the dendrogram is cut into 
# 10 clusters. The agglomeration method is 
# set to ward, which denotes the increase in
# variance when two clusters are merged. 

# Some other options are single linkage, complete
# linkage, average linkage, median and centroid. 

# remove sparse terms
myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
m2 <- as.matrix(myTdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward")
plot(fit)
# cut tree into 10 clusters
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))

# In the above dendrogram, we can see the 
# topics in the tweets. Words "analysis", "network"
# and "social" are clustered into one group, because
# there are a couple of tweets on social network
# analysis.

# The second cluster from left comprises "positions", 
# "postdoctoral" and "research", and they are clustered
# into one group because of tweets on vacancies 
# of research and postdoctoral positions.

# We can also see cluster on time series, R packages,
# parallel computing, R codes and examples,
# and tutorial and slides. 

# The rightmost three clusters consists of "r", 
# "data"and "mining", which are the keywords of 
# @RDataMining tweets.

# save m2 for social network analysis later
termDocMatrix <- m2
save(termDocMatrix, file="data/termDocMatrix.rdata")

### CLUSTERING TWEETS WITH K-MEANS

# We first try k-means clustering, which takes
# the values in the matrix as numeric.

# We transpose the term-document matrix to a 
# document-term one. The tweets are then
# clustered with kmeans() with the number of 
# clusters set to eight. After that, we check
# the popular words in every cluster and also the
# cluster centers. Note that a fixed random seed
# is set with set.seed() before running kmeans(),
# so the clustering result can be reproduced.

# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k <- 8
kmeansResult <- kmeans(m3, k)
# cluster centers
round(kmeansResult$centers, digits=3)

# To make it easy to find what the clusters
# are about, we then check the top three words
# in every cluster.
for (i in 1:k) {
  cat(paste("cluster ", i, ":  ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:3], "\n")
  # print the tweets of every cluster
  # print(rdmTweets[which(kmeansResult$cluster==i)])
}

# From the above top words and centers of clusters,
# we can see that the clusters are of different
# topics. For instance, cluster 1 focuses on 
# R codes and examples, cluster 2 on data mining
# with R, cluster 4 on parallel computing in R,
# cluster 6 on R packages and cluster 7 on slides
# of time series analysis with R. 

# We can also see that, all clusters, except for
# cluster 3, 5 & 8, focus on R.

# Cluster 3, 5 & 8 are about general information
# on data mining and are not limited to R. Cluster
# 3 is on social network analysis, cluster 5 on
# data mining tutorials, and cluster 8 on 
# positions for data mining research.


#### CLUSTERING TWEETS USING K-MEDOIDS ALGORITHM

# We then try k-medoids clustering with the 
# Partitioning Around Medoids (PAM) algorithm, 
# which uses medoids (representative objects) 
# instead of means to represent clusters. 

# It is more robust to noise and outliers than
# k-means clustering, and provides a display of
# the silhouette plot to show the quality of
# clustering. 

# In this example, we use function pamk() 
# from package fpc, which calls the function 
# pam() with the number of clusters estimated
# by optimum average silhouette.

library(fpc)
# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric="manhattan")
# number of clusters identified
(k <- pamResult$nc)
pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat(paste("cluster", i, ":  "))
  cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
  # print tweets in cluster i
  # print(rdmTweets[pamResult$clustering==i])
}

# plot clustering result
layout(matrix(c(1,2),2,1)) # set to two graphs per page 
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,
     col.p=pamResult$clustering)
layout(matrix(1)) # change back to one graph per page 

# The first chart is a 2D "clusplot" (clustering 
# plot) of the k clusters, and the second one shows
# their silhouettes. 

# A large s1 (almost 1) suggests that the
# corresponding observations are very well 
# clustered, a small si (around 0) means that
# the observation lies between two clusters, 
# and observations with a negative si are
# probably placed in the wrong cluster. 

# The average silhouette width is 0.29, which
# suggests that the clusters are not well
# separated from one another.

# These show that there are nine clusters of tweets.
# Clusters 1, 2, 3, 5 and 9 are well separated
# groups, with each of them focusing on a 
# specific topic. 

# Cluster 7 is composed of tweets not fitted
# well into other clusters, and it overlaps
# all other clusters. There is also a big overlap
# between cluster 6 and 8, which is understandable
# from their medoids. 

# Some observations in cluster 8 are of negative
# silhouette width, which means that they may 
# better in other clusters than cluster 8.

# To improve the clustering quality, we have also
# tried to set the range of cluster numbers
# krange=2:8 when calling pamk(), and in the
# new clustering result, there are eight clusters, 
# with the observations in the above cluster 8 
# assigned to other clusters, mostly to cluster 6. 

