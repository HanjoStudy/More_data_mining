##################################################
#####     ASSOCIATION ANALYSIS EXERCISE      #####
##################################################

#### FROM EXAMPLE 1 IN SESSION #3: ONLINE RADIO

# HERE IS THE ORIGINAL SCRIPTS FROM Example 1: Online Radio.
# TAKE THIS EXAMPLE AND THESE SCRIPTS AND STRATIFY THE
# FOLLOWING MARKET BASKET ANALYSIS ON GENDER AND COUNTRY
# OF ORIGIN, AND INVESTIGATE WHETHER FINDINGS CHANGE.

# Online radio keeps track of everything you play. 
# It uses this information for recommending music
# you are likely to enjoy and supports focused 
# marketing that sends you advertisements for music 
# you are likely to buy. Why waste scarce advertising
# dollars on items that customers are unlikely to purchase.

# Suppose you were given data from a music community
# site. For each user you may have a log of every 
# artist he/she had downloaded to their computer. 

# You may even have demographic information on the user 
# (such as age, sex, location, occupation, and interests). 

# Your objective is to build a system that 
# recommends new music to users in this community. 

# From the available information, it is quite easy 
# to determine the support for (i.e., the 
# frequencies of listening to) various individual
# artists, as well as the joint support for pairs 
# (or larger groupings) of artists.

# All you need do is count the incidences (0/1)
# across all members of your network and divide
# those frequencies by the number of your members. 

# From the support we can calculate the 
# confidence and the lift.

# We use a large data set with close to 300,000
# records of song (artist) selections made by 
# 15,000 users. Each row of our data set
# contains the name of the artist the user has 
# listened to. Our first user, a woman from
# Germany, has listened to 16 artists, resulting 
# in the first 16 rows of the data matrix. The 
# two demographic variables listed here (gender and country)
# are not used in our analysis. 

### *** Play counts *** ###

# read in lastfm.csv file
lastfm <- read.csv(file.choose())

# how many rows and columns
dim(lastfm)

# names of variables
names(lastfm)

# types of variables
str(lastfm)

# list first 19 records
lastfm[1:19,]
length(lastfm$user)   ## 289,955 records in the file

# Note date is in "tall" format, we
# do not have one record per user

# make user a factor
lastfm$user <- factor(lastfm$user)

# how many different users?
levels(lastfm$user)   ## 15,000 users

# how many artists
levels(lastfm$artist) ##  1,004 artists

# load arules package
library(arules) ## a-rules package for association rules
## Computational environment for mining association rules
## and frequent item sets 

## we need to manipulate the data a bit for arules
## split into a list of users
?split
playlist <- split(x=lastfm[,"artist"],f=lastfm$user)
# each component of list is now one user and we
# see which artists each one listens to.
str(playlist)

## remove artist duplicates
playlist <- lapply(playlist,unique) 
# now look
str(playlist)

## the first two listeners (1 and 3) 
# listen to the following bands
length(playlist)
playlist[1:2]

## view this as a list of "transactions"
## transactions is a data class defined
## in arules
playlist <- as(playlist,"transactions")

## lists the support of the 1,004 bands
## number of times band is listed to on the 
## shopping trips of 15,000 users computes
## the rel freq each artist mentioned by 
## the 15,000 users
itemFrequency(playlist) 

## plots the item frequencies (only bands with > % support)
itemFrequencyPlot(playlist,support=.08,cex.names=1.5) 

## Finally, we build the association rules 
## only rules with support > 0.01 and confidence > .50
## so it can't be a super rare band 
musicrules <- apriori(playlist,parameter=list(support=.01,confidence=.5)) 

# and inspect the 50 rules
inspect(musicrules)

## let's filter by lift > 5.0
## Among those associations with 
## support > 0.01 and confidence > .50, 
## only show those with lift > 5
inspect(subset(musicrules, subset=lift > 5)) 

## lastly, order by confidence to make it 
## easier to understand
inspect(sort(subset(musicrules, subset=lift > 5), 
             by="confidence"))
