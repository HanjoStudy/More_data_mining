##################################################
#####     ASSOCIATION ANALYSIS (Ledolter)    #####
##################################################

#### MARKET BASKET ANALYSIS: 
#### ASSOCIATION RULES AND LIFT

# Market basket analysis looks at purchase coincidence. 
# It investigates whether two products are being 
# purchased together, and whether the purchase of one 
# product increases the likelihood of purchasing the 
# other.

# The data typically encountered in these applications 
# can be arranged in a large matrix of rows and columns.

# Rows represent the different shoppers (or shopping 
# trips) and columns represent the different products. 

# The entries in the data matrix are the incidences
# (1 or 0) indicating whether or not the item in 
# column j of the matrix (say Goose Island beer) 
# is purchased by the shopper in row i (say Ledolter).

# The dimensions of the data matrix are usually quite 
# large, with the information on incidences coming from
# many shoppers (rows) and many different products
# (columns).

# Other common applications involve subscribers to 
# internet radio sites making selections to listen 
# to certain artists (bands). Internet radio (also 
# known as streaming radio or web radio) has a huge 
# following. While in the past services used to be
# free, owing to copyright considerations, sites such 
# as Last.fm now charge the users a small monthly fee
# for gaining access to their play list. 

# Data on the listening preferences of subscribers is 
# being collected continuously. Again, dimensions of
# the data matrices involved tend to be huge, with 
# many users (100,000s of users) and many different 
# artists (10,000s of artists). The entries in the data
# matrix are incidences (1/0) indicating whether or 
# not a certain user listens to a certain artist.

# Knowing which supermarket products tend to get purchased 
# together and knowing which pair of artists or pair
# of movies have high co-incidences brings important
# benefits. This knowledge allows for targeted 
# (i.e., “smart”) marketing, where customers
# who have bought one product or viewed one type 
# of movie are being targeted (or “recommended”)
# for advertisements that push related products
# that have high chances of being purchased.

# Recall from elementary probability calculus the 
# following results about probabilities:

### SUPPORT:
# P(A): Probability that product A is being purchased 
# (event A)—The proportion of times event A occurred 
# is also referred to as the support of A. It is the
# relative frequency of 1s in column A of the 
# incidence matrix.

# P(B): Probability that product B is being purchased 
# (event B)—The proportion of times event B occurred
# is referred to as the support of B. It is the relative
# frequency of 1s in column B of the incidence matrix.

# P(A and B): Probability that products A and B are 
# being purchased at the same time—The proportion of 
# times events A and B occurred together is referred
# to as the support of A and B. It is the relative 
# frequency of having 1s (i.e., of co-incidence) in 
# both columns A and B of the incidence matrix.

### ASSOCIATION RULES
# We look for association rules such as:

# A = LHS  (buy chips) → B = RHS (buy beer). 

# The left-hand side is the “antecedent”; 
# the right-hand side is the “consequent”; 
# and the arrow expresses “is related to.”

# P(B|A) is the conditional probability of 
# B given A. It expresses the probability of
# event B (the RHS: buying beer), knowing that 
# event A (the LHS: buying chips) has occurred. 
# Recall from your study of probability that 
# P(B|A) = P(A and B)/P(A).

### CONFIDENCE
# The conditional probability of B (RHS) given 
# A (LHS) is referred to as the confidence of B. 
# It expresses our confidence that product B gets 
# bought if A has been purchased. If this is a 
# small number, the relationship between 
# antecedent A and consequent B is not very 
# relevant as in this case B is unlikely to 
# occur. The confidence is calculated as the ratio, 
# supp(A = LHS and B = RHS)/supp(A = LHS), 
# with the supports of these two events being 
# obtained from the incidence matrix.

### LIFT
# The lift of A on B is defined as the ratio:

# lift(A → B) = P(B|A) / P(B) = P(A and B) / P(A)P(B).

# It compares P(B|A) with P(B). If this ratio is 
# larger than 1, we say that A (LHS) results 
# in an upward lift on B (RHS). Or, knowing that 
# A (the antecedent) has occurred, increases the
# chance that B (the consequent) occurs. 

# The lift of A (LHS) on B (RHS) is calculated as the ratio 
# supp(LHS and RHS)/[supp(LHS)supp(RHS)]. 

# Note that this is the same as the lift of B on A, 
# P(A|B)/P(A) = P(A and B)/P(A)P(B).


# You want to find antecedents that result in 
# big lifts. But, big lifts are practically
# relevant only if the consequent has a reasonable
# chance of occurring. Hence, it is productive ti
# screen for combinations that result in good lift
# AND high confidence.

# Note that confidence and lift can be both obtained
# from the incidence matrix. However, with the usually 
# very large dimensions of this matrix, supports of many
# events need to be computed. For 1000 columns (products), 
# there is the need to form half a million pairs and 
# calculate one million confidences and lifts, before the
# screening for the important events can even start. 

# Considering that the incidence matrix may have 
# 100,000 rows (customers), this certainly amounts
# to a lot of data crunching. Fortunately, efficient 
# software is available to carry out the calculations
# and screen for good lift and high confidence.

# Note also that there is no need to restrict oneself
# to just one column or item when defining the conditioning
# (the antecedent, or LHS) variable. Why not consider
# pairs or triples of columns, and study the relationship 
# between a consequent and an antecedent that is being 
# described by the joint occurrence of several items
# (columns)? If this is done, then we also need to 
# calculate the support of triples and quadruples of columns. 

# This involves even more computations and searches,
# making it even more critical that the algorithms 
# are efficient.

#### EXAMPLE 1: Online Radio 

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
# two demographic variables listed here (gender 
# and country) are not used in our analysis. 

# However, it would be straightforward to stratify the
# following market basket analysis on gender and country 
# of origin, and investigate whether findings change.

# We do this for your exercise for the next session.

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
playlist <- split(x=lastfm[,"artist"], f=lastfm$user)
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
## so it can’t be a super rare band 
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

##### Example 2: Predicting Income 

# As second example we use the Adult data set from 
# the UCI machine learning repository; this data 
# set was analyzed by Hahsler et al. (2005) in 
# their illustration of the R package arules. 

# The data, taken from the US Census Bureau database,
# contains 48,842 individuals with data on income 
# and several possible predictors of income such 
# as age, work class, education, and so on. 

# The first part of the R program in their paper 
# (copied below) transforms the original variables 
# into 115 binary indicator variables. 

# Income, for example is coded as “small” (US$50,000
# or less) and “large” (US$50,000 or more). We skip 
# these details and focus our attention on the 
# resulting transaction (incidence) matrix Adult 
# with its 48,842 rows and 115 columns. 

# Since the dimensions of the matrix are so large, 
# the matrix is stored in a special, sparse, 
# transaction matrix format. The item (column) with
# the largest number of 1s is “capital − loss = none”
# (46,560 out of 48,842). The number of 1s on a 
# subject (also referred to as the length) varies 
# between 9 and 13; this narrow band is not surprising
# as indicator variables were constructed from
# categorical variables, and we know that summing 
# over indicators of each categorical variable must
# give one for every subject. But, it is the way how
# these 0s and 1s coincide that matters. We get a 
# better understanding of how the strings of zeros
# and ones look like by considering the incidence 
# matrix (which we create from the transaction 
# matrix). The indicator for small income (0/1) is 
# in the penultimat column; the indicator for large 
# income is in the very last column.

library(arules)
data(AdultUCI)
# how many rows and columns
dim(AdultUCI)
# look at first three rows
AdultUCI[1:3,]
# remove columns we do not need
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)), labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]], c(0, 25, 40, 60, 168)), labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]], c(-Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0]), Inf)), labels = c("None", "Low", "High"))
AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss"]], c(-Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] > 0]), Inf)), labels = c("none", "low", "high"))
# make it a sparse format
Adult <- as(AdultUCI, "transactions")
Adult
summary(Adult)
# transforms transaction matrix into incidence matrix
aa=as(Adult,"matrix") 
# print the first two rows of the incidence matrix
aa[1:2,]
# look at frequencies of items
itemFrequencyPlot(Adult[, itemFrequency(Adult) > 0.2], cex.names = 1)
# create rules. We apply the function apriori. 
# This function calculates and screens the support, 
# the confidence and the lift of items.
# Consequents in apriori are single items
# (columns), but antecedents can be either 
# single items or groups of items (item sets). 
# Here we use the default specification for 
# two of apriori’sparameters, and specify minlen 
# (an integer value for the minimal number of 
# items per item set) as 1 and maxlen (an integer
# value for the maximal number of items per 
# item set) as 10. 

# This means that apriori searches over all item 
# sets for which the sum of the number of items
# in the LHS item set and the (single) item on 
# the RHS is between 1 and 10.

# This involves quite a lot of computations and queries. 
# In this particular application we consider only those
# LHS and RHS item sets for which the support
# is at least 0.01 and the confidence is at least 0.60.

rules <- apriori(Adult, 
                 parameter = list(support = 0.01, 
                                  confidence = 0.6))
# how many of them?
rules
# summary of rules
summary(rules)

# Finally, we search over a subset of all rules 
# (with support greater than 0.01 and confidence greater
# than 0.60, as specified previously) that have small 
# income on the RHS and achieve a lift larger than 1.2. 

rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
inspect(sort(rulesIncomeSmall, by = "confidence")[1:3])

# The combinations that satisfy these conditions
# are the ones that are able to predict small 
# income earners from the coded explanatory 
# information. 

# Similarly, we search and list all rules that
# identifyhigh income earners. 

# We previously found that workers in the 
# private sector working part-time tend to 
# have a small income. Now we find that persons
# with high capital gain who are born in the U.S.
# born tend to have a large income.

rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)
inspect(sort(rulesIncomeLarge, by = "confidence")[1:3])

### Example 3: Sampling with arules

# In this example, we show how sampling 
# can be used with arules. 

# We again use the Adult dataset.
data("Adult")
Adult

# To calculate a reasonable sample 
# size n, we choose a minimum support
# of 5% tied into the formula you see
# below. As an acceptable error
# rate for support we choose 10% 
# and as the confidence level 
# (1 − c) we choose 90%.

supp <- 0.05
epsilon <- 0.1
c <- 0.1
n <- -2 * log(c)/ (supp * epsilon^2)
n

# The resulting sample size is considerably smaller than 
# the size of the original database. With sample() we 
# produce a sample of size n with replacement from 
# the database.

AdultSample <- sample(Adult, n, replace = TRUE)


# The sample can be compared with the database 
# (the population) using an item frequency plot.
# The item frequencies in the sample are displayed 
# as bars and the item frequencies in the original
# database are represented by the line. 

# For better readability of the labels, we only
# display frequent items in the plot and reduce the 
# label size with the parameter cex.names.

itemFrequencyPlot(AdultSample, population = Adult, support = supp,
                  cex.names = 0.7)

# Alternatively, a sample can be compared with the 
# population using the lift ratio (with lift = TRUE).
# The lift ratio for each item i is 
# P(i|sample)/P(i|population) where the probabilities
# are estimated by the item frequencies. 

# A lift ratio of one indicates that the items occur 
# in the sample in the same proportion as in the population.

# A lift ratio greater than one indicates that the item
# is over-represented in the sample and vice versa. 

# With this plot, large relative deviations for less 
# frequent items can be identified visually.

itemFrequencyPlot(AdultSample, population = Adult,
                  support = supp, lift = TRUE,
                  cex.names = 0.9)

# To compare the speed-up reached by sampling we use 
# the Eclat algorithm to mine frequent itemsets on both,
# the database and the sample and compare the system time 
# (in seconds) used for mining.

time <- system.time(itemsets <- eclat(Adult,
                                      parameter = list(support = supp), control = list(verbose = FALSE)))
time

timeSample <- system.time(itemsetsSample <- eclat(AdultSample,
                                                  parameter = list(support = supp), control = list(verbose = FALSE)))
timeSample

# The first element of the vector returned by system.time()
# gives the (user) CPU time needed for the execution of 
# the statement in its argument. Therefore, mining the 
# sample instead of the whole data base results in a 
# speed-up factor of:

# speed up
time[1] / timeSample[1]

# To evaluate the accuracy for the itemsets mined from 
# the sample, we analyze the difference between the two sets.

itemsets
itemsetsSample

# The two sets have roughly the same size. To check if the 
# sets contain similar itemsets, we match the sets and see
# what fraction of frequent itemsets found in the database
# were also found in the sample.

match <- match(itemsets, itemsetsSample, nomatch = 0)
## remove no matches
sum(match>0) / length(itemsets)

# Almost all frequent itemsets were found using the sample. 
# The summaries of the support of the frequent itemsets 
# which were not found in the sample and the itemsets 
# which were frequent in the sample although they were 
# infrequent in the database give:

summary(quality(itemsets[which(!match)])$support)
summary(quality(itemsetsSample[-match])$support)

# This shows that only itemsets with support very close
# to the minimum support were falsely missed or found.

# For the frequent itemsets which were found in the 
# database and in the sample, we can calculate accuracy
# from the the error rate.

supportItemsets <- quality(itemsets[which(match > 0)])$support
supportSample <- quality(itemsetsSample[match])$support
accuracy <- 1 - abs(supportSample - supportItemsets) / supportItemsets
summary(accuracy)

# The summary shows that sampling resulted in finding 
# the support of itemsets with high accuracy.

# This small example illustrates that for extremely 
# large databases or for application where mining 
# time is important, sampling can be a powerful technique.