##################################################
#####  ASSOCIATION ANALYSIS (WILLIAMS); ALSO #####
#####    VISUALIZING ASSOCIATION RULES       #####
##################################################

# Association analysis identifies relations or
# correlations between observations and/or
# between variables in our datasets.

# These relationships are then expressed
# as a collection of "association rules".

# Is a core technique of data mining. Is very
# useful for mining very large transactional
# databases, like shopping baskets and on-line
# customer purchases.

### Knowledge Representation: Assocation Rules

# General Format is A -> C    Can generalize
# A (the antecedent) to a specific variable
# or value combination so can apply to various
# datasets

### Search Heuristic

# Basis of an association analysis algorithm
# is the generation of frequent itemsets. Is
# an "apriori algorithm", a generate-and-test
# type of search algorithm. Only after exploring
# all of the possibilities of associations
# containing k items does it consider those
# contining K + 1 items. For each k, all candidates
# are tested to determine whether they have
# enough support.

# A frequent itemset is a set of items that
# occur together frequently enough to be 
# considered as a candidate for generating
# association rules.

### 3 Measures: Support, Confidence, Lift

# "Support" is a measure of how frequently the
# items must appear in the whole dataset before
# they can be considered as a candidate asso-
# ciation rule.

# "Support" for a collection of items is the
# proportion of all transactions in which the
# items appear together

# support(A -> C) = P(A U C)

# We use small values of "support" as are not
# looking for the obvious ones.

# The actual association rules that we retain
# are those that meet a criterion "confidence"
# "Confidence" calculates the proportion of 
# transactions containing A that also contain C.

# confidence(A -> C) = P(C|A) = P(A U C)/P(A)

# confidence(A -> C) = support(A -> C)/support(A)

# Typically looking for larger values of
# confidence

# Another measure: "Lift"
# "Lift" is the increased likelihood of C
# being in a transaction if A is included in
# the transaction:

# lift(A -> C) = confidence(A -> C)/support(C)

# Another measure: Leverage, which captures
# the fact that a higher frequency of A and C
# with a lower lift may be interesting:

# leverage(A->C)=support(A->C)-support(A)*support(C)

### Tutorial Example using Rattle: DVDs

# Two types of association rules were identified 
# corresponding to the type of data available. 
# The simplest case, known as "market basket
# analysis", is when we have a transaction 
# dataset that records just a transaction
# identifer. The identifer might identify a 
# single shopping basket containing multiple 
# items from shopping or a particular customer
# or patient and their associated purchases or 
# medical treatments over time.

# A simple example of a market basket dataset 
# might record the purchases of DVDs by customers
# (three customers in this case):

# ID, Item
# 1, Sixth Sense
# 1, LOTR1
# 1, Harry Potter1
# 1, Green Mile
# 1, LOTR2
# 2, Gladiator
# 2, Patriot
# 2, Braveheart
# 3, LOTR1
# 3, LOTR2

### Tutorial Example using R (package arules)

### Function apriori()

# When loading a dataset to process with apriori()
# it must be converted into a transaction data
# structure. Consider a basket with two columns
# one being the identifier of the "basket" and
# the other being an item contained in the basket
# as is the case for the dvdtrans.csv data.

# We load into R:
install.packages("arules")
library("arules")
dvdtrans <- read.csv(system.file("csv", "Materials-Session-2-Association-Analysis/data/dvdtrans.csv",
                                 package="rattle"))

str(dvdtrans)
dvdtrans

dvdDS <- new.env()
dvdDS$data <- as(split(dvdtrans$Item, dvdtrans$ID),
                   "transactions")
dvdDS$data

# We can then build the model
# using the tranformed dataset:
dvdAPRIORI <- new.env(parent=dvdDS)
evalq({
  model <- apriori(data, 
                   parameter=list(support=0.2,
                                        confidence=0.1))
}, dvdAPRIORI)

dvdAPRIORI$model

# The rules can be extracted and ordered by
# confidence using inspect()
inspect(sort(dvdAPRIORI$model, 
             # limit display to first 5 rules
             by="confidence")[1:5])

### Visualizing Association Rules: Introduction to
### the R-extension Package arules Viz 
### article provided (by Hahsler, Chelluboina)


# Data Preparation and Unified
# Interface of arulesViz

install.packages("arulesViz")
library("arulesViz")

# Groceries contains sales data of groceries
# with 9835 transactions and 169 items.
data("Groceries")
summary(Groceries)

# Whole milk most popular. AVerage transaction
# has less than 5 items.

# We mine association rules using
# Apriori algorithm implemented in arules

rules <- apriori(Groceries,
                 parameter=list(support=0.001,
                                confidence=0.5))

rules

# result is a set of 5668 association rules.
# The top three rules with respect to the
# lift measure, a popular measure of rule
# strength are:
inspect(head(sort(rules, by ="lift"),3))

# Impossible to go through all 5668 rules
# manually so use visualization techniques
# implemented in arulesViz

# Generic Plot Syntax is:
# plot # plot(x, method = NULL, measure = "support",
#             shading = "lift", interactive = FALSE,
#             data = data_set, control = list(reorder=TRUE))
#
# x = set of rules to be visualized
# method = visualization method
# measure and shading = interest measures for plot
# interactive = if we want to interactively explore
#   or just present the rules
# data = transaction data set used to mine rules
# control = list with further control arguments.

### Scatterplot of association rules

# with two interest measures on the axes
# default is support and confidence
# life is color (gray level) of the points.
# includes a color key to the right of the plot

plot(rules)

# We see that rules with high lift typically
# have relatively low support

# Any measure in the quality slot of the
# rules can be used for the axes or for
# color shading. These are available:

head(quality(rules))

# Here we customize the plot by
# switching lift and confidence:

plot(rules, 
     measure=c("support", "lift"), 
     shading="confidence")

# In a "two-key plot", support and confidence
# are used for the x and y-axes and the
# color of the points is used to indicate
# "order" which is the number of items
# contained in the rule.

# From plot, is clear that order and support
# have very strong inverse relationship which
# is a known fact for association rules.

# We also give plot different title

plot(rules, 
     shading="order", 
     control=list(main="Two-key plot"))

# For exploration, scatter plot method offers 
# interactive features for selecting and
# zooming. Interaction is activated using 
# interactive = TRUE.

# Interactive features include:

# Inspecting individual rules by selecting them
# and clicking the inspect button.

# Inspecting sets of rules by selecting a
# rectangular region of the plot and clicking
# the inspect button

# Zooming into a selected region 
# (zoom in/ zoom out buttons)

# Filtering rules using the measure used for 
# shading by clicking the filter button and 
# selecting a cut-off point in the color key.
# All rules with a measure lower than the
# cut-off point will be filtered.

# Returning the last selection for further 
# analysis (end button).

sel <- plot(rules, 
            measure=c("support", "lift"), 
            shading="confidence", 
            interactive=TRUE)