###################################################
#####      ASSOCIATION RULE MINING WITH R     #####
###################################################

# So we are talking about association rule mining
# with R.

# free memory
rm(list = ls())
gc()

# Basics of Association Rules

# Association rules are rules presenting association 
# or correlation between itemsets. An association
# rule is in the form of A ==> B, where A and B are 
# two disjoint itemsets, referred to respectively
# as the lhs (left-hand side) and rhs (right-hand side)
# of the rule. The three most widely-used measures
# for selecting interesting rules are:

# support, 
# confidence and 
# lift. 

# Support is the percentage of cases in the data that 
# contain both A and B, or the Union of A and B,
# i.e. support(A => B) = P(A U B)

# Confidence is the percentage of cases containing
# A that also contain B, 
# i.e. confidence(A => B) = P(B|A) = P(A U B)/P(A)

# Lift is the ratio of confidence to the percentage 
# of cases containing B,
# i.e. life(A => B) = confidence(A => B)/P(B) or
# P(A U B)/P(A)P(B)

# where P(A) and P(B) are the percentages (or probability)
# of cases containing A.

# In addition to support, confidence and lift, there 
# are many other interestingness measures, such as
# chi-square, conviction, gini and leverage.

## The Titanic Dataset

# The Titanic dataset in the datasets package is a 
# 4-dimensional table with summarized information
# on the fate of passengers on the Titanic according 
# to social class, sex, age and survival. 

# You have an external R object version of it
# available in your folder, but it is already loaded
# into your session via the datasets package.

# To make it suitable for association rule mining, 
# we reconstruct the raw data as titanic.raw, where 
# each row represents a person.

# Is a table at the moment
data(Titanic)
View(Titanic)

# Is a table....this is why we cannot see frequency
str(Titanic)

# Make it a data frame
df <- as.data.frame(Titanic)

# View first 6 records
head(df)

# make new object to hold data
titanic.raw <- NULL

# restructure it as a character matrix
for(i in 1:4) {
   titanic.raw <- cbind(titanic.raw, rep(as.character(df[,i]), df$Freq))
}

# take a look
str(titanic.raw)

# turn it back into a data frame, we created one record of 
# a person for each count in the frequency table
titanic.raw <- as.data.frame(titanic.raw)

# take another look
str(titanic.raw)

# put the variable names back
names(titanic.raw) <- names(df)[1:4]
dim(titanic.raw)
str(titanic.raw)
head(titanic.raw)
summary(titanic.raw)

# So now we have a dataset where each row stands for
# a person, and it can be used for association 
# rule mining.

## Association Rule Mining

# A classic algorithm for association rule mining is APRIORI 
# [Agrawal and Srikant, 1994]. It is a level-wise, breadth-
# first algorithm which counts transactions to find frequent 
# itemsets and then derive association rules from them. 

# An implementation of it is function apriori() in package
# arules [Hahsler et al., 2011]. Another algorithm for association 
# rule mining is the ECLAT algorithm [Zaki, 2000], which finds
# frequent itemsets with equivalence classes, depth-first search
# and set intersection instead of counting. It is implemented 
# as function eclat() in the same package.

# Here we demonstrate association rule mining with apriori(). 
# With the function, the default settings are: 1) supp=0.1, 
# which is the minimum support of rules; 2) conf=0.8, which is the
# minimum confidence of rules; and 3) maxlen=10, which is
# the maximum length of rules.

# Package arules provides the infrastructure for 
# representing,manipulating and analyzing transaction
# data and patterns (frequent itemsets and association 
# rules). Also provides interfaces to
# C implementations of the association mining algorithms
# Apriori and Eclat by C. Borgelt.
library(arules)
# Take a look at the apriori function
?apriori
# find association rules with default settings
rules.all <- apriori(titanic.raw)
rules.all
inspect(rules.all)

# As a common phenomenon for association rule mining, 
# many rules generated above are uninteresting.

# Suppose that we are interested in only rules with 
# rhs indicating survival, so we set rhs=c("Survived=No", 
# "Survived=Yes") in appearance to make sure that only
# "Survived=No" and "Survived=Yes" will appear in the 
# rhs of rules. 

# All other items can appear in the lhs, as set with
# default="lhs". In the above result rules.all, we can 
# also see that the left-hand side (lhs) of the first 
# rule is empty. To exclude such rules, we set minlen 
# to 2 in the code below. Moreover, the details of the
# progress are suppressed with verbose=F. 

# After association rule mining, rules are sorted by
# lift to make high-lift rules appear first.

# rules with rhs containing "Survived" only
rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                                   default="lhs"))
quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="lift")

inspect(rules.sorted)

# When other settings are unchanged, with a lower minimum 
# support, more rules will be produced, and the
# associations between itemsets shown in the rules will be 
# more likely to be by chance. 

# In the above code, the minimum support is set to 0.005,
# so each rule is supported at least by 
# 12 (=ceiling(0.005 * 2201)) cases, which is acceptable
# for a population of 2201.

# Support, confidence and lift are three common measures
# for selecting interesting association rules. Besides them, 
# there are many other interestingness measures, 
# such as chi-square, conviction, gini and leverage.

# More than 20 measures can be calculated with 
# function interestMeasure() in the arules package.

## Removing Redundancy

# Some rules generated in the previous section 
# (see rules.sorted, page 89) provide little or no
# extra information when some other rules are in the result. 

# For example, the above rule 2 provides no extra knowledge
# in addition to rule 1, since rules 1 tells us that all 
# 2nd-class children survived.

# Generally speaking, when a rule (such as rule 2) is a super
# rule of another rule (such as rule 1) and the former 
# has the same or a lower lift, the former rule 
# (rule 2) is considered to be redundant.

# Other redundant rules in the above result are rules 4, 7 and 8, 
# compared respectively with rules 3, 6 and 5.

# Below we prune redundant rules. Note that the rules
# have already been sorted descendingly by lift.

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

# In the code above, function is.subset(r1, r2) checks 
# whether r1 is a subset of r2 (i.e., whether r2 is a 
# superset of r1). 

# Function lower.tri() returns a logical matrix with TRUE
# in the lower triangle.

# From the above results, we can see that rules 2, 4, 7 
# and 8 (before redundancy removal) are successfully pruned.

## Interpreting Rules

# While it is easy to find high-lift rules from data, 
# it is not an easy job to understand the identified
# rules. It is not uncommon that the association rules 
# are misinterpreted to find their business meanings.

# For instance, in the above rule list rules.pruned, 
# the first rule "{Class=2nd, Age=Child} => {Survived=Yes}" 
# has a confidence of one and a lift of three and there 
# are no rules on children of the 1st or 3rd classes. 

# Therefore, it might be interpreted by users as 
# children of the 2nd class had a higher survival rate 
# than other children. This is wrong! The rule states only 
# that all children of class 2 survived, but provides 
# no information at all to compare the survival rates of
# different classes. 

# To investigate the above issue, we run the code 
# below to find rules whose rhs is "Survived=Yes" and 
# lhs contains "Class=1st", "Class=2nd", "Class=3rd", 
# "Age=Child" and "Age=Adult" only, and which contains 
# no other items (default="none"). We use lower thresholds
# for both support and confidence than before to find 
# all rules for children of different classes.

rules <- apriori(titanic.raw, 
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult"),
                                   default="none"), 
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

# In the above result, the first two rules show that children 
# of the 1st class are of the same survival rate as children
# of the 2nd class and that all of them survived. 

# The rule of 1st-class children didn't appear before, 
# simply because of its support was below the threshold 
# specified in Section 9.3.

# Rule 5 presents a sad fact that children of class 3 had 
# a low survival rate of 34%, which is comparable with that
# of 2nd-class adults (see rule 4) and much lower than 
# 1st-class adults (see rule 3).

## Visualizing Association Rules

# Here we show some ways to visualize association rules, 
# including scatter plot, balloon plot, graph and parallel
# coordinates plot. More examples on visualizing association
# rules can be found in the vignettes of package arulesViz 
# [Hahsler and Chelluboina, 2012] which is included in
# your folder of class materials for today.

library(arulesViz)

# figure 9.1 in PDF
plot(rules.all) # figure 9.1 in PDF

# figure 9.2 in PDF
plot(rules.all, method="grouped")

# figure 9.3 in PDF
plot(rules.all, method="graph")

# figure 9.4
plot(rules.all, method="graph", control=list(type="items"))

# figure 9.5
plot(rules.all, method="paracoord", control=list(reorder=TRUE))


