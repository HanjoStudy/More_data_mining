#############################################
#####   Network Data: More Examples     #####
#############################################

#### NETWORK DATA 

# Network data consist of nodes (also called 
# vertices); such as individuals in a social
# network, or companies in a trade network, 
# and edges (the links between them).

# The links describe the presence or absence of 
# connections among the nodes. Links can be either
# directed or undirected. A connection from node x 
# to node y is called directed if y is a direct 
# successor of x; in this case the edge from x to y
# includes an arrow that points to y.

# A network of nodes and their interconnections can 
# be represented with an adjacency matrix.

# Below, we list the adjacency matrix A for a 
# directed graph with three nodes; the relationships
# among the nodes are expressed with directed arrows. 

# The first node points to the second node and to 
# the third node. The second node points to the 
# first node, and the third node points to the 
# second node.

#     [0 1 1]
# A = [1 0 0]
#     [0 1 0]


library(igraph)
m=matrix(nrow=3,ncol=3)
m
m[1,1]=0
m[1,2]=1
m[1,3]=1
m[2,1]=1
m[2,2]=0
m[2,3]=0
m[3,1]=0
m[3,2]=1
m[3,3]=0
m

# create the label
lab=c(1,2,3)
# create the object
object <- graph.adjacency(m,mode="directed") 
set.seed(1)
plot(object,vertex.label=lab)

### Example 1: Marriage and Power in 15th
### Century Florence

# Early Renaissance Florence was ruled by an 
# oligarchy of powerful families. By the 15th
# century, the Medicis emerged supreme, and the 
# Medici Bank became the largest in Europe. 

# How did the Medici win?
# Political ties were established via marriage. 
# The adjacency matrix for the 16 most powerful
# families in Florence shown in the following 
# includes a one whenever two families were connected 
# in marriage. This adjacency matrix represents an
# undirected graph. A graphical representation of the 
# network, created with the R package igraph, is shown.

# The data set analyzed here has been compiled from 
# extensive data collected by John Padgett. The data 
# includes information on families who were locked in
# a struggle for political control of the city of Florence
# around 1430. Two factions were dominant in this struggle:
# one revolved around the infamous Medicis, the other
# around the powerful Strozzis. 

# The data can be downloaded from
# http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm#padgett. The papers
# by Padgett (1994), Padgett and Ansel (1993), Breiger 
# and Pattison (1986), and Wasserman and Faust (1994) 
# provide a detailed discussion of this fascinating
# dataset that has been constructed from historical
# documents. All we see here is a simple adjacency 
# matrix of zeros and ones, but not the enormous
# research effort that must have gone into constructing
# this matrix.

library(igraph) ## load the package 
## read the firenze.csv data
florence <- as.matrix(read.csv(file.choose()))
# take a look
florence

# create adjacency matrix:
marriage <- graph.adjacency(florence, mode="undirected", diag=FALSE)
## use the help function to understand the options for the graph  
set.seed(1)
plot(marriage,layout=layout.fruchterman.reingold,
     vertex.label=V(marriage)$name,vertex.color="red",
     vertex.label.color="black", vertex.frame.color=0,
     vertex.label.cex=1.5)

# The presence and absence of network links can be 
# used to measure network connectivity and "social 
# importance." A node's degree in an undirected network 
# is defined as its number of edges to other nodes. 

# Medicis are connected. There are connections (edges) 
# between the Medici and six other families, hence the 
# degree is 6 for the Medici. The Lambertschi family, 
# on the other hand, is connected to only one other
# family (the Guadagni); the Lambertschi family has 
# degree one.

data.frame(V(marriage)$name,degree(marriage))

# A deeper measure of network structure is obtained 
# through betweenness. Betweenness is a centrality
# measure of a node (or vertex) within a graph. 

# Nodes that occur on many shortest paths between
# other nodes have higher betweenness than those 
# that do not.

## calculate and plot the shortest paths
V(marriage)$color <- 8
E(marriage)$color <- 8
PtoA <- get.shortest.paths(marriage, from="Peruzzi", to="Acciaiuoli")
E(marriage, path=PtoA[[1]])$color <- "magenta"
V(marriage)[PtoA[[1]] ]$color <- "magenta"
GtoS <- get.shortest.paths(marriage, from="Ginori", to="Strozzi")
E(marriage, path=GtoS[[1]])$color <- "green"
V(marriage)[ GtoS[[1]] ]$color <- "green"
V(marriage)[ "Medici" ]$color <- "cyan"

set.seed(1)
plot(marriage,  layout=layout.fruchterman.reingold, 
     vertex.label=V(marriage)$name,
     vertex.label.color="black", 
     vertex.frame.color=0,
     vertex.label.cex=1.5)

data.frame(V(marriage)$name, betweenness(marriage))

# Betweenness versus Degree. The Medici have the highest 
# degree (largest number of edges), but only by a factor 
# of 3/2 over the Strozzi's. But the Medici's betweenness
# (47.5) is five times higher than that of the Strozzi 
# (9.33). 

# Betweenness measures total graph connectivity, rather 
# than counting the next door neighbors.

### Example 2: Connections in a Friendship Network

# The second example represents a simulation of an 
# in-school friendship network for a school community 
# in the rural western United States, with a student body
# that is largely Hispanic and Native American. The network
# object faux.mesa.high, included in the R library statnet,
# has 205 vertices (students, in this case) and 203
# undirected edges (mutual friendships). 

# The vertex (node) attributes are Grade, Sex, and Race.
# The Grade attribute has values 7 through 12, indicating 
# each student's grade in school. 

# The Race attribute is based on the answers to two 
# questions, one on Hispanic identity and one on race, 
# and takes six possible values: White, Black, Hispanic,
# Asian, Native American, and Other. 
# See Resnick et al. (1997).

# The R package statnet (Handcock et al., 2008) contains
# much useful software for statistical network analysis. 
# The package includes tools for the informative
# graphical display of networks (network visualization) 
# and programs for model estimation, model evaluation, 
# and model-based network simulation. There is a large
# and growing literature on statistical models for network
# data, and statnet implements recent advances on 
# exponential-family random graph models (in short, ergm;
# also an R package). We use statnet for the analysis
# of this example, but will not discuss the formal 
# statistical models in this brief introduction.

library(statnet)
data(faux.mesa.high)          ## load the network object
summary(faux.mesa.high)       ## summarize the data set

# There are 203 undirected connections among 
# the 205 nodes. The edge list summarizes the
# unordered pairs of nodes that are connected.

# The 205 ? 205 symmetric adjacency matrix has 
# zeros in the diagonal (as a node cannot be connected
# to itself), and many zeros and (2)(213) ones as 
# its off-diagonal elements. The network density
# is given by (2)(203)/[(205)(204)] = 203/[(205)(204)/2] = 0.0097.

# Only about 1% of all possible network connections 
# are realized.

lab=network.vertex.names(faux.mesa.high)=c(1:205)  
## assigns numbers to nodes
grd=faux.mesa.high%v%"Grade"
sx=faux.mesa.high%v%"Sex"
race=faux.mesa.high%v%"Race"	## we don't look at race in this example
vs=c(4,12)[match(sx,c("M","F"))]	
## used for graph later on; boys by square (4 sides); girls by 12-sided
col=c(6,5,3,7,4,2)		## used for graph later on
as.sociomatrix(faux.mesa.high)## gives adjacency matrix
faux.mesa.high[1,]
faux.mesa.high[5,]
faux.mesa.high[,3]
m=faux.mesa.high[,]    		## adjacency matrix
network.density(faux.mesa.high) 	
## density of network = NuEdges/[nodes*(nodes-1)/2]

# The degree of a certain node in an undirected network 
# is obtained by adding the number of connections that 
# exist between the node and all other nodes of the
# network. Statnet, in its calculation of degree, 
# counts an edge between nodes i and j twice as it 
# considers both the direction from and the direction 
# to the node. 

# For an undirected network we divide the degrees that
# are obtained by statnet by 2, to make the results
# consistent with our earlier definition and the 
# results from igraph.


# The same adjustment needs to be made for betweenness
# as statnet sums over all paths that move from one 
# node to the other and vice versa.

deg=degree(faux.mesa.high)/2 	
## degree of network nodes (number of connections)
## Statnet double-counts the connections in an undirected network
## Edge between nodes i and j in an undirected network is counted twice
## We divide by 2 in order to make the results consistent with our 
## discussion in the text and the output from igraph (in Example 1)
deg

betw=betweenness(faux.mesa.high)/2 		
## betweenness of network
## Statnet double-counts the betweenness in an undirected network
## We divide by 2 in order to make the results consistent with our 
## discussion in the text and the output from igraph 
betw

plot(deg)
plot(betw)
hist(deg,breaks=
       c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5))


# A couple of students (e.g., the first student on the 
# list, a female seventh grader, with 13 connections) 
# are well connected, but most students have relatively
# few connections.


# More than 25% of the students are "singletons" 
# (i.e., have no connections to other nodes), and 
# about 50% of the students have at most one connection.

plot(deg,betw)

boxplot(deg~grd)
boxplot(deg~sx)

# Females have more connections than males, 
# and eighth to tenth graders have fewer connections
# than entering seventh graders and students who are 
# close to graduation.

# We have already shown how to obtain the adjacency 
# matrix from a given statnet network object. Below we
# show how to export the edge list. We also show how
# to create a statnet network object from first 
# principles when given the edge list (or adjacency
# matrix).

attributes(faux.mesa.high)
vv=faux.mesa.high$mel
edge=matrix(nrow=203,ncol=2)
for (i in 1:203) {
  vvv=vv[[203+i]]
  edge[i,1]=vvv$inl
  edge[i,2]=vvv$outl
}
edge				
## edge contains the edge list
## in an undirected network, edge information is
## stored in the second half of faux.mesa.high$mel
faux1=network(edge,directed=FALSE,matrix.type="edgelist")
faux1
faux1[,]
deg=degree(faux1)/2
betw=betweenness(faux1)/2
plot(deg)
plot(betw)
plot(deg,betw)

## faux.mesa.high is already a network object
## below we illustrate how to create an undirected network
## from the adjacency matrix
## the adjacency matrix had been stored previously in m

faux2=network(m,directed=FALSE,matrix.type="adjacency")
faux2
faux2[,]
deg=degree(faux2)/2
betw=betweenness(faux2)/2
plot(deg)
plot(betw)
plot(deg,betw)

# Visual displays of the network, with and without
# information on node attributes, are shown as follows:

set.seed(654)          ## to get reproducible graphs
plot(faux.mesa.high)   ## generic graph without labels/covariates

set.seed(654)          ## to get reproducible graphs
plot(faux.mesa.high,label=lab) ## generic graph with labels

set.seed(654)  	     ## to get reproducible graphs
plot(faux.mesa.high,vertex.sides=vs,vertex.rot=45,vertex.cex=2,
     vertex.col=col[grd-6],edge.lwd=2,cex.main=3,displayisolates=FALSE)
legend("bottomright",legend=7:12,fill=col,cex=0.75)
## 45 rotates square
## isolates are not displayed

# The last graph is quite informative. 

# Females are represented by circles (12-sided
# objects) and males by squares; grade is indicated 
# by color. Students, especially those in lower grades,
# interact mostly with students from the same grade; 
# links with students from other grades are not that
# common. 

# We notice a few isolated small groups, usually 
# consisting of students from the same grade and 
# gender, with very few ties to other students. 

# A couple of students (e.g., student 1, a female seventh
# grader, with 13 connections) are well connected, but most
# students have relatively few connections. 

# More than 25% of the students are isolated 
# ("singletons"), and about 50% of the students have 
# at most one connection. 

# Note that "singletons" are not displayed on the last graph.

# Let us look at the connections more closely and 
# let us investigate whether connections are affected
# by the node characteristics, grade and gender. 

# While we can use statistical models to estimate
# the impact of grade and gender (and the R package 
# statnet can be used for this), the following 
# descriptive analysis will be sufficient for our purpose.


# Below, we use the adjacency matrix to calculate 
# network densities for various subgroups. 

# We measure the density of the

# . interaction among students from a certain grade 
#   with students from the same grade;
# . interaction among students from a certain grade 
#   with students from all grades;
# . interaction among students from a certain gender
#   group with students of the same gender;
# . interaction among students from a certain gender 
#   group with students of either gender.

# The network among females is more "dense" than is
# the network among males (0.0169 vs 0.0089). 

## density of interaction among students from the 
## same grade (ignoring gender)

m1=m[grd==7,grd==7]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==8,grd==8]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==9,grd==9]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==10,grd==10]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==11,grd==11]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==12,grd==12]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

# The density among students within the
# same grade is largest for twelfth graders 
# (0.0909).

## density of interaction among students from a given grade 
## with students from all grades (ignoring gender)

m1=m[grd==7,]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==8,]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==9,]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==10,]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==11,]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[grd==12,]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

# When considering connections with students 
# from all grades, the densities for seventh graders
# (0.0121) and for twelfth graders (0.0114)
# are about the same.


## density of interaction among students from the 
## same gender group (ignoring grade)

m1=m[sx=="F",sx=="F"]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[sx=="M",sx=="M"]
sum(m1)/(nrow(m1)*(ncol(m1)-1))


## density of interaction among students from a given gender 
## group with students of either gender (ignoring grade)

m1=m[sx=="F",]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

m1=m[sx=="M",]
sum(m1)/(nrow(m1)*(ncol(m1)-1))

# Ties between female students and students of 
# either gender are more prevalent than ties between
# male students and students of either gender
# (0.0116 vs 0.0079). 

# Also the network between seventh graders and same 
# gender students of all grades is about four times
# "denser" for females (0.0291) than it is for males 
# (0.0060). 

## density of interaction among students from the 
## same grade, for given gender

## female seventh graders
m1=m[sx=="F",sx=="F"]
grd1=grd[sx=="F"]
m2=m1[grd1==7,grd1==7]
sum(m2)/(nrow(m2)*(ncol(m2)-1))

## male seventh graders
m1=m[sx=="M",sx=="M"]
grd1=grd[sx=="M"]
m2=m1[grd1==7,grd1==7]
sum(m2)/(nrow(m2)*(ncol(m2)-1))

# Densities above are stratified according 
# to gender. For example, the network among seventh 
# graders of the same gender is four times "denser" for
# females (0.0823) than it is for males (0.0228). 

## female twelfth graders
m1=m[sx=="F",sx=="F"]
grd1=grd[sx=="F"]
m2=m1[grd1==12,grd1==12]
sum(m2)/(nrow(m2)*(ncol(m2)-1))

## male twelfth graders
m1=m[sx=="M",sx=="M"]
grd1=grd[sx=="M"]
m2=m1[grd1==12,grd1==12]
sum(m2)/(nrow(m2)*(ncol(m2)-1))

# The gender differences become considerably
# weaker for twelfth graders. The density of the network
# between twelfth graders and same gender students of
# all grades for females (0.0117) and males (0.0114)
# are similar.


## density of interaction among students from a given grade 
## with students from all grades, for given gender

## female seventh graders
m1=m[sx=="F",sx=="F"]
grd1=grd[sx=="F"]
m2=m1[grd1==7,]
sum(m2)/(nrow(m2)*(ncol(m2)-1))

## male seventh graders
m1=m[sx=="M",sx=="M"]
grd1=grd[sx=="M"]
m2=m1[grd1==7,] 
sum(m2)/(nrow(m2)*(ncol(m2)-1))

## female twelfth graders
m1=m[sx=="F",sx=="F"]
grd1=grd[sx=="F"]
m2=m1[grd1==12,]
sum(m2)/(nrow(m2)*(ncol(m2)-1))

## male twelfth graders
m1=m[sx=="M",sx=="M"]
grd1=grd[sx=="M"]
m2=m1[grd1==12,]
sum(m2)/(nrow(m2)*(ncol(m2)-1))

# The gender differences become considerably
# weaker for twelfth graders. The density of the network
# between twelfth graders and same gender students of
# all grades for females (0.0117) and males (0.0114)
# are similar.

# A further comment: There are many different 
# ways of plotting the connections of a network, 
# and plots of the very same adjacency matrix may 
# look quite different depending on how nodes are 
# located on the graph. So, even a simple visual
# display of a network structure makes some assumptions. 

# We illustrate this by visualizing the same network 
# as drawn according to three commonly adopted design
# principles (Fruchterman and Reingold, the design that 
# is usually recommended; Kamada-Kawai; and the circle 
# arrangement, which at least in this case, is not a
# very informative design criterion).

## Plotting options. Not that easy. Will make pictures look differently
## Principles of Fruchterman/Reingold: 
## 	Distribute vertices evenly in the frame 
## 	Minimize the number of edge crossings
## 	Make edge lengths uniform
## 	Reflect inherent symmetry
## 	Conform to the frame 

set.seed(654)  			## to get reproducible graphs
plot(faux.mesa.high,mode="fruchtermanreingold",label=lab,vertex.sides=vs,vertex.rot=45,vertex.cex=2.5,vertex.col=col[grd-6],edge.lwd=2,cex.main=3,displayisolates=FALSE)	
legend("bottomright",legend=7:12,fill=col,cex=0.75)

set.seed(654)  			## to get reproducible graphs
plot(faux.mesa.high,mode="kamadakawai",label=lab,vertex.sides=vs,vertex.rot=45,vertex.cex=2.5,vertex.col=col[grd-6],edge.lwd=2,cex.main=3,displayisolates=FALSE)	
legend("bottomright",legend=7:12,fill=col,cex=0.75)

set.seed(654)  			## to get reproducible graphs
plot(faux.mesa.high,mode="circle",label=lab,vertex.sides=vs,vertex.rot=45,vertex.cex=2.5,vertex.col=col[grd-6],edge.lwd=2,cex.main=3,displayisolates=FALSE)	
legend("bottomright",legend=7:12,fill=col,cex=0.75)

