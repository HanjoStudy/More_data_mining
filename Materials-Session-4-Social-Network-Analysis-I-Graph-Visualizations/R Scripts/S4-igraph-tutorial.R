#######################################
#####      igraph Tutorial        #####
#######################################

### Introduction to igraph

# igraph is "free software package for creating 
# and manipulating undirected and directed graphs"

# Need to install and load it
# if necessary:
# install.packages("igraph", dep=T)
# load it into your R session:
library("igraph")

### BASICS: Creating Graphs
# The objects are graphs (or networks). They 
# consist of a set of nodes and a set of edges. 

# As an example, if you type into the RStudio
# console the following command:
g <- graph(c(1,2, 1,3, 2,3, 3,5), n=5)
# and then plot it
plot(g)

# We are assigning to the variable g a graph
# that has nodes V = {1,2,3,4,5} (from n=5) and 
# has edges E = {(1,2), (1,3), (2,3), (3,5)}

# The commands V(g) and E(g) print the list of 
# nodes and edges of the graph g:

# vertex sequence:
V(g)

# edge sequence
E(g)

# You can add nodes and edges to an already
# existing graph by, for example, doing this:
g <- graph.empty() + vertices(letters[1:10], 
                              color="red")
# it creates an empty graph and then adds vertices.
# take a look:
plot(g)
# we add more:
g <- g + vertices(letters[11:20], 
                  color="blue")
# take a look:
plot(g)
# We add edges sampling from the existing
# edges in g:
g <- g + edges(sample(V(g), 30, replace=TRUE), 
               color="green")
# Take a look:
plot(g)

# resulting Vertex sequence:
V(g)

# resulting Edge sequence
E(g)

# These lines create a graph g with 20 nodes 
# and 15 random edges. Notice that nodes and edges
# can have attributes; in this example we assign
# different colors to nodes. 

### LOADING GRAPHS

# We have seen how to create graphs from scratch,
# but most often we will be loading them from a 
# file containing the graph in some sort of format.

# For example, we can access online graphs. The
# following command loads a Pajek graph from an online site:
karate <- read.graph("http://cneurocvs.rmki.kfki.hu/igraph/karate.net", format="pajek")
plot(karate)

### GRAPH GENERATORS

# igraph implements also many useful graph generators. 
# There are models such as: the Edos-Renyi model (ER), 
# the Barabasi-Albert model (BA), and the Watts-Strogratz
# model (WS). The following commands generate graphs
# using these models:

er_graph <- erdos.renyi.game(100, 2/100)
plot(er_graph)
ws_graph <- watts.strogatz.game(1, 100, 4, 0.05)
plot(ws_graph)
ba_graph <- barabasi.game(100)
plot(ba_graph)

# We can add attributes to nodes and edges 
# of the graphs. These are useful for selecting
# certain types of nodes, and for visualization.

# What these commands do is to generate a 
# random graph with 10 nodes, assigns random
# colors to the nodes, colors edges joining 
# red nodes in red, and edges joining black 
# nodes in black. All remaining edges are colored grey.

g <- erdos.renyi.game(10, 0.5)
plot(g)
# assigns random colors to nodes
V(g)$color <- sample( c("red", "black"), vcount(g), rep=TRUE)
plot(g)
# assign grey to the edges
E(g)$color <- "grey"
plot(g)
# identify red vertices as variable "red"
red <- V(g)[ color == "red" ]
# no changes:
plot(g)
# identify black vertices as variable "bl"
bl <- V(g)[ color == "black" ]
# no changes:
plot(g)
# make edges for red nodes red:
E(g)[ red %--% red ]$color <- "red"
# make edges for black nodes black:
E(g)[ bl %--% bl ]$color <- "black"
# remaining edges are still grey:
plot(g)

## ANOTHER EXAMPLE:
# What the following set of commands do is this:
# They assign random weights to a lattice graph
# and then colors the ones having weight over 
# 0.9 red, and the rest grey.

g <- graph.lattice( c(10,10) )
# graph that looks like a lattice:
plot(g)
# assign random weights:
E(g)$weight <- runif(ecount(g))
# color all edges grey:
E(g)$color <- "grey"
# plot g:
plot(g)
# color it red if weight > 0.9:
E(g)[ weight > 0.9 ]$color <- "red"
# show g:
plot(g)

### VISUALIZING GRAPHS

# A very important part in the analysis of 
# networks is being able to visualize them

# As an example, consider the following commands
# which render the three graphs depicted below:

er_graph <- erdos.renyi.game(100, 2/100)
plot(er_graph, vertex.label=NA, vertex.size=3)
ws_graph <- watts.strogatz.game(1, 100, 4, 0.05)
plot(ws_graph, layout=layout.circle, 
     vertex.label=NA, vertex.size=3)
ba_graph <- barabasi.game(100)
plot(ba_graph, vertex.label=NA, vertex.size=3)

# 'vertex.label' controls the label written in the 
# nodes, if set to NA then no text label is written. 

# You can access all the parameters and their
# possible values through the help system by typing
help(igraph.plotting)

# As another example, consider adding attributes
# to edges for a nicer visualization:

# create a lattice graph:
g <- graph.lattice( c(10,10) )
# show it:
plot(g)
E(g)$weight <- runif(ecount(g))
plot(g)
E(g)$color <- "grey"
plot(g)
E(g)[ weight > 0.9 ]$color <- "red"
plot(g)
# changes the layout; the intersections
# are now the nodes:
plot(g, vertex.size=2, vertex.label=NA, 
     layout=layout.kamada.kawai,
     edge.width=2+3*E(g)$weight)

### MEASURING GRAPHS

# There are many measures that help us 
# understand and characterize networks.

# Some are diameter (and average path length), 
# clustering coefficient (or transitivity), 
# and degree distribution. 

# igraph provides functions that compute these 
# measures for you. The functions are: diameter(),
# transitivity(), average.path.length(), degree(), and 
# degree.distribution(). 
?transitivity

?degree
?degree.distribution

# These examples illustrate usage of these functions:

# diameter() and average.path.length():
g <- graph.lattice(length=100, dim=1, nei=4)
plot(g)
average.path.length(g)
# [1] 8.79798
diameter(g)
# [1] 25
g <- rewire.edges( g, prob=0.05 )
plot(g)
average.path.length(g)
# [1] 3.132323
diameter(g)
# [1] 6

# for transitivity():
ws <- watts.strogatz.game(1, 100, 4, 0.05)
transitivity(ws)
# [1] 0.4784437
p_hat <- ecount(ws)/(vcount(ws)*(vcount(ws))/2)
p_hat
# [1] 0.08
er <- erdos.renyi.game(100, p_hat)
transitivity(er)
# [1] 0.08157424

# For degree() and degree.distribution():
g <- graph.ring(10)
plot(g)
degree(g)
# [1] 2 2 2 2 2 2 2 2 2 2
ba <- barabasi.game(10000, m=3)
p_hat <- ecount(ba)/ ((vcount(ba)-1)*vcount(ba)/2)
er <- erdos.renyi.game(10000, p_hat)
degree.distribution(er)
# [1] 0.0029 0.0120 0.0460 0.0876 0.1388 0.1562 0.1569
# [8] 0.1349 0.1082 0.0691 0.0447 0.0218 0.0118 0.0051
# [15] 0.0022 0.0011 0.0003 0.0001 0.0002 0.0001

# Erdos-Renyi:
hist(degree(er))

# Barabasi-Albert:
hist(degree(ba))

# Erdos-Renyi:
plot(degree.distribution(er))

# Barabasi-Albert:
plot(degree.distribution(ba))

#### A SECOND SET OF igraph DEMOS

# if you need to set the directory
setwd(choose.dir())

# Load the igraph package (install if needed)

require(igraph)
# or
library(igraph)

# Data format. The data is in 'edges' format 
# meaning that each row records a relationship 
# (edge) between two people (vertices).

# Additional attributes can be included. Here is an example:
#  Supervisor	Examiner	Grade	Specialization
#	         AA		    BD		  6	             X	
#	         BD		    CA		  8	             Y
#	         AA		    DE		  7	             Y
#	...		...		...	...
# In this anonymized example, we have data on co-supervision 
# with additional information about grades and specialization. 

# It is also possible to have the data in a matrix form 
# (see the igraph documentation for details)

# Load the data. The data needs to be loaded as a table first: 
bsk <- read.table("http://www.dimiter.eu/Data_files/edgesdata3.txt", sep='\t', dec=',', header=T)#specify the path, separator(tab, comma, ...), decimal point symbol, etc.

# if you need to save it to disk
write.csv(x=bsk, file="bsk.csv")

# Transform the table into the required graph format:
# the 'directed' attribute specifies 
# whether the edges are directed
bsk.network <- graph.data.frame(bsk, directed=F) 
# or equivalent irrespective of the position (1st vs 2nd column). 
# For directed graphs use 'directed=T'

# Inspect the data:

# prints the list of vertices (people)
V(bsk.network) 
# prints the list of edges (relationships)
E(bsk.network) 
# print the number of edges per vertex 
# (relationships per people)
degree(bsk.network) 

# First try. We can plot the graph right 
# away but the results will usually be 
# unsatisfactory:
plot(bsk.network)

# Subset the data. 
# If we want to exclude people who are in 
# the network only tangentially (participate
# in one or two relationships only) we can exclude
# them by subsetting the graph on the basis of the 'degree':

# identify those vertices part of less than three edges
bad.vs <- V(bsk.network)[degree(bsk.network)<3]
# exclude them from the graph
bsk.network <- delete.vertices(bsk.network, bad.vs) 

# Plot the data:
plot(bsk.network)

# Some details about the graph can 
# be specified in advance. For example we can separate
# some vertices (people) by color:

# useful for highlighting certain people. Works by
# matching the name attribute of the vertex to the
# one specified in the 'ifelse' expression. If name
# is 'CA'they are colored blue, otherwise red
V(bsk.network)$color <- ifelse(V(bsk.network)$name=='CA', 'blue', 'red') 
plot(bsk.network)

# We can also color the connecting edges differently
# depending on the 'grade': 
E(bsk.network)$color<-ifelse(E(bsk.network)$grade==9, "red", "grey")
plot(bsk.network)

# or depending on the different specialization ('spec'):
E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))
plot(bsk.network)

# Note: the example uses nested ifelse expressions
# which is in general a bad idea but does the job in this case.
# Additional attributes like size can be further 
# specified in an analogous manner, either in advance
# or when the plot function is called:

# here the size of the vertices is specified by the 
# degree of the vertex, so that people supervising 
# more have get proportionally bigger dots. 

# Getting the right scale gets some playing around 
# with the parameters of the scale function 
# (from the 'base' package):
V(bsk.network)$size<-degree(bsk.network)/10
plot(bsk.network)

# And the final plot:
# This specifies the size of the margins. 
# The default settings leave too much free space
# on all sides (if no axes are printed)
par(mai=c(0,0,1,0)) 

# here is the graph to be plotted:
plot(bsk.network,
     # the layout method
     # see the igraph documentation for details
     layout=layout.fruchterman.reingold,
     # specifies the title
     main='Organizational network example',
     # puts the name labels slightly off the dots
     vertex.label.dist=0.5,	
     # the color of the border of the dots 
     vertex.frame.color='blue',
     # the color of the name labels
     vertex.label.color='black',		
     # the font of the name labels
     vertex.label.font=2,
     # specifies the lables of the vertices. 
     # in this case the 'name' attribute is used
     vertex.label=V(bsk.network)$name,
     # specifies size of the font of the labels. 
     # can also be made to vary
     vertex.label.cex=1			
)

# Save and export the plot. 
# The plot can be copied as a metafile to the
# clipboard, or it can be saved as a pdf or png 
# (and other formats).
# For example, we can save it as a png:

# call the png writer and run the plot
png(filename="org_network.png", 
    height=800, width=600) 

# don't forget to close the device
dev.off()
