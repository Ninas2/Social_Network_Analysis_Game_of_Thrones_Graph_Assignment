setwd("C:\\Users\\ninas\\R\\RPackages")

.libPaths('C:\\Users\\ninas\\R\\RPackages')


#import of necessary libraries
library(igraph)

#Task 1 - Read the nodes' and the edges' csv files and create an undirected graph
#we read all the nodes from the github csv
github_link_asoiaf_nodes <- read.csv('https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-nodes.csv', header = TRUE)
head(github_link_asoiaf_nodes)

#we read all the edges between nodes from the github csv
github_link_asoiaf_edges <- read.csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv", header = TRUE)
#we keep only the source, the target and the weight of each edge
github_link_asoiaf_edges <- github_link_asoiaf_edges[,c(1,2,5)]
head(github_link_asoiaf_edges)

#create a graph from the nodes and the edges
asoiaf_graph <- graph_from_data_frame(github_link_asoiaf_edges, directed=FALSE, vertices=github_link_asoiaf_nodes)
#assign appropriate weights to each edge
E(asoiaf_graph)$weight <- github_link_asoiaf_edges[,3]

#check if the edges are weighted
is_weighted(asoiaf_graph)

#print the graph
print(asoiaf_graph, e=TRUE, v=TRUE)



#Task 2 - Identify the graph's properties
#Number of vertices - 796
vcount(asoiaf_graph)

#Number of edges - 2823
ecount(asoiaf_graph)

#Diameter of the graph - 53
diameter(asoiaf_graph)

#Number of triangles - 16965
sum(count_triangles(asoiaf_graph))/3

#The top-10 characters of the network as far as their degree is concerned
all_degrees <- igraph::degree(asoiaf_graph)
top_10_characters <- sort.int(all_degrees,decreasing=TRUE,index.return=FALSE)[1:10]
print(rownames(as.data.frame(top_10_characters)))

#The top-10 characters of the network as far as their weighted degree is concerned
weighted_degrees <- graph.strength(asoiaf_graph)
top_10_char_w <- sort.int(weighted_degrees,decreasing=TRUE,index.return=FALSE)[1:10]
print(rownames(as.data.frame(top_10_char_w)))


#Task 3 - Plot and customize the graph
#plot the entire graph as is
plot(asoiaf_graph, e=TRUE, v=TRUE, vertex.label = NA, edge.arrow.width = .5, vertex.size=7
     ,vertex.color='grey', edge.color='blue')

#create the same plot but for a graph that disregards all characters with less than 10 connections
#identify the characters with low degree (<10 connections)
char_with_low_degree <- github_link_asoiaf_nodes[which(all_degrees < 10),]

#create a new graph that only contains high degree characters
high_degree_graph <- delete.vertices(asoiaf_graph, char_with_low_degree$Id)

#plot the new graph
plot(high_degree_graph, e=TRUE, v=TRUE, vertex.label = NA, edge.arrow.width = .5, vertex.size=8
     ,vertex.color='grey', edge.color='blue')

#we will calculate the density of both graphs - the full one and the high degree one -.
#density for full graph - 0.008921968
paste("the density for the full graph is",mean(degree(asoiaf_graph))/(vcount(asoiaf_graph)-1))

#density for high degree graph - 0.117003
paste("the density for the high-degree graph is",mean(degree(high_degree_graph))/(vcount(high_degree_graph)-1))


#Task 4 - identify top 15 characters based on betweenness and closeness
#betweenness of each character
all_char_between <- betweenness(asoiaf_graph, directed = FALSE)
#we keep the top 15 characters based on their betweenness
top_15_bet <- sort.int(all_char_between,decreasing=TRUE,index.return=FALSE)[1:15]
print(rownames(as.data.frame(top_15_bet)))


#closeness of each character
all_char_closeness <- closeness(asoiaf_graph, mode = 'all')
top_15_close <- sort(all_char_closeness,decreasing=TRUE,index.return=FALSE)[1:15]
print(rownames(as.data.frame(top_15_close)))

#we will find the position of Jon Snow from the top in terms of closeness
#we will sort all characters based on their closeness
all_close_sorted <- sort(all_char_closeness,decreasing=TRUE,index.return=FALSE)
#we will get the position of Jon Snow - 10
Jon_close <- which(rownames(as.data.frame(all_close_sorted)) == 'Jon-Snow')
paste('Jon Snow is in the', Jon_close, 'position in terms of closeness')

#we will find the position of Jon Snow from the top in terms of betweenness
#we will sort all characters based on their betweenness
all_bet_sorted <- sort(all_char_between,decreasing=TRUE,index.return=FALSE)
#we will get the position of Jon Snow - 1
Jon_bet <- which(rownames(as.data.frame(all_bet_sorted)) == 'Jon-Snow')
paste('Jon Snow is in the', Jon_bet, 'position in terms of betweenness')

#Task 5 - conduct pagerank method on the network
all_pageranks <- page_rank(asoiaf_graph, directed = FALSE)

#plot the graph and give each character size equivalent to his/her pagerank
plot(asoiaf_graph, e=TRUE, v=TRUE, vertex.label = NA, edge.arrow.width = .5, vertex.size=all_pageranks$vector*200
     ,vertex.color='grey', edge.color='blue')

#sort all the characters based on their pagerank and keep the first 5
pageranks_sorted_top_5 <- sort(all_pageranks$vector,decreasing=TRUE,index.return=FALSE)[1:5]

print(rownames(as.data.frame(pageranks_sorted_top_5)))

