### Exercise 2
#https://kateto.net/networks-r-igraph

rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi",
                  "igraph")

for(package in packages_used){
  if(package %in% rownames(installed.packages()) == FALSE) {
    install.packages(package)
  }
}

setwd_current_path = function(){
  library(rstudioapi)
  current_path = getActiveDocumentContext()$path
  setwd(dirname(current_path)) #get this current folder
  print(getwd())
}
setwd_current_path()

library(igraph)

source('./oen_graphfunctions.R')


#load nodes and links
nodes = read.csv("./Data/netscix2016/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links = read.csv("./Data/netscix2016/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

links = aggregate(links[,3], links[,-3], sum)
links = links[order(links$from, links$to),]
colnames(links)[4] = "weight"
rownames(links) = NULL



#plot the graph
dir_net = graph_from_data_frame(d=links, vertices=nodes, directed=T)
undir_net = graph_from_data_frame(d=links, vertices=nodes, directed=F)

#remove duplicates
undir_net <- simplify(undir_net, remove.multiple = T, remove.loops = T)
dir_net <- simplify(dir_net, remove.multiple = T, remove.loops = T)


x11()
plot(dir_net)


#get all the edges (node-node) of the undirected graph
edgelist = as_edgelist(undir_net, names=T)

#get adjacency matrix
adjacency_matrix = as_adjacency_matrix(
  dir_net,
  type = c("both", "upper", "lower"),
  attr = NULL,
  edges = FALSE,
  names = TRUE,
  sparse = igraph_opt("sparsematrices")
)

lcc_list = lcc(edgelist)
transitivity(undir_net, type = 'local') #lcc confirmation
#check
as.vector(lcc_list) == transitivity(undir_net, type = 'local')


dc = degree_centrality(edgelist)

degree(
  undir_net,
  v = V(undir_net),
  mode ="total",
  loops = TRUE,
  normalized = FALSE
)/16 #dc confirmation, max degree = 16

#check
dc == degree(
  undir_net,
  v = V(undir_net),
  mode ="total",
  loops = TRUE,
  normalized = FALSE
)/16


#using the directed net
dp = degree_prestige(dir_net)
degree(dir_net, mode="in")/16 # dp validation. https://rpubs.com/pjmurphy/313180

#check
dp == degree(dir_net, mode="in")/16

greg = gregariousness(dir_net)
degree(dir_net, mode="out")/16 #greg validation.https://rpubs.com/pjmurphy/313180

#check
greg == degree(dir_net, mode="out")/16

closeness_centrality = cc(undir_net, edgelist)

cl = closeness(
  undir_net,
  #vids = V(undir_net),
  mode = "total",
  weights = NULL,
  normalized = FALSE
)

#check
unname(cl) == closeness_centrality


prox_prest = pp(dir_net, nodes)
prox_prest



#### under construction
bc(dir_net)

igraph::betweenness(
  dir_net,
  v = V(dir_net),
  directed = TRUE,
  weights = NULL,
  nobigint = FALSE,
  normalized = FALSE)

####


cn(undir_net)




#### 
jm3 = similarity(
  undir_net,
  vids = V(undir_net),
  mode = "total",
  loops = FALSE,
  method = "jaccard"
)

jmm = jm(undir_net)

#check
jmm == jm3
