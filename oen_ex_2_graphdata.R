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

######### BEGIN LOAD DATA
#load nodes and links
nodes = read.csv("./Data/netscix2016/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links = read.csv("./Data/netscix2016/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
######### END LOAD DATA

######### BEGIN PRE-PROCESSING
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

#get all the edges (node-node) of the undirected graph
edgelist = as_edgelist(undir_net, names=T)

plot(dir_net)
######### END PRE-PROCESSING


######### BEGIN IMPLEMENTATION
### local clustering coefficient
lcc_list = lcc(edgelist)

#third party check
transitivity(undir_net, type = 'local') #lcc confirmation
as.vector(lcc_list) == transitivity(undir_net, type = 'local')


### degree centrality
dc = degree_centrality(edgelist)


#third party check
igraph:: degree(
  undir_net,
  v = V(undir_net),
  mode ="total",
  loops = TRUE,
  normalized = FALSE
)/16 #dc confirmation, max degree = 16

dc == igraph::degree(
  undir_net,
  v = V(undir_net),
  mode ="total",
  loops = TRUE,
  normalized = FALSE
)/16


### degree prestige
dp = degree_prestige(dir_net)
igraph::degree(dir_net, mode="in")/16 # dp validation. https://rpubs.com/pjmurphy/313180

#third party check
dp == igraph::degree(dir_net, mode="in")/16


### gregariousness
greg = gregariousness(dir_net)

#third party check
igraph::degree(dir_net, mode="out")/16 #greg validation.https://rpubs.com/pjmurphy/313180
greg == degree(dir_net, mode="out")/16



### closeness centrality
closeness_centrality = cc(undir_net, edgelist)

cl = igraph::closeness(
  undir_net,
  mode = "total",
  weights = NULL,
  normalized = FALSE
)

#third party check
unname(cl) == closeness_centrality



### proximity prestige
prox_prest = pp(dir_net, nodes)
prox_prest

### betweenness centrality
bc(dir_net)

#third party check
igraph::betweenness(
  dir_net,
  v = V(dir_net),
  directed = TRUE,
  weights = NULL,
  nobigint = FALSE,
  normalized = FALSE)/(16*17)

### common neighbor based measure
cn(undir_net)


### jaccard measure
jmm = jm(undir_net)

#third party check
jm3 = igraph::similarity(
  undir_net,
  vids = V(undir_net),
  mode = "total",
  loops = FALSE,
  method = "jaccard"
)

jmm == jm3
######### END IMPLEMENTATION

