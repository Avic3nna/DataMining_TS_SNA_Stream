### Exercise 2
#https://kateto.net/networks-r-igraph

### all functions


find_neighbour_nodes = function(edgelist, name){
  #indeces of rows containing this name, (index > dim(edgelist)[1]) - dim(edgelist)[1]
  row_indeces = which(edgelist==name)
  
  #wrap it around to get the indeces in the second column as well
  row_indeces[row_indeces > dim(edgelist)[1]] = row_indeces[row_indeces > dim(edgelist)[1]] - dim(edgelist)[1]
  
  #get a vector of all connections and filter the node we look at right now
  all_connections = as.vector(edgelist[row_indeces,])
  filter = all_connections == name
  all_connections = all_connections[!filter]
}


lcc = function(edgelist){
  ## local cluster coefficient
  # https://en.wikipedia.org/wiki/Clustering_coefficient
  
  # all the links it has
  # ___________________________
  # all the links it could have
  
  
  lcc_list = list()
  idx = 1
  for(node in sort(unique(as.vector(edgelist)))){
    lcc_iter = 0
    all_connections = find_neighbour_nodes(edgelist, node)
    Ki = length(all_connections)
    
    #find the clique
    num_of_links = 0
    for(neighbour in all_connections){
      find_clique = find_neighbour_nodes(edgelist, neighbour)
      found = find_clique %in% all_connections
      num_of_links = num_of_links + sum(found)
    }
    num_of_links = num_of_links
    
    if(Ki > 1){
      lcc_iter = num_of_links/(Ki*(Ki-1))
    }

    
    lcc_list = append(lcc_list, lcc_iter)
    names(lcc_list)[idx] = node
    idx = idx + 1
    
  }
  return(lcc_list)

}
  

degree_centrality = function(edgelist){
  degree_centr_list = list()
  max_nodes = length(table(edgelist))
  max_degree = max_nodes - 1
  idx = 1
  for(node in table(edgelist)){
    degree = unname(node)
    degree_centr = degree/max_degree

    degree_centr_list = append(degree_centr_list, degree_centr)
    #degree_list = c(degree_list, degree)
    names(degree_centr_list)[idx] = names(table(edgelist))[idx]
    idx = idx+1
  }
  
  return(degree_centr_list)
  
}


degree_prestige = function(net){
  #dir_net[] #[i,] = from, [,i] = to
  
  degree_prest_list = list()
  max_degree = length(V(net)) - 1
  
  for(i in 1:(length(V(net)))){
    indegree = sum(dir_net[,i] != 0)
    degree_prest = indegree/max_degree
    degree_prest_list = append(degree_prest_list, degree_prest)
    names(degree_prest_list)[i] = V(dir_net)[i]
  }
  return(degree_prest_list)
}



#outdegree #using the directed net
gregariousness = function(net){
  greg_list = list()
  
  degree_prest_list = list()
  max_degree = length(V(net)) - 1
  
  for(i in 1:(length(V(net)))){
    outdegree = sum(dir_net[i,] != 0)
    greg = outdegree/max_degree
    greg_list = append(greg_list, greg)
    names(greg_list)[i] = V(dir_net)[i]
  }
  return(greg_list)
}


####
# Closeness Centrality and Proximity Prestige
####

#https://www.r-bloggers.com/2020/10/finding-the-shortest-path-with-dijkstras-algorithm/
#Closeness Centrality
path_length = function(graph,path) {

  if (is.null(path)) return(Inf)
  
  path_length_val = 0
  
  if(length(path) > 1){
    
    for(node in seq(along = 1:(length(path)-1))){
      path_length_val = path_length_val + graph[path[node], path[node+1]]
    }
  }

  return(path_length_val)
}


#closeness centrality
cc = function(net, edgelist){
  AvDist = vector(length = length(V(net)$name))
  Dist = matrix(NA, nrow = length(V(net)$name), ncol = length(V(net)$name))
  max_nodes = length(table(edgelist))
  max_degree = max_nodes - 1
  
  i=1
  for(start in V(net)$name){
    j=1
    for(end in V(net)$name){

      sp = shortest_paths(net, start, end)
      sho_pa = names(sp$vpath[[1]])
      length_shortest_path = path_length(net,sho_pa)
      Dist[i,j] = length_shortest_path
      j = j+1
    }
    
    AvDist[i] = sum(Dist[i,])
    # print(Dist[i,])
    i = i+1
  }
  cc_list = 1/AvDist
  
  return(cc_list)
}

pp = function(net, nodes){
  # directed net!
  
 #need AvDist again, but now only from nodes who can reach this particular node
 #so perhaps the indegree is useful here, because those nodes can reach our node
  #then we calculate and sum the distances from those nodes to us,
  # and divide by the amount of nodes
  # calc influencefraction = number of influenced nodes/max degree
  # Pp = influencefrac/avdist
  
  num_of_nodes = length(nodes$id)
  
  AvDist = vector(length = num_of_nodes)
  prox_prest = vector(length = num_of_nodes)
  InfluenceFrac = vector(length = num_of_nodes)
  Dist = matrix(NA, nrow = num_of_nodes, ncol = num_of_nodes)
  max_degree = num_of_nodes - 1
  
  

  i=1
  for(name in nodes$id){
    #number of influenced nodes
    matches = (links$to == name)

    num_influencers = sum(matches)
    influencers = links$from[matches]
    
    
    j=1
    for(neighbour in influencers){
      node_path = c(neighbour, name) #from j to i (see book)
      length_path = path_length(net,node_path)
      Dist[j,i] = length_path
      j = j+1
    }

    i = i+1
  }
  
  for(i in seq(along = 1:num_of_nodes)){
      AvDist[i] = sum(na.omit(Dist[,i]))/num_influencers #take the whole column
      InfluenceFrac[i] = num_influencers/max_degree
      prox_prest[i] = InfluenceFrac[i]/AvDist[i]
  }
  
  return(prox_prest)
}


#betweenness centrality
#https://stackoverflow.com/questions/37861070/get-all-shortest-paths-help-file-on-nrgeo
#dir net
bc <- function(net) {
  len = dim(net[])[1]
  result = vecto=r(length = len)

  for(node in 1:len) {
    fraction = 0

    for(j in 1:len) {
      for(k in 1:len) {
        if (j != node && k != node){
          # All shortest paths from j to k
          q_jk = suppressWarnings(all_shortest_paths(net,j,k, mode = 'in')$res)
          # Find the amount of matches of the node in the shortest paths
          if(length(q_jk) > 0){
            q_jk_i = sum(sapply(q_jk,function(x){node %in% x}))
            # Increment fraction with the ratio
            fraction = fraction +  q_jk_i / length(q_jk)
          }

        }
      }
    }
    
    # Fraction divided by the max possible combinations
    result[node] = fraction / ((len)*(len-1))
  }
  return(result)
}



#common neighbours
#directed graph
cn = function(net){
  
  common_neighs = matrix(NA, nrow = length(V(net)), ncol = length(V(net)))
  
  for(i in 1:(length(V(net)))){
    neigh_i = neighbors(net, i)
    for(j in 1:(length(V(net)))){
      neigh_j = neighbors(net, j)
      common_neighs[i,j] = length(intersection(neigh_i, neigh_j))
    }
    
  }
  return(common_neighs)
}


#jaccard measure
jm = function(net){
  
  jaccard = matrix(NA, nrow = length(V(net)), ncol = length(V(net)))
  
  for(i in 1:(length(V(net)))){
    neigh_i = neighbors(net, i)
    for(j in 1:(length(V(net)))){
      neigh_j = neighbors(net, j)
      jaccard[i,j] = length(intersection(neigh_i, neigh_j))/length(union(neigh_i, neigh_j))
    }
    
  }
  return(jaccard)
}


