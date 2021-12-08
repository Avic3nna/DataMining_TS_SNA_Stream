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

#indegree #using the directed net
degree_prestige = function(links, nodes){
  degree_prest_list = list()
  total_freq_to = table(links$to)
  total_nodes = dim(nodes)[1]
  max_degree = total_nodes - 1
  
  idx=1
  
  for(name in nodes$id){
    
    #check if it occurs in the lists
    to_na = (name %in% links$to)
    
    indegree = 0
    
    #if there is no arrow pointing to/from, 
    #it returns NA and breaks the calc
    if(to_na){
      indegree = unname(total_freq_to[name])
    }
    else{
      indegree = 0
    }
    
    degree_prest = indegree/max_degree
    
    degree_prest_list = append(degree_prest_list, degree_prest)
    names(degree_prest_list)[idx] = name
    
    idx = idx+1
  }

  return(degree_prest_list)
}



#outdegree #using the directed net
gregariousness = function(links, nodes){
  greg_list = list()
  fromtal_freq_from = table(links$from)
  fromtal_nodes = dim(nodes)[1]
  max_degree = fromtal_nodes - 1
  
  idx=1
  
  for(name in nodes$id){
    
    #check if it occurs in the lists
    from_na = (name %in% links$from)
    
    outdegree = 0
    
    #if there is no arrow pointing from/from, 
    #it returns NA and breaks the calc
    if(from_na){
      outdegree = unname(fromtal_freq_from[name])
    }
    else{
      outdegree = 0
    }
    
    greg = outdegree/max_degree
    
    greg_list = append(greg_list, greg)
    names(greg_list)[idx] = name
    
    idx = idx+1
  }
  
  return(greg_list)
}


####
# Closeness Centrality and Proximity Prestige
####

#https://www.r-bloggers.com/2020/10/finding-the-shortest-path-with-dijkstras-algorithm/
#Closeness Centrality and Proximity Prestige
path_length = function(graph,path) {
  # if path is NULL return infinite length
  if (is.null(path)) return(Inf)
  
  # get all consecutive nodes
  path_length_val = 0
  # print(length(path))
  # print(seq(along = 1:(length(path)-1)))
  if(length(path) > 1){
    # print(path)
    # print(path[1])
    
    for(node in seq(along = 1:(length(path)-1))){
      # print(path[node])
      # print(path[node+1])
      # print(graph[path[node], path[node+1]])
      path_length_val = path_length_val + graph[path[node], path[node+1]]
    }
  }

  return(path_length_val)
}


#dijkstra algorithm
shortest_path <- function(graph, start, end, path = c()) {
  # if there are no nodes linked from current node (= dead end) return NULL
  if (is.null(graph[[start]])) return(NULL)
  # add next node to path so far
  path <- c(path, start)
  
  # base case of recursion: if end is reached return path
  if (start == end) return(path)
  
  # initialize shortest path as NULL
  shortest <- NULL
  # loop through all nodes linked from the current node (given in start)
  node_list = graph[[start]]
  for (node in names(node_list[[1]])) {
    
    # proceed only if linked node is not already in path
    if (!(node %in% path)) {
      # recursively call function for finding shortest path with node as start and assign it to newpath
      newpath <- shortest_path(graph, node, end, path)
      # if newpath is shorter than shortest so far assign newpath to shortest
      if (path_length(graph, newpath) < path_length(graph, shortest))
        shortest <- newpath
    }
  }
  # return shortest path
  return(shortest)
}


#closeness centrality
cc = function(net, edgelist){
  AvDist = vector(length = length(V(net)$name))
  Dist = matrix(NA, nrow = length(V(net)$name), ncol = length(V(net)$name))
  max_nodes = length(table(edgelist))
  max_degree = max_nodes - 1
  
  i=1
  for(start in V(net)$name){
    # print('start')
    # print(start)
    j=1
    for(end in V(net)$name){
      # print('end')
      # print(end)
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


