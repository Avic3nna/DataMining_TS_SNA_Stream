### Exercise 2
#https://kateto.net/networks-r-igraph

### all functions


## local cluster coefficient
# https://en.wikipedia.org/wiki/Clustering_coefficient

# all the links it has
# ___________________________
# all the links it could have

lcc = function(links, nodes){
  #number of nodes (vertices) in the neighbourhood
  all_lcc = list()
  
  place_in_list = 0
  idx = 1
  for(i in table(links$from)){
    lcc_iter = 0
    Ki = unname(i)

    number_of_links = 0
    neighbour_name = links$to[place_in_list:(place_in_list+Ki)]
    for(from in neighbour_name){
      match_indeces = which(links$from == from)
      for(to in neighbour_name){
        if(from != to){
          corresponding_names = links$to[match_indeces]

          if(to %in% corresponding_names){
            number_of_links = number_of_links+1
          }
        }
      }
      
    }
    
    if(Ki > 1){
      lcc_iter = number_of_links/(Ki*(Ki-1))
    }
    else{
      lcc_iter = 0
    }
    
    #print(lcc_iter)
    
    all_lcc = append(all_lcc, lcc_iter)
    names(all_lcc)[idx] = names(table(links$from)[idx])
    
    idx = idx+1
  }
  return(all_lcc)
}

# problem: $from has 16 unique in them, $to has 17
# meaning 1 node doesn't have any arrows pointing away
# can this one then form a cluster..?



degree_centrality = function(links, nodes){
  degree_centr_list = list()
  degree_list = list()
  total_freq_to = table(links$to)
  total_freq_from = table(links$from)
  total_nodes = dim(nodes)[1]
  max_degree = total_nodes - 1
  
  idx=1
  
  for(name in nodes$id){

    #check if it occurs in the lists
    to_na = (name %in% links$to)
    from_na = (name %in% links$from)
    
    degree = 0

    #if there is no arrow pointing to/from, 
    #it returns NA and breaks the calc
    if(!from_na){
      degree = unname(total_freq_to[name])
    }
    else if(!to_na){
      degree = unname(total_freq_from[name])
    }
    else{
      degree = unname(total_freq_to[name]) + unname(total_freq_from[name])
    }
    
    degree_centr = degree/max_degree

    degree_centr_list = append(degree_centr_list, degree_centr)
    degree_list = c(degree_list, degree)
    names(degree_centr_list)[idx] = name
    
    idx = idx+1
  }
  # print('degree per node:')
  # for(item in degree_list){
  #   cat(item)
  #   cat(' ')
  # }
  return(degree_centr_list)
}


#indegree
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



#outdegree
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


#https://www.r-bloggers.com/2020/10/finding-the-shortest-path-with-dijkstras-algorithm/
#Closeness Centrality and Proximity Prestige
path_length = function(path) {
  # if path is NULL return infinite length
  if (is.null(path)) return(Inf)
  
  # get all consecutive nodes
  pairs = cbind(values = path[-length(path)], ind = path[-1])
  # join with G and sum over weights
  return(sum(merge(pairs, G)[ , "weights"]))
}

find_shortest_path <- function(graph, start, end, path = c()) {
  # if there are no nodes linked from current node (= dead end) return NULL
  if (is.null(graph[[start]])) return(NULL)
  # add next node to path so far
  path <- c(path, start)
  
  # base case of recursion: if end is reached return path
  if (start == end) return(path)
  
  # initialize shortest path as NULL
  shortest <- NULL
  # loop through all nodes linked from the current node (given in start)
  for (node in graph[[start]]) {
    # proceed only if linked node is not already in path
    if (!(node %in% path)) {
      # recursively call function for finding shortest path with node as start and assign it to newpath
      newpath <- find_shortest_path(graph, node, end, path)
      # if newpath is shorter than shortest so far assign newpath to shortest
      if (path_length(newpath) < path_length(shortest))
        shortest <- newpath
    }
  }
  # return shortest path
  return(shortest)
}



