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
    
    lcc_iter = number_of_links/(Ki*(Ki-1))
    print(lcc_iter)
    
    all_lcc = append(all_lcc, lcc_iter)
      
  }
  return(all_lcc)
}

# problem: $from has 16 unique in them, $to has 17
# how to solve, use the biggest of the two, or use to, or...? Read!
# also: if Ki = 1, it returns NAN, so just return 0
