library(tidyverse)
input <- read_lines('AoC2017/Day7/input.txt')

parse_tower = function(line) {
  
  name = str_extract(line,'\\w+')
  weight = str_extract(line,'\\d+') %>% as.integer()
  
  if (str_detect(line, "->")) {
    
    sub_towers = line %>% str_extract_all('[a-z]+', simplify = T) %>% setdiff(name)
    
  } else { sub_towers = vector(mode = 'character')}
  
  return(list(
    name = name,
    weight = weight,
    sub_towers = sub_towers
  ))
  
}

towers = map(input, parse_tower)
# names(towers) <-  map_chr(towers,'name')

# Part 1 ----
library(igraph)

build_graph = function(towers) {
  g <- make_empty_graph(directed = TRUE)
  g = add_vertices(g, nv = length(towers), attr = list(name = map_chr(towers, 'name'),
                                                      weight = map_int(towers, 'weight')))
  for (t in towers) {
    if (length(t$sub_towers) > 0) {
      g = add_edges(g, edges = as.character(rbind(t$name, t$sub_towers))) 
    }
  }
  g
}

g = build_graph(towers) 

names(which(degree(g, mode = "in")==0))

# Part 2 ----
get_weight = function(g,v) {
  sum(subcomponent(g, v, mode = "out")$weight)
}

get_weights = function(g, v) {
  
  map_dbl(neighbors(g,v, mode = 'out'), function(v) {
    get_weight(g,v)
  })
  
}

find_unbalanced = function(g) {
  for ( v in V(g)) {
    
    weights = get_weights(g, v)
    
    if (length(unique(weights))>1) {
      unbalanced = names(which.max(weights))
      return(find_unbalanced(induced_subgraph(g, vids = subcomponent(g, unbalanced, mode = "out"))))
    }
  }
  return(g)
}

unbalanced_tower = find_unbalanced(g)

unbalanced_disc = names(which(degree(unbalanced_tower, mode = "in")==0))

excess = diff(range(neighbors(g, unbalanced_disc, 'in') %>% 
  get_weights(g,.)))

V(g)[unbalanced_disc]$weight - excess
