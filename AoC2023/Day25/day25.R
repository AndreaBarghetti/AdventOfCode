library(tidyverse)
input <- read_lines("AoC2023/Day25/input.txt")

parse_input <- function(input) {
  names = str_extract(input, "\\w+")
  to = str_remove(input, ".*:") %>% 
    str_extract_all("\\w+")
   setNames(to, names)
}

connections <- parse_input(input)

# Part 1 ####
library(igraph)

# make it numerical
make_num <- function(connections) {
  
  names <- names(connections)
  other_names <- unlist(connections) %>% setdiff(names) %>% unique()
  all_names <- c(names,other_names)
  name_num <- setNames(seq_along(all_names), all_names)
  connections <- unname(connections)
  num_connections <- map(connections, function(x) {
    unname(name_num[x])
  })
  
  num_connections
  
}

#make graph
connections_to_graph <- function(connections) {
  
  edges <- imap(make_num(connections), function(to, from) {
    map(to,~c(from, .x)) %>% 
      purrr::reduce(c)
  }) %>% 
    purrr::reduce(c)
  
  g <- make_graph(edges = edges, directed = FALSE)
  
  g
}

g = connections_to_graph(connections)

# I have to clue how this works
clusters <- igraph::cluster_fluid_communities(g,2)

clusters$membership %>% 
  table() %>% prod()
