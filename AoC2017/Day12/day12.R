library(tidyverse)
library(igraph)

input <- read_lines('AoC2017/Day12/input.txt')

# Part 1 ----
make_pipes = function(input) {
  
  verteces = str_extract_all(input, '\\d+') %>% unlist() %>% 
    unique()
  
  g = make_empty_graph(directed = F)
  
  g = add_vertices(g, nv = length(verteces),  attr = list(name=verteces))
  
  for (i in input) {
    pipes = str_extract_all(i,'\\d+', simplify = T)
    edges = c(cbind(pipes[1],pipes[-1])) %>% as.character()
    
    g = add_edges(g,edges = edges)
  }
  g
}
g = make_pipes(input)

length(subcomponent(g, "0", mode = "all"))

# Part 2 ----
count_components(g)
