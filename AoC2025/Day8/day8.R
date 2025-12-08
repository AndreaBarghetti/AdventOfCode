library(tidyverse)
library(igraph)

jboxes <- read_lines("AoC2025/Day8/test") %>% 
  str_extract_all('\\d+', simplify = T) %>% 
  apply(2,as.numeric)

# Part 1 ####
make_dist_matrix = function(jboxes) {
  D = dist(jboxes, diag = T)
  D=as.matrix(D)
  diag(D) <- NA
  D[upper.tri(D)] <- NA
  D
}

get_closest_pairs = function(jboxes, topN) {
  
  D = make_dist_matrix(jboxes)
  topNd = sort(D, decreasing = F) %>% head(topN)
  pairs = map(topNd, ~{
    which(D==.x,arr.ind = T) %>% as.integer()
  })
  pairs = do.call(rbind, pairs)
  pairs
}

connect_jboxes = function(jboxes, N) {
  pairs = get_closest_pairs(jboxes,N)
  g = graph_from_edgelist(pairs, directed = FALSE)
  g
}

g = connect_jboxes(jboxes, 1000)
components(g)$csize %>% sort(decreasing = T) %>% head(3) %>% 
  prod()

# Part 2 ####
connect_all_jboxes = function(jboxes) {
  
  g = make_empty_graph(n = nrow(jboxes), directed = FALSE) 
  
  D = make_dist_matrix(jboxes)
  topNd = sort(D, decreasing = F)
  
  for (d in topNd) {
    vertices = which(D==d, arr.ind = T) %>% as.integer()
    g <- add_edges(g, vertices)
    if (components(g)$no == 1) {break}
  }
  
  #return last 2 jboxes connected
  vertices
}

last2 = connect_all_jboxes(jboxes)

jboxes[last2,1] %>% prod() %>% 
  format(scientific = F)
