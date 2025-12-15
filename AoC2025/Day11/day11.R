library(tidyverse)
library(igraph)
# library(Matrix)

input <- read_lines("AoC2025/Day11/input.txt")

parse_io = function(input) {
  
  str_extract_all(input,'\\w+') %>% 
    map(~{list(
      i = .x[1],
      o = .x[-1]
    )})
  
}

ios = parse_io(input)

# Part 1 ####
edges = map(ios, ~{
  i = .x[[1]]
  o = .x[[2]]
  map(o, ~{
    c(i,.x)
  })
}) %>% unlist()

g = make_graph(edges = edges, directed = T)

all_simple_paths(g, from = 'you', to = 'out',mode = 'out') %>% 
  length()

# Part 2 ####

# I don't understand how this works Has (but it does).
count_n_paths = function(graph, from, to) {
  
  A <- as_adjacency_matrix(g, type = "both", sparse = TRUE)
  n <- vcount(g) 
  
  M <- A
  Current_Power_A <- A
  
  for (k in 2:(n - 1)) {
    Current_Power_A <- Current_Power_A %*% A
    
    M <- M + Current_Power_A
    
    # Optimization: break early if the graph is very sparse and no more paths exist
    if (all(Current_Power_A == 0)) break
  }
  
  a_idx <- match(from, V(g)$name)
  z_idx <- match(to, V(g)$name)
  
  res <- M[a_idx, z_idx]
  
  res
} 
count_n_paths(g, from = 'svr', to = 'fft')

svr_fft = count_n_paths(g, from = 'svr', to = 'fft')
fft_dac = count_n_paths(g, from = 'fft', to = 'dac')
fft_out = count_n_paths(g, from = 'dac', to = 'out')

format(
  prod(svr_fft, fft_dac, fft_out),
  scientific = F
)
