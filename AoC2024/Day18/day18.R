library(tidyverse)
input <- read_lines("AoC2024/Day18/input.txt")

parse = function(input) {
  str_extract_all(input,"\\d+",simplify = T) %>% 
    apply(., 2, \(x) as.integer(x)) %>% 
    apply(1,rev) %>% t()+1
}

bytes = parse(input)

# Part 1 ####
bfs = function(map, L=71) {
  
  start = cbind(1,1)
  end = cbind(L,L)
  visited = matrix(-Inf,L,L)
  visited[start]=0
  queue=collections::queue()
  queue$push(start)

  dirs = list(c(0,1),
              c(0,-1),
              c(1,0),
              c(-1,0))
  
  while(queue$size()>0) {
    pos = queue$pop()
    
    if (all(pos==end)) {return(visited)}
    
    nps = map(dirs, ~ pos+.x)
    
    for (np in nps) {
      if (any(np<=0) | any(np>L)){next}
      if (visited[np]>0) {next}
      if (map[np]) {next}
      visited[np] = visited[pos]+1
      queue$push(np)
    }
    
  }
  
  return(visited)
    
}

count_steps = function(bytes, L=71) {
  map = matrix(F, nrow = L, ncol = L)
  map[bytes] <- T
  path = bfs(map,L)
  path[L,L]
}

count_steps(bytes[1:1024,])

# Part 2 ####
run_until_close = function(bytes, L=71) {
  
  min_bites = 1025
  max_bytes = nrow(bytes)
  
  while(max_bytes-min_bites != 1) {
    try = min_bites+((max_bytes-min_bites)%/%2)
    test = count_steps(bytes[1:try,])
    if (test>0) {min_bites=try}else{max_bytes=try}
  }
  
  rev(bytes[max_bytes,]-1) %>% str_c(collapse = ",")
  
}

run_until_close(bytes)

