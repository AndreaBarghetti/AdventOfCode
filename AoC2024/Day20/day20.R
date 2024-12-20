library(tidyverse)
input <- read_lines("AoC2024/Day20/input.txt")

map = str_split(input,"",simplify = T)

# Part 1 ####
bfs = function(map) {
  
  moves = list(c(0,1),
               c(0,-1),
               c(1,0),
               c(-1,0))
  
  start = which(map=='S', arr.ind = T)
  end = which(map=='E', arr.ind = T)
  queue=collections::queue()
  queue$push(start)
  visited = matrix(-Inf, nrow(map), ncol(map))
  visited[start]=0
  
  while(queue$size()>0) {
    pos = queue$pop()
    # if (identical(pos,end)) {}
    for (m in moves) {
      np = pos+m
      if (any(np<=0) | any(np > dim(map))) {next}
      if (map[np]=="#") {next}
      if (visited[np]>=0) {next}
      visited[np] <- visited[pos]+1
      queue$push(np)
    }
    
  }
  return(visited)
}

explored_map = bfs(map)

score_cheats = function(explored_map) {
  ep = explored_map
  
  cheats = list(c(0,2),
                c(0,-2),
                c(2,0),
                c(-2,0),
                c(1,1),
                c(1,-1),
                c(-1,1),
                c(-1,-1))
  tests = list()
  # r=2;c=3
  for (r in 1:nrow(ep)) {
    for (c in 1:ncol(ep)) {
      p = cbind(r,c)
      if (map[p]=="#"){next}
      for (t in cheats) {
        np = p+t
        if (any(np<=0) | any(np > dim(map))) {next}
        if (map[np]=="#") {next}
        score = ep[np] - ep[p] - 2 # -2 steps in cheat
        if ( score>0) {
          tests = c(tests, list(c(p,np,score)))
        }
      }
    }
  }
  return(tests)
}

cheats = score_cheats(explored_map)

sum(map_dbl(cheats, ~.x[5])>=100)

#1417

# Part 2 ####
