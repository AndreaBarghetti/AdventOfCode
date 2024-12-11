library(tidyverse)
input <- read_lines("AoC2024/Day10/input.txt")

map = str_split(input, "", simplify = T) %>% 
  apply(2,as.integer)

trailheads = which(map==0,arr.ind = T)

moves = list(U=c(-1,0),
             D=c(1,0),
             L=c(0,-1),
             R=c(0,1))

# Part 1 ####

find_summits = function(th, map, skip_visited=T) {
  
  th=rbind(th)
  visited = matrix(F, nrow = nrow(map), ncol = ncol(map))
  queue = collections::queue()
  queue$push(list(p=th,h=0))
  
  count = 0
  
  while(queue$size()>0) {
    pos = queue$pop()
    # if visited
   
    if (skip_visited && visited[pos$p]) {next} else{visited[pos$p] <- T}

    # if end found
    if (pos$h == 9) {count=count+1; next}
    # otherwise try move
    for (m in moves) {
      
      np = rbind(pos$p+m)
      # out of bound
      if (any(np<=0)|any(np>dim(map))){next}
      # step up
      h = map[np]
      if (h-1!=pos$h) {next}
      # add to queue
      queue$push(list(p=np,h=h))
    }
  }
  
  count
  
}

summits = apply(trailheads,1,function(th){find_summits(th,map)})

sum(summits)

# Part 2 ####
ratings = apply(trailheads,1,function(th){find_summits(th,map, skip_visited = F)})
sum(ratings)
