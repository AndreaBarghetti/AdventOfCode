library(tidyverse)
input <- read_lines("AoC2023/Day21/input.txt")

garden = str_split(input,"", simplify = T)

# Part 1 ####
step_garden <- function(garden, steps) {
  
  moves = list(U=c(-1,0),
               D=c(1,0),
               L=c(0,-1),
               R=c(0,1))

  pos = which(garden=="S", arr.ind = T)
  garden[pos]<-"."
  garden = ifelse(garden==".",1,0)
 
  visited = matrix(0,nrow = nrow(garden), ncol = ncol(garden))
  visited[pos] <- 1

  for (i in 1:steps) {
    occupied = which(visited==1, arr.ind = T)
    
    next_pos <- map(moves, function(move){
      t(t(occupied)+move)
    }) %>% purrr::reduce(rbind) %>% 
      unique()
    skiprows = next_pos[,1]==0|next_pos[,2]==0|next_pos[,1]>nrow(garden)|next_pos[,2]>ncol(garden)
    next_pos <- matrix(next_pos[!skiprows,], ncol=2)
    visited[next_pos] <- 1
    visited[garden==0]<-0
    visited[occupied] <-0
  }
  
  visited
}

visited = step_garden(garden, steps = 64)
sum(visited)

garden_bfs <- function(garden, steps) {
  moves = list(U=c(-1,0),
               D=c(1,0),
               L=c(0,-1),
               R=c(0,1))
  
  pos = which(garden=="S", arr.ind = T)

  visited = matrix(F,nrow = nrow(garden), ncol = ncol(garden))
  visited[pos] <- T
  dist = matrix(Inf, nrow = nrow(garden), ncol = ncol(garden))
  dist[pos] <- 0
  
  queue = collections::queue()
  queue$push(pos)
  
  while(queue$size() > 0) {
    pos = queue$pop()
    
    for (move in moves) {
      next_pos = pos + move
      if(any(next_pos==0) |next_pos[1]>nrow(garden)|next_pos[2]>ncol(garden)){next}
      if(garden[next_pos]=="#"){next}
      d = dist[pos]+1
      if(d > steps) {next}
      if(visited[next_pos]) {next}
      visited[next_pos]<-T
      dist[next_pos]<-d
      queue$push(next_pos)
    }
  }
  visited
}

reached = garden_bfs(garden, 64)
sum(reached[garden!="#" & (seq_along(garden)%%2==1)])

# bfs is way faster
system.time({
  sum(step_garden(garden, steps = 64))}
  )
system.time({
  reached = garden_bfs(garden, 64)
  sum(reached[garden!="#" & (seq_along(garden)%%2==1)])
})

# Part 2 ####
garden_bfs <- function(garden, steps, start, phase) {
  moves = list(U=c(-1,0),
               D=c(1,0),
               L=c(0,-1),
               R=c(0,1))

  pos = start

  visited = matrix(F,nrow = nrow(garden), ncol = ncol(garden))
  visited[pos] <- T
  dist = matrix(Inf, nrow = nrow(garden), ncol = ncol(garden))
  dist[pos] <- 0

  queue = collections::queue()
  queue$push(pos)

  while(queue$size() > 0) {
    pos = queue$pop()

    for (move in moves) {
      next_pos = pos + move
      if(any(next_pos==0) |next_pos[1]>nrow(garden)|next_pos[2]>ncol(garden)){next}
      if(garden[next_pos]=="#"){next}
      d = dist[pos]+1
      if(d > steps) {next}
      if(visited[next_pos]) {next}
      visited[next_pos]<-T
      dist[next_pos]<-d
      queue$push(next_pos)
    }
  }
  sum(visited[garden!="#" & (seq_along(garden)%%2==phase)])
}
pos = which(garden=="S", arr.ind = T)
#test
garden_bfs(garden, steps = 64, start = pos, phase = 1)

mid = which(garden=='S', arr.ind = T)[1]
last = ncol(garden)

steps= 26501365

# how many gardens is the big square made of
side = ((steps*2+1)%/%last)


type1_tiles = garden_bfs(garden, steps, pos, phase=0)
type2_tiles = garden_bfs(garden, steps+1, pos, phase=1)

# full_square = (n_gardens%/%2)*even_tiles + (n_gardens%/%2 + n_gardens%%2)*odd_tiles
# format(full_square-((full_square - steps*4 - (steps*2+1))/2), scientific = F)
count_full_gardens = function(n){
  if(n==0){return(c(t1=1,t2=0))}
  t1=as.numeric(0)
  t2=as.numeric(0)
  i=0
  for(i in 1:n){
    ifelse(i%%2==0,{t1=t1+i*4},{t2=t2+i*4})
  }
  t1=t1+1
  c(t1=t1,t2=t2)
}
count_full_gardens(4)
# should be approx this:
n_gardens=side^2
format(((n_gardens%/%2)*type2_tiles + (n_gardens%/%2+1)*type1_tiles)/2, scientific = F)


L <- (side-2)%/%2
full_gardens = count_full_gardens(L)

total = full_gardens['t1']*type1_tiles + full_gardens['t2'] * type2_tiles

#add gardens on top, bottom, left right corners
total = total + 
  garden_bfs(garden, steps = last-1, start = cbind(last,mid), phase=0) +
  garden_bfs(garden, steps = last-1, start = cbind(1,mid), phase=0) +
  garden_bfs(garden, steps = last-1, start = cbind(mid,1), phase=0) + 
  garden_bfs(garden, steps = last-1, start = cbind(mid,last), phase=0)
# add partial gardens reached from corner

total = total +
  garden_bfs(garden, steps = mid-2, start = cbind(last,last), phase=1)*(L+1)+
  garden_bfs(garden, steps = mid-2, start = cbind(1,last), phase=1)*(L+1)+
  garden_bfs(garden, steps = mid-2, start = cbind(last,1), phase=1)*(L+1)+
  garden_bfs(garden, steps = mid-2, start = cbind(1,1), phase=1)*(L+1)

total = total +
  garden_bfs(garden, steps = last+mid-2, start = cbind(last,last), phase=0)*(L)+
  garden_bfs(garden, steps = last+mid-2, start = cbind(1,last), phase=0)*(L)+
  garden_bfs(garden, steps = last+mid-2, start = cbind(last,1), phase=0)*(L)+
  garden_bfs(garden, steps = last+mid-2, start = cbind(1,1), phase=0)*(L)

format(total, scientific = F)
