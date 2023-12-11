library(tidyverse)
library(AoCUtils)

input <- read_lines("AoC2023/Day10/input.txt")

pipes <- str_split(input, "", simplify = T)

# Part 1 ####
pipe_type = list(
  "|" = list(c(-1,0),c(1,0)),
  "-" = list(c(0,-1),c(0,1)),
  "L" = list(c(-1,0),c(0,1)),
  "J" = list(c(-1,0),c(0,-1)),
  "7" = list(c(1,0),c(0,-1)),
  "F" = list(c(1,0),c(0,1))
)

start=which(pipes=="S", arr.ind = T)
pipes[(start[1]-1):(start[1]+1),(start[2]-1):(start[2]+1)]

bfs_pipes <- function(pipes) {
  
  pos = which(pipes=="S", arr.ind = T)
  pipes[pos] <-"J" #set manually
  
  distances <- matrix(Inf, nrow=ncol(pipes), ncol=ncol(pipes))
  visited <- matrix(F, nrow=ncol(pipes), ncol=ncol(pipes))
  distances[pos] <- 0
  visited[pos] <- T
  
  queue = list(pos)
  
  while (length(queue)>0) {
    
    pos = queue[[1]]
    queue <- queue[-1]
    
    moves <- pipe_type[[pipes[pos]]]
    next_pos <- map(moves,~{pos+.x}) #%>% purrr::reduce(rbind)
    
    for (np in next_pos) {
      if ((np[1]<=0) | (np[1]>nrow(pipes)) | (np[2]<=0) | (np[2]>nrow(pipes))) {next}
      if (visited[np]) {next}
      visited[np] <- T
      distances[np] <- distances[pos]+1
      queue = c(queue, list(np))
    }
  }
  return(distances)
}

dists = bfs_pipes(pipes)
max(dists[dists<Inf])


# Part 2 ####
count_crossings <- function(string) {
  string = str_remove_all(string,"[^|LJ7F]")
  str_count(string, "\\||(L7)|(FJ)")
}
clean_pipes <- ifelse(dists==Inf,".",pipes)
clean_pipes[start] <-"J"

insides = apply(clean_pipes,1,function(row){
  l=length(row)
  crossings = map_dbl(1:l, function(c) {
    row[-c(1:c)] %>% str_c(collapse = "") %>% 
      count_crossings()
  })
  inside = (crossings%%2)==1
  inside
}) %>% t()

insides <- ifelse(dists!=Inf,F,insides)

sum(insides)


# vis ####
plot_matrix(dists) +
  scale_fill_viridis_c()

plot_matrix(insides) +
  scale_fill_viridis_d()
  
