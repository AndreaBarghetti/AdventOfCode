library(tidyverse)
input <- read_lines("AoC2023/Day23/input.txt")

map = str_split(input, "", simplify = T)

plot_map <- function(map) {
  AoCUtils::mat_to_tibble(map) %>% 
    mutate(color = case_when(value %in% c("^","<",">","v")~"red",
                             value=="#"~"black",
                             value=="."~"white"),
           label = case_when(value %in% c("^","<",">","v")~value,T~"")) %>% 
    ggplot(aes(x=x,y=y, fill=color)) +
    geom_tile(show.legend = F) +
    geom_text(aes(label=label)) +
    scale_fill_identity() +
    coord_equal() +
    theme_void()
}

plot_map(map)

# Part 1 ####

# simplify graph

# return pos of possible moves in all 4 directions
get_next_pos = function(map, pos) {
  
  moves <- list("^"=c(-1,0),"v"=c(1,0),"<"=c(0,-1),">"=c(0,1))
  
  next_pos = map(moves, function(move) {
    pos = pos + move
    if (any(pos==0) | pos[1]>nrow(map) | pos[2]>ncol(map)) {return(NULL)}
    if (map[pos]=="#") {return(NULL)}
    return(pos)
  })
  next_pos[!map_lgl(next_pos,is.null)]
}

get_junctions <- function(map) {
  junctions = list()
  
  start = cbind(r=1,c=which(map[1,]=="."))
  end = cbind(r=nrow(map),c=which(map[nrow(map),]=="."))
  junctions[[toString(start)]] = list(pos = cbind(r=1,c=which(map[1,]==".")))
  junctions[[toString(end)]] = list(pos=cbind(r=nrow(map),c=which(map[1,]==".")))
  
  for (r in 1:nrow(map)) {
    for (c in 1:ncol(map)) {
      if (map[r,c]==".") {
        pos = cbind(r,c)
        npos = get_next_pos(map, pos)
        if (length(npos)>2) {
          junctions[[toString(pos)]] <- list(pos=pos)
        }
      }
    }
  }
  junctions
}

junctions <- get_junctions(map)

# bfs to get distance from one point to all neighbouring junctions
get_next_junctions <- function(junction, junctions, map) {
  
  dests=list()
  moves <- list("^"=c(-1,0),"v"=c(1,0),"<"=c(0,-1),">"=c(0,1))
  queue = collections::queue()
  queue$push(junction$pos)
  visited = matrix(F, nrow(map), ncol(map))
  visited[junction$pos] <-T
  dist = matrix(Inf, nrow(map), ncol(map))
  dist[junction$pos] <- 0
  while (queue$size() >0) {
    pos = queue$pop()
    
    if (map[pos] %in% names(moves)) {use_moves = moves[map[pos]]} else {use_moves = moves}
    
    for (move in use_moves) {
      # move = use_moves[[2]]
      npos = pos + move
      if (any(npos==0) | npos[1]>nrow(map) | npos[2] > ncol(map)) {next}
      if (map[npos]=="#") {next}
      if (visited[npos]) {next}
      visited[npos] <- T
      dist[npos] <- dist[pos]+1
      
      if (!is.null(junctions[[toString(npos)]])) {
        dests[toString(npos)] = dist[npos]
      } else {
        queue$push(npos)
      }
      
    }
  }
  dests
  
}

graph = map(junctions, function(junction) {
  get_next_junctions(junction, junctions, map)
})


# try all paths
find_longest_path <- function(start, end, graph) {
 
  # save states to optimize speed
  # do states ever repeat though ?
  # states = rlang::env()
  
  if (length(graph[[start]])==0) {
    # print("This !!!!!")
    return(-Inf)
    }
  
  dists = map_dbl(names(graph[[start]]), function(e) {
    
    if (e==end) {return(graph[[start]][[e]])}
    ngraph = graph
    ngraph[[start]] <- NULL
    graph[[start]][[e]] + find_longest_path(start=e, end=end, graph = ngraph)
  })
  max(dists)
}

start = cbind(r=1,c=which(map[1,]=="."))
end = cbind(r=nrow(map),c=which(map[nrow(map),]=="."))

find_longest_path(start=toString(start), end=toString(end), graph)

# part 2 ####"
map2 = map 
map2[map2 %in% c("^","v","<",">")] <- "."

junctions2 <- get_junctions(map2)
graph2 = map(junctions2, function(junction) {
  get_next_junctions(junction, junctions2, map2)
})

# it will take a few minutes
find_longest_path(start=toString(start), end=toString(end), graph2)
