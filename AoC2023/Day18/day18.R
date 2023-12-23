library(tidyverse)
input <- read_lines("AoC2023/Day18/input.txt")

parse_input <- function(input) {
  
  map_dfr(input, function(line){
    dir = str_extract(line,"^[A-Z]")
    dist = str_extract(line, "\\d+") %>% as.integer()
    color = str_extract(line, "#.{6}")
    return(list(dir=dir,dist=dist, color=color))
  })
  
}

str_extract(input,"^[A-Z]")
str_extract(input, "\\d+") %>% as.integer()
str_extract(input, "#.{6}")

moves = parse_input(input)

# Part 1 ####
get_corners <- function(moves) {
  
  pos = c(0,0)
  edges = list(pos)
  
  dir_to_values <- function(dir){
    if (dir == "R") {return(c(0,1))}
    if (dir == "L") {return(c(0,-1))}
    if (dir == "U") {return(c(-1,0))}
    if (dir == "D") {return(c(1,0))}
  }
  
  for (i in 1:nrow(moves)) {
    dir = dir_to_values(moves$dir[i])
    dir = dir * moves$dist[i]
    pos = pos+dir
    edges = c(edges, list(pos))
  }
  edges
}

corners = get_corners(moves)

# shoelace formula + half of edges
get_area = function(corners) {
  dets = map2_dbl(corners, lead(corners, default = corners[1]), function(c1,c2) {
    m = matrix(c(c1[2],c1[1], c2[2],c2[1]),nrow = 2)
    det(m)
  })
  edgs = map2_dbl(corners, lead(corners, default = corners[1]), function(c1,c2) {
    sum(abs(c2-c1))
  }) %>% sum() %>% `/`(2)+1
  sum(dets)/2 + edgs
}

get_area(corners)


# Part 2 ####
moves2 = moves %>% 
  mutate(dir = str_sub(color,7,7) %>% recode("0"="R","1"="D","2"="L","3"="U"),
         dist = str_sub(color,2,6) %>% strtoi(base = 16)) %>% 
  select(-color)

corners2 = get_corners(moves2)

get_area(corners2) %>% 
  format(scientific = F)


