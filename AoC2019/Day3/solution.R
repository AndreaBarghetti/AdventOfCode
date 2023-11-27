library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 3, .fun = read_lines)

read_wire <- function(x) {
  txt <- x %>% 
    str_split(",", simplify = T)
  dirs = str_sub(txt,1,1)
  values = str_extract(txt, '\\d+') %>% as.integer()
  list(dirs=dirs,values=values)
}

wires <- map(input, read_wire)

get_wire_path <- function(wire) {
  
  x <- y <- l <- integer(length = sum(wire$values)+1)
  pos <- 1
  
  for (s in seq_along(wire$values)) {
    
    d = wire$dirs[s]
    v = wire$values[s]
    
    i <- pos + 1:v
    
    if (d == "U") {
      x[i] <- x[pos]
      y[i] <- y[pos] + 1:v
    }
    if (d == "D") {
      x[i] <- x[pos]
      y[i] <- y[pos] - 1:v
    }
    if (d == "L") {
      x[i] <- x[pos] - 1:v
      y[i] <- y[pos]
    }
    if (d == "R") {
      x[i] <- x[pos] + 1:v
      y[i] <- y[pos]
    }
    
    l[i] <- l[pos] + 1:v
    
    pos <- max(i)
    
  }
  
  return(tibble(x=x,y=y,l=l))
}

wire_paths <- map(wires, get_wire_path)

crossings <- wire_paths[[1]] %>% 
  inner_join(wire_paths[[2]], by=c("x","y"), suffix = c("W1","W2")) %>% 
  mutate(dist = abs(x)+abs(y)) %>% 
  arrange(dist)

crossings$dist[2]

# Part 2 ####
crossings %>% 
  mutate(wire_dist = lW1 + lW2) %>% 
  arrange(wire_dist)

