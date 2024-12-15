library(tidyverse)
input <- read_lines("AoC2024/Day15/test.txt")

parse = function(input) {
  
  sep = which(input=="")
  
  map = input[1:(sep-1)] %>% str_split("",simplify = T)
  
  moves = input[(sep+1):length(input)] %>% 
    str_split("") %>% unlist()
  
  return(list(map=map,
              moves=moves))
}

map = parse(input)$map
moves = parse(input)$moves

print_map = function(map) {
  map %>% 
    apply(1, function(x){
      x=ifelse(x==0,".",as.character(x))
      cat(paste0(x, collapse = ""));cat("\n")
    })
  invisible()
}

# Part 1 ####

try_move = function(x, dir) {

  if (dir == -1){x=rev(x)}
  
  p = which(x=="@")
  
  if (x[p+1]==".") { x[p] = "."; x[p+1]="@" } 
  
  front = x[(p+1):length(x)]
  which(front=="#")
  
  else if (any(front==".")) {
    x[p]="."
    p=p+1
    x[p]="@"
    x[min(which(x[(p+1):length(x)]=="."))+p] = "#"
  } 
  
  if (dir == -1) {x=rev(x)}
  return(x)
  
}

move_robot = function(map, moves) {
  
  
  for (m in moves) {
    
    pos = which(map=="@", arr.ind = T)
    
    if (m ==">") {map[pos[2],] = try_move(map[pos[2],],1)}
    if (m =="<") {}
    if (m =="^") {}
    if (m =="v") {}
  }
  
  
}

# Part 2 ####
