library(tidyverse)
input <- read_lines("AoC2024/Day15/input.txt")

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
      cat(paste0(x, collapse = ""))
      cat("\n")
    })
  cat("\n")
  invisible()
}

# Part 1 ####
try_move = function(x, dir) {

  if (dir == -1) { x=rev(x) ; x = try_move(x,1); return(rev(x))}
  
  p = which(x=="@")
  
  if (x[p+1]=="#") { return(x) } 
  
  if (x[p+1]==".") { x[p] = "."; x[p+1]="@" ; return(x)} 
  
  front = x[(p+1):length(x)]
  front = front[1:min(which(front=="#"))]
  
  if (any(front==".")) {
    x[p]="."
    p=p+1
    x[p]="@"
    x[min(which(x[(p+1):length(x)]=="."))+p] = "O"
  } 
  
  return(x)
  
}

move_robot = function(map, moves) {
  
  for (m in moves) {
    
    pos = which(map == "@", arr.ind = T)
    
    if (m ==">") { map[pos[1],] = try_move(map[pos[1],],1) }
    if (m =="<") { map[pos[1],] = try_move(map[pos[1],],-1) }
    if (m =="^") { map[,pos[2]] = try_move(map[,pos[2]],-1) }
    if (m =="v") { map[,pos[2]] = try_move(map[,pos[2]],1) }
    
    # print_map(map)
  }
  
  map
}

count_gps_score = function(map, box = 'O') {
  
  boxes = which(map == box, arr.ind = T)-1
  apply(boxes,1,function(x) {100*x[1]+x[2]}) %>% 
    sum()
}

move_robot(map, moves) %>% 
  count_gps_score()

# Part 2 ####
double_width = function(map) {
  
  .double_width = function(x) {
    map(x, ~{
      if (.x=="O") {return(c("[","]"))}
      if (.x==".") {return(c(".","."))}
      if (.x=="#") {return(c("#","#"))}
      if (.x=="@") {return(c("@","."))}
    }) %>% unlist()
  }
  
  t(apply(map,1,.double_width))
}

try_move = function(map, move) {
  
  robot = which(map=="@", arr.ind = T)
  visited = matrix(F, nrow = nrow(map),ncol = ncol(map))
  
  moves = list(">" = c(0,1),
               "<" = c(0,-1),
               "^" = c(-1,0),
               "v" = c(1,0))
  
  m = moves[[move]]
  #if not a box
  if (map[robot+m]==".") { map[robot] ="."; map[robot+m] = "@"; return(map)}
  if (map[robot+m]=="#") { return(map) }

  # push boxes  
  queue = collections::queue()
  queue$push(robot)
  
  while(queue$size()>0) {
    
    p = queue$pop()
    if (visited[p]) {next}
    visited[p] = T
    
    np = p + m
    
    if (map[np] == "#") {return(map)}
    if (map[np] == "[") { queue$push(np);  queue$push(np+c(0,1)) }
    if (map[np] == "]") { queue$push(np);  queue$push(np+c(0,-1)) }
    
  }
  moved_boxes = which(visited & map %in% c("[","]","@"), arr.ind = T)
  boxes = map[moved_boxes]
  map[moved_boxes] = "."
  moved_boxes = t(t(moved_boxes) + m)
  map[moved_boxes] = boxes
  return(map)
}

move_robot = function(map, moves) {
  
  for (move in moves) {
    map = try_move(map, move)
  }
  map
}

map2 = double_width(map)

move_robot(map2, moves) %>% 
  count_gps_score(box = "[")
