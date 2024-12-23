library(tidyverse)
input <- read_lines("AoC2024/Day16/input.txt")

map = str_split(input,"", simplify = T)

# Part 1 ####
run_maze = function(map) { 
  
  best_score = Inf
  
  visited = matrix(F, nrow(map),ncol(map))
  
  moves = list(
    U = c(-1,0),
    R = c(0,1),
    D = c(1,0),
    L = c(0,-1)
  )
  
  turn = function(dir, side) {
    
    dirs = c("U","R","D","L")
    
    i = which(dirs == dir)
    if (side=="L"){i = i-1; i = (i-1)%%4+1}
    if (side=="R"){i = i+1; i = (i-1)%%4+1}
    dirs[i]
  }
  
  start = which(map=="S", arr.ind = T)
  dir = "L"
  
  status = list(pos=start, 
                dir=dir, 
                score=0)
  
  get_nexts = function(status) {
    
    list(
      run = list(pos = status$pos + moves[[status$dir]], 
                 dir = status$dir, 
                 score = status$score+1),
      
      turnL = list(pos = status$pos + moves[[turn(status$dir, "L")]], 
                   dir = turn(status$dir, "L"), 
                   score=status$score+1001),
      
      turnR = list(pos = status$pos + moves[[turn(status$dir, "R")]], 
                   dir = turn(status$dir, "R"), 
                   score = status$score+1001)
      
    )
  }
  
  queue = collections::priority_queue()
  queue$push(status, -status$score)
  
  
  while (queue$size()>0){
    status = queue$pop()
    
    if (map[status$pos]=="#") {next}
    if (status$score>best_score) {next}
    if (map[status$pos]=="E") {best_score = status$score; next}
    if (visited[status$pos]) {next}
    visited[status$pos] <- T
    
    next_s = get_nexts(status)
    for (s in next_s) {
      queue$push(s, -s$score)
    }
    
    
  }
  
  return(best_score)
  
}

run_maze(map)

# Part 2 ####
run_maze = function(map) { 
  
  best_score = Inf
  
  paths=list()
  
  visited = matrix(Inf, nrow(map),ncol(map))
  
  moves = list(
    U = c(-1,0),
    R = c(0,1),
    D = c(1,0),
    L = c(0,-1)
  )
  
  turn = function(dir, side) {
    
    dirs = c("U","R","D","L")
    
    i = which(dirs == dir)
    if (side=="L"){i = i-1; i = (i-1)%%4+1}
    if (side=="R"){i = i+1; i = (i-1)%%4+1}
    dirs[i]
  }
  
  start = which(map=="S", arr.ind = T)
  dir = "L"

  status = list(pos=start, 
                dir=dir, 
                score=0,
                path=list())
  
  get_nexts = function(status) {
    
    run = list(pos = status$pos + moves[[status$dir]], 
               dir = status$dir, 
               score = status$score+1,
               path = status$path)
    
    turnL = list(pos = status$pos + moves[[turn(status$dir, "L")]], 
                 dir = turn(status$dir, "L"), 
                 score=status$score+1001,
                 path = status$path)
    
    turnR = list(pos = status$pos + moves[[turn(status$dir, "R")]], 
                 dir = turn(status$dir, "R"), 
                 score = status$score+1001,
                 path = status$path)
    
    options = list(run, 
                   turnL,
                   turnR)
    
    options[map_lgl(options, ~map[.x$pos]!='#')]
    
  }
  
  queue = collections::priority_queue()
  queue$push(status, -status$score)
  
  
  while (queue$size()>0){
    status = queue$pop()
    
    if (map[status$pos]=="#") {next}
    if (status$score>best_score) {next}
    if (map[status$pos]=="E") {
      best_score = status$score
      paths = c(paths,list(list(path = status$path, score = status$score)))
      next
      }
    if (status$score > visited[status$pos]+1000) {next} else {visited[status$pos] <- (status$score)}
    
    status$path = c(status$path,list(status$pos))
    
    next_s = get_nexts(status)
    for (s in next_s) {
      queue$push(s, -s$score)
    }
    
    
  }
  
  return(paths)
  
}

paths = run_maze(map)

map(paths, ~.x$path) %>%
  unlist(recursive = F) %>% 
  unique() %>% length() +1
