library(tidyverse)
input <- read_lines("AoC2024/Day6/input.txt")

map = str_split(input,"", simplify = T)
start = which(map == "^", arr.ind = T)
map[start] <- "."
direction = c(-1,0)

turnR = function(dir) {
  ndir = dir
  ndir[1]=dir[2]
  ndir[2]=dir[1]*-1
  ndir
}

is_out = function(pos,map) {
  
  any(pos<=0) | any(pos>dim(map))
  
}

# Part 1 ####
walk_map = function(map, pos, dir) {
  
  map[pos]='X'
  
  repeat {
    if(is_out(pos+dir,map)){break}
    if (map[pos+dir]=="#") {
      dir = turnR(dir)
      next }
    pos=pos+dir
    map[pos]='X'
  }
  
  return(map)
}

walked_map = walk_map(map, start, direction)
sum(walked_map=='X')

# Part 2 ####

# faster way to jump to next position before block
# 1 dimentional

stop_at = function(x, pos, dir) {
  stops = which(x=="#")
  stops = stops[sign((stops-pos)/(dir))>0]
  if (length(stops)==0) {return(Inf)}
  min(stops*dir)*dir-dir
}

# 2 dimentional
stop_at2 =  function(map, pos, dir) {
  
  # move up/down or L/R
  if (dir[1]!=0) {
    pos[1] = stop_at(map[,pos[2]], pos = pos[1], dir = dir[1])
  } else {
    pos[2] = stop_at(map[pos[1],], pos = pos[2], dir = dir[2])
  }
  return(pos)
}

check_loop2 = function(map, pos, dir) {
  
  map[pos]="." #prevent # in start pos
  
  events = collections::dict()
  events$set(paste0(c(pos, dir), collapse = "."),NULL)
  
  repeat {
    
    pos = stop_at2(map, pos, dir)
    dir = turnR(dir)
    
    if (any(pos==Inf)) {return(FALSE)} #out of map
    
    if (events$has(paste0(c(pos, dir), collapse = "."))) {
      return(TRUE)
    }
    
    events$set(paste0(c(pos, dir), collapse = "."), NULL)
  }
  
}

# only test positions that are walked
apply(which(walked_map=='X', arr.ind = T), 1, function(rc) {
  rc=rbind(rc)
  map[rc] <- '#'
  check_loop2(map, start, direction)
}) %>% sum()


# it's quite slow... there must be a much smarter solution
