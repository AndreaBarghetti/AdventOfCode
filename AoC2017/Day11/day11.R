library(tidyverse)

input <- read_lines('AoC2017/Day11/input.txt')

dirs = str_split(input,",", simplify = T)
# Part 1 ----

# from https://www.redblobgames.com/grids/hexagons/
get_pos = function(dirs) {
  
  pos = c(r=0,q=0)
  
  for (d in dirs) {
    
    s = switch(d,
               n = c(-1,0),
               ne = c(-1,1),
               se = c(0,1),
               s = c(1,0),
               sw = c(1,-1),
               nw = c(0,-1))
    
    pos = pos+s 
  }
  pos['s'] = -pos['r']-pos['q']
  pos
}
pos = get_pos(dirs)
max(abs(pos))

# Part 2 ----
max_dist = function(dirs) {
  
  max_dist=0
  
  pos = c(r=0,q=0,s=0)
  
  for (d in dirs) {
    
    rq = switch(d,
                n = c(-1,0),
                ne = c(-1,1),
                se = c(0,1),
                s = c(1,0),
                sw = c(1,-1),
                nw = c(0,-1))
    
    pos = pos + c(rq,0)
    pos['s'] = -pos['r']-pos['q']
    max_dist = max(max_dist, max(abs(pos)))
  }
  max_dist
}

max_dist(dirs)
