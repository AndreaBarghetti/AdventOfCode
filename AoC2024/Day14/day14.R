library(tidyverse)
input <- read_lines("AoC2024/Day14/input.txt")

width = 101
heigth = 103

parse = function(input) {
  input %>% 
    str_extract_all("[+-]*\\d+") %>% 
    map(as.numeric) %>%
    map(~list(pos = .x[1:2]+1, vel=.x[3:4]))
}

robots = parse(input)
map = matrix(0, nrow = heigth, ncol = width)

# Part 1 ####
move_robot = function(robot, sec, map) {
  np = robot$pos + sec * robot$vel
  
  robot$pos <- ((np-1) %% (dim(map)[2:1]))+1
  robot
}

move_robots = function(robots, sec, map) {
  map(robots, move_robot, sec, map)
}

locate_robots = function(robots, map) {
  
  for (r in robots) {
    # print(r)
    map[rbind(r$pos[2:1])] <-  map[rbind(r$pos[2:1])]+1
  }
  
  map
  
}

get_safety_factor = function(map) {
  
  mid_r = nrow(map)%/%2+1
  mid_c = ncol(map)%/%2+1
  
  q1 = map[1:(mid_r-1), 1:(mid_c-1)] %>% sum()
  q2 = map[1:(mid_r-1), (mid_c+1):ncol(map)] %>% sum()
  q3 = map[(mid_r+1):nrow(map), 1:(mid_c-1)] %>% sum()
  q4 = map[(mid_r+1):nrow(map), (mid_c+1):ncol(map)] %>% sum()

  prod(c(q1,q2,q3,q4))
    
}

print_map = function(map) {
  map %>% 
    apply(1, function(x){
      x=ifelse(x==0,".",as.character(x))
      cat(paste0(x, collapse = ""));cat("\n")
    })
}

moved_robots = move_robots(robots, 100, map)


moved_robots %>% 
  locate_robots(map) %>% 
  get_safety_factor()

# Part 2 ####
