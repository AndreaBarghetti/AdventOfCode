library(tidyverse)
input <- read_lines("AoC2024/Day13/test.txt")

parse = function(input) {
 raw_grps = split(input, cumsum(input=="")) %>% 
    map(~.x[.x!=""])

 parse_grp = function(grp) {
   machine = grp %>% str_extract_all("[+-]*\\d+", simplify = F) %>% 
     map(as.numeric) %>% 
     map(setNames, c('x','y')) %>% 
     setNames(c("A","B","P")) 
   machine
 }
  
 machines = map(raw_grps, parse_grp)
 machines
}

machines = parse(input)

# Part 1 ####
find_solutions <- function(m, max_value = 100) {
  solutions <- list()
  
  for (N in 1:max_value) {
    for (M in 1:max_value) {
      # N * A$x + M * B$x = P$x
      # N * A$y + M * B$y = P$y
      if ((N * m$A['x'] + M * m$B['x'] == m$P['x']) && (N * m$A['y'] + M * m$B['y'] == m$P['y'])) {
        solutions <- append(solutions, list(c(N = N, M = M, cost = 3*N+M)))
      }
    }
  }
  return(solutions)
}
solutions = map(machines, find_solutions)

map_dbl(solutions, function(s) {
  if(is_empty(s)){return(0)}
  min(map_dbl(s,~.x['cost']))
}) %>% sum()

# Part 2 ####
correct_pos = function(machines) {
  map(machines, function(m){
    m$P = m$P + 1e+13
    m
  })
}

machines2 = correct_pos(machines)
