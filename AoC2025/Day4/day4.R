library(tidyverse)
input <- read_lines("AoC2025/Day4/input.txt")

map = str_split(input,"", simplify = T)

# Part 1 ####
count_reachable_rolls = function(map) {
  reachable=0
  for (r in 1:nrow(map)) {
    for (c in 1:ncol(map)) {
      if (map[r,c]==".") {next}
      rolls = 0
      for (v in c(-1,0,1)) {
        for (h in c(-1,0,1)) {
          if (v==0 & h == 0){next}
          nc = c+h
          nr = r+v
          if (nc<=0 | nc>ncol(map) | nr <=0 | nr > nrow(map)) {next}
          if (map[nr,nc]=="@") { rolls=rolls+1}
        }
      }
      if (rolls<4) {reachable=reachable+1}
    }
  }
  reachable
}

count_reachable_rolls(map)

# Part 2 ####
collect_rolls = function(map) {
  
  n_rolls = sum(map=='@')
  
  for (r in 1:nrow(map)) {
    for (c in 1:ncol(map)) {
      if (map[r,c]==".") {next}
      rolls = 0
      for (v in c(-1,0,1)) {
        for (h in c(-1,0,1)) {
          if (v==0 & h == 0) {next}
          nc = c+h
          nr = r+v
          if (nc<=0 | nc>ncol(map) | nr <=0 | nr > nrow(map)) {next}
          if (map[nr,nc]=="@") { rolls = rolls+1}
        }
      }
      if (rolls<4) {map[r,c]="."}
    }
  }
  
  if (sum(map=='@') < n_rolls) {map = collect_rolls(map)}
  
  return(map)
}

collected = collect_rolls(map)
sum(map=='@') - sum(collected=='@')
