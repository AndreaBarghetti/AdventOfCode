library(tidyverse)
input <- read_lines("AoC2024/Day1/input.txt")

mat = map(input, str_extract_all,"\\d+", simplify = T) %>% 
  map(as.integer) %>% 
  reduce(rbind, deparse.level = F)

# Part 1 ####
abs(sort(mat[,1]) - sort(mat[,2])) %>% sum()

# Part 2 ####
t = map_int(mat[,1], ~sum(.x == mat[,2]))
sum(mat[,1]*t)
