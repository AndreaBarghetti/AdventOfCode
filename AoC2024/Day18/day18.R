library(tidyverse)
input <- read_lines("AoC2024/Day18/input.txt")

parse = function(input) {
  str_extract_all(input,"\\d+",simplify = T) %>% 
    apply(2,as.integer) + 1
}
bytes = parse(input)
map = matrix(F, nrow = 71, ncol = 71)
map[bytes[1:1024]] <- T

# Part 1 ####

# Part 2 ####
