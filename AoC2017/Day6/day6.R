library(tidyverse)
library(collections)

input <- read_lines('AoC2017/Day6/input.txt')

memory = as.integer(str_split(input, "\t", simplify = T))

# Part 1 ----
redistr = function(memory) {
  L=length(memory)
  block_idx = which.max(memory)
  size = memory[block_idx]
  memory[block_idx]=0
  for (i in (1:size+block_idx-1) %% L+1) {
    memory[i] = memory[i] + 1
  }
  memory
}

find_loop = function(memory) {
  
  states = dict()
  cycles=0
  
  while (!states$has(memory)) {
    cycles = cycles+1
    states$set(memory, TRUE)
    memory = redistr(memory)
  }
  list(cycles=cycles,
       memory=memory)
}
find_loop(memory)$cycles

# Part 2 ----
memory = find_loop(memory)$memory
find_loop(memory)$cycles
