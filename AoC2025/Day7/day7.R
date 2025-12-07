library(tidyverse)
input <- read_lines("AoC2025/Day7/input.txt")

map = str_split(input,"", simplify = T)

# Part 1 ####
use_beam = function(map) {
  splits = 0
  for (r in 1:nrow(map)) {
    for (c in 1:ncol(map)) {
      m = map[r,c]
      if (m=='S') {map[r,c]="|"}
      if (m=="." & r>1) {
        if (map[r-1,c]=="|") {
          map[r,c]="|"
        }
      }
      if (m=='^') {
        if (map[r-1,c]=='|'){
          splits = splits+1
          map[r,c-1]="|"
          map[r,c+1]="|"
        }
      }
    }
  }
  splits
}

use_beam(map)

# Part 2 ####
use_beam2 = function(map) {
  
  particles = matrix(0,
                     nrow = nrow(map), 
                     ncol = ncol(map))
  
  particles[map=="S"]=1 

  for (r in 2:nrow(map)) {
    for (c in 1:ncol(map)) {
      m = map[r,c]
      p = particles[r-1,c]
      
      if (m!="^") {
        if (p>0) {
          particles[r,c] = particles[r,c] + p
        }
      }
      if (m=='^') {
        if (p>0){
          particles[r,c-1] = particles[r,c-1] + p
          particles[r,c+1] = particles[r,c+1] + p
        }
      }
    }
  }
  sum(particles[r,])
}

use_beam2(map) %>% 
  format(scientific=F)
