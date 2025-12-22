library(tidyverse)
input <- read_lines('AoC2017/Day3/input.txt') %>% as.numeric()

# Part 1 ----
get_dist = function(input) {

  layer = ceiling((sqrt(input) - 1) / 2)
  side = layer * 2 + 1
  max_val <- side^2
  dist_to_max <- (max_val - input) %% (side - 1)
  dist_mid <- abs(dist_to_max - layer)
  layer + dist_mid
}
get_dist(input)

# Part 2 ----
library(collections)

first_above = function(input) {
  
  m = matrix(0,nrow=11, ncol = 11) 
  
  dirs = list(c(-1,0),c(0,-1),c(1,0),c(0,1))
  
  l=1
  r=nrow(m)%/%2+1;c=ncol(m)%/%2+1
  m[r,c] = 1
  
  repeat {
    c=c+1; r=r+1 ; l=l+2

    for (d in dirs) {
      for (step in 1:(l-1)) {
        r=r+d[1];c=c+d[2]
        m[r,c] = sum(m[(r-1):(r+1),(c-1):(c+1)])
        if (m[r,c]>=input){return(m[r,c])}
      }
    }
  }
}

first_above(input)

