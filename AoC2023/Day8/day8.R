library(tidyverse)
library(AoCUtils)
library(gmp)

input <- read_lines("AoC2023/Day8/input.txt")

directions <- input[[1]] %>% str_split("") %>% unlist()

parse_input = function(input) {
  
  input = input[-c(1:2)]
  sites = str_extract(input, "^\\w+")
  L = str_extract(input, "(?<=\\()\\w+")
  R = str_extract(input, "\\w+(?=\\))")
  
  map2(L,R, function(L,R) {list(L=L,R=R)}) %>% 
    setNames(sites)
}

map <- parse_input(input)

# Part 1 ####
follow_directions <- function(map, directions, start="AAA", end="ZZZ") {
  directions <- circular(directions)
  pos = start
  i=0
  while(pos != end) {
    i=i+1
    pos = map[[pos]][[directions[i]]]
  }
  return(i)
}

follow_directions(map, directions)

# Part 2 ####
get_loop_len = function(map, directions, start) {
  directions <- circular(directions)
  pos = start
  l=length(directions)
  i=1

  states = list()
  t = c()
  
  repeat {
    pos = map[[pos]][[directions[i]]]
    i=i+1
    if(str_sub(pos,-1)=="Z") {
      t=c(t,i)
      state = str_c(pos,i%%l)
      if (!is.null(states[[state]])){break}
      states[[state]] <- c(states[[state]],i)
    }
  }
  t-1
}

loops = map(str_subset(names(map),"A$"), function(start) {
  t = get_loop_len(map, directions, start)
  t
})
names(loops) <- str_subset(names(map),"A$")

# LCM of two numbers
lcm_two <- function(a, b) {
  abs(a * b) / gcd(a, b)
}

# LCM of a vector of numbers
lcm_multiple <- function(numbers) {
  Reduce(lcm_two, numbers)
}

lcm_multiple(map_int(loops, diff)) %>% 
  format(scientific = F)
