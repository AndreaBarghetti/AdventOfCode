library(tidyverse)
input <- read_lines("AoC2025/Day1/input.txt")
parse_value = function(i) {
  switch(str_sub(i,1,1), L=-1,R=1) * as.numeric(str_sub(i,2,-1))
}

# Part 1 ####
count_zeros = function(input) {
  
  pos = 50
  zeros = 0
  for (i in input) {
    v = parse_value(i)
    v = v%%100
    pos = pos + v
    pos = pos%%100
    if (pos == 0 ) {zeros = zeros + 1}
  }
  zeros
}
count_zeros(input)

# Part 2 ####
count_zeros2 = function(input) {
  pos = 50
  zeros = 0
  for (i in input) {
    v = parse_value(i)
    zeros = zeros + abs(v)%/%100
    v = sign(v) * (abs(v)%%100)
    npos = pos + v
    if (pos!=0 & (npos <0 | npos >99 | npos%%100 == 0)) {zeros = zeros + 1}
    
    pos = npos
    pos = pos%%100
  }
  zeros
}

count_zeros2(input)
