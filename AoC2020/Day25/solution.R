library(tidyverse)

# Day 25: Combo Breaker ####
input=read_lines('AoC2020/Day25/input.txt')

card_pk = input[1] %>% as.numeric()
door_pk = input[2] %>% as.numeric()

# * - part 1 - *####
gen_pk = function(sn, loop_size) {
  v=1
  for (i in 1:loop_size) {
    v = (v*sn)%%20201227
  }
  v
}

find_loop_size =function(pk) {
  v=1
  i=0
  while(v!=pk) {
    i=i+1
    v = (v*7)%%20201227
  }
  i
}

card_loop_size = find_loop_size(card_pk)
door_loop_size = find_loop_size(door_pk)

gen_pk(door_pk, card_loop_size)
gen_pk(card_pk, door_loop_size)
