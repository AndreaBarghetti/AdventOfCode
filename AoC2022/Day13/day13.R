library(tidyverse)

input <- read_lines("day13/input.txt")
source("Day13/functions.R")

# part 1 ####
pairs <- split(input[input!=""],f = cumsum(input=="")[input!=""])

correct_pairs <- map_lgl(pairs, function(pair,i) {compare_LR(pair[1], pair[2])})

sum(seq_along(correct_pairs) * correct_pairs)

# part 2 ####
divider_packets <- c("[[2]]","[[6]]")

packets <- unlist(pairs)

d2 <- map_lgl(packets, function(L) {
  compare_LR(L, divider_packets[1])
})
d6 <- map_lgl(packets, function(L) {
  compare_LR(L, divider_packets[2])
})

prod(sum(d2)+1,sum(d6)+2)
