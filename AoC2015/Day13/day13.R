library(tidyverse)

input <- read_lines("AoC2015/Day13/input.txt")
# input <- read_lines("AoC2015/Day13/test.txt")

parse_input <- function(input) {
  input = str_remove(input, ".$")
    
  subj = str_extract(input, "^\\w+")
  obj = str_extract(input, "\\w+$")
  value = str_extract(input, "\\d+") %>% as.numeric()
  value = ifelse(str_detect(input, "lose"), -value,value)
  people <- unique(subj)
  map(setNames(people,people), function(p) {
    setNames(value[subj==p], obj[subj==p])
  })
}

scores = parse_input(input)

# part 1 ####
all_tables = combinat::permn(names(scores)[-1], c) %>% 
  map(~c(names(scores)[1],.x))

get_score = function(table,scores) {
  table
  L = lead(table, default = table[1])
  R = lag(table, default = table[length(table)])
  pmap_dbl(list(table, L, R), function(p,l,r) {
    scores[[p]][l]+scores[[p]][r]
  }) %>% sum()
}
map_dbl(all_tables, get_score, scores) %>% max()

# part 2 ####
scores2 = map(scores, ~c(.x,me=0)) %>% 
  c(list(me=setNames(numeric(length(scores)), names(scores))))

all_tables2 = combinat::permn(names(scores2)[-1], c) %>% 
  map(~c(names(scores2)[1],.x))

map_dbl(all_tables2, get_score, scores2) %>% max()
