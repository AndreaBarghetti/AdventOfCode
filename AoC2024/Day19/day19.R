library(tidyverse)
input <- read_lines("AoC2024/Day19/input.txt")

parse = function(input) {
  patterns = input[1] %>% str_split(", ") %>% unlist()
  designs = input[-c(1:2)] %>% unlist()
  return(list(patterns=patterns,designs=designs))
}

patterns = parse(input)$patterns
designs = parse(input)$designs

# Part 1 ####
match_design = function(design, patterns) {
  if (any(patterns==design)) {return(T)}
  # p=patterns[1]
  for (p in patterns) {
    if (str_starts(design,p)) {
      cut_design = str_remove(design,p) 
      if (match_design(cut_design, patterns)) {return(T)}
    }
  }
  return(FALSE)
}

sum(map_lgl(designs, match_design, patterns))
#342

# Part 2 ####
