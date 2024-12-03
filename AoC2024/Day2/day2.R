library(tidyverse)
input <- read_lines("AoC2024/Day2/input.txt")

levels = map(input, str_extract_all, "\\d+", simplify=T) %>% 
  map(as.integer)

# Part 1 ####
is_safe = function(lv) {
  
  d = lv - lead(lv)
  d=d[-length(d)]
  order = all(sign(d)==-1) | all(sign(d)==1)
  steps = all(abs(d)>=1 & abs(d)<=3)
  order&steps
}

map_lgl(levels, is_safe) %>% sum()

# Part 2 ####
is_safe2 = function(lv) {
  
  if (is_safe(lv)) { return(TRUE) }
  
  for (l in seq_along(lv)) {
    slv = lv[-l]
    if (is_safe(slv)) {return(TRUE)}
  }
  return(FALSE)
}

map_lgl(levels, is_safe2) %>% sum()
