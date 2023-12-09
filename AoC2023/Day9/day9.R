library(tidyverse)
input <- read_lines("AoC2023/Day9/input.txt")
# input <- read_lines("AoC2023/Day9/test.txt")

oasis <- map(input, ~{
  str_split(.x, " ", simplify = T) %>% 
    as.numeric
})

# Part 1 ####
get_diffs <- function(values) {
  (lead(values)-values)[-length(values)]  
}

get_levels = function(history) {
  levels = list(history)
  i=1
  values=history
  while(!all(values==0)) {
    i=i+1
    values <- get_diffs(values)
    levels[[i]] = values
  }
  rev(levels)
}

expand_levels <-function(levels) {
  d = 0
  l = length(levels)
  for (i in 1:(l-1)) {
    levels[[i]] <- c(levels[[i]],d)
    d = rev(levels[[i+1]])[1]+d
  }
  levels[[i+1]] <- c(levels[[i+1]],d)
  levels
}

predict = function(history) {
  levels = get_levels(history) %>% 
    expand_levels()
  rev(levels)[[1]]
}

map(oasis, predict) %>% 
  map(rev) %>% 
  map_dbl(`[`,1) %>% 
  sum()



# Part 2 ####
expand_levels_bw <-function(levels) {
  d = 0
  l = length(levels)
  for (i in 1:(l-1)) {
    levels[[i]] <- c(d,levels[[i]])
    d = levels[[i+1]][1]-d
  }
  levels[[i+1]] <- c(d,levels[[i+1]])
  levels
}

predict_bw = function(history) {
  levels = get_levels(history) %>% 
    expand_levels_bw()
  rev(levels)[[1]]
}

map(oasis, predict_bw) %>% 
  map_dbl(`[`,1) %>% 
  sum()
