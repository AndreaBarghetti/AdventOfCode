library(tidyverse)
input <- read_tsv('AoC2017/Day2/input.txt', col_names = F)

# Part 1 ----
input %>% 
  apply(1,function(x) {diff(range(x))}) %>% 
  sum()

# Part 2 ----
solve_line = function(x) {
  x = sort(x, decreasing = T)
  for (i in 1:(length(x)-1)) {
    for (j in (i+1):length(x)) {
      if (x[i]%%x[j]==0) return(x[i]/x[j])
    }
  }
}

input %>% 
  apply(1,solve_line) %>% 
  sum()
