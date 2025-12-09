library(tidyverse)
library(collections)

input = readLines('AoC2015/Day17/input.txt')

containers = as.integer(input)

# part 1 ####
count_combos = function(containers, vol=150) {
  # browser()
  q = stack()
  
  count = 0
  
  q$push(list(L = c(0), R = containers))
  
  while (q$size()>0) {
    
    s = q$pop()
    
    if (sum(s$L)==vol) {
      count = count+1
      next
    }
    if (sum(s$L)>vol) {
      next
    }
    if (sum(s$L,s$R)<vol) {
      next
    }
    if (sum(s$L)<vol) {
      q$push(list( L = s$L, R = s$R[-1]))
      q$push(list( L = c(s$L,s$R[1]), R = s$R[-1]))
    }
  }
  return(count)
}

count_combos(containers, 25)

# part 1 ####
list_combos = function(containers, vol=150) {

  q = stack()
  
  combos = list()
  
  q$push(list(L = c(0), R = containers))
  
  while (q$size()>0) {
    
    s = q$pop()
    
    if (sum(s$L)==vol) {
      combos = append(combos, list(s$L))
      next
    }
    if (sum(s$L)>vol) {
      next
    }
    if (sum(s$L,s$R)<vol) {
      next
    }
    if (sum(s$L)<vol) {
      q$push(list( L = s$L, R = s$R[-1]))
      q$push(list( L = c(s$L,s$R[1]), R = s$R[-1]))
    }
  }
  return(combos)
}

combos = list_combos(containers, 150)

Ls = map_int(combos, length)
m = min(Ls)

combos[Ls==m] %>% length()

