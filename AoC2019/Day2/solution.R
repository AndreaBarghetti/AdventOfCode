library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 2, .fun = read_lines)
input <- input %>% str_split(",", simplify = T) %>% as.integer()

# Part 1 ####
read_code <- function(x) {
  i <- 1 
  
  repeat {
    
    op <- x[i]
    r1 <- x[x[i+1]+1]
    r2 <- x[x[i+2]+1]
    w <- x[i+3]+1
    
    if (op == 1) {
      x[w] <- r1+r2
    }
    if (op == 2) {
      x[w] <- r1*r2
    }
    if (op == c(99)) {return(x)}
    if (!op %in% c(1,2,99)) {stop("Error")}
    i <- i + 4
  }
  return(x)
}

replace(input, 2:3,c(12,2)) %>% 
  read_code() %>% 
  pluck(1)

# Part 2 ####
read_code2 <- function(x, noun, verb) {
  x = replace(x, 2:3,c(noun,verb))
  read_code(x) %>% 
    pluck(1)
}

find_nv <- function(input, exp_output) {
  for (n in 0:99) {
    for(v in 0:99) {
      if (read_code2(input,n,v)==exp_output) {
        return(list(n=n,v=v, result = 100*n+v))
      }
    }
  }
}

find_nv(input,19690720)
