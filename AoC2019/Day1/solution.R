library(tidyverse)
library(AoCUtils)

# Part 1 ####
input <- read_aoc_input_with(year = 2019, day = 1, .fun = read_lines)
input = as.integer(input)

count_fuel <- function(x) {
  (floor(x/3)-2) %>% sapply(max, 0)
}

sum(count_fuel(input), na.rm=T)

# Part 2 ####
count_fuel2 <- function(x) {
  fuel <- 0
  f <- count_fuel(x)
  while(any(f > 0)) {
    fuel = fuel+f
    f = count_fuel(f)
  }
  fuel
}

sum(count_fuel2(input))
