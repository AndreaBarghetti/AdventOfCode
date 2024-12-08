library(tidyverse)
input <- read_lines("AoC2024/Day7/input.txt")

parse = function(input) {
  parse_line = function(line) {
    l = str_split(line,":", simplify = T)
    y = as.numeric(l[1])
    x = as.numeric(str_extract_all(l[2], "\\d+", simplify = T))
    
    return(list(y=y,x=x))
  }
  map(input, parse_line)
}

input = parse(input)

# Part 1 ####
operators = list(`+`, `*`)

check_equation = function(y,x) {
  if ( any(x>y) ) { return(FALSE) }
  if ( sum(x)>y ) { return(FALSE) }
  

}

# Part 2 ####
options(warn =2)
