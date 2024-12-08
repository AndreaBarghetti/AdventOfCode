library(tidyverse)
input <- read_lines("AoC2024/Day7/test.txt")

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

try_operators = function(R, x, y, operators) {
  for (o in operators) {
    if (length(x)>1) {
      R = do.call(o,list(R,x[1]))
      return(try_operators(R,x[-1], y, operators))
    } else {
      R = do.call(o,list(R,x[1]))
      if (R == y) {return(TRUE)} else {return(FALSE)} 
    }
  }
}

map_lgl(input, ~{
  try_operators(.x$x[1],.x$x[-1], .x$y, operators)
})


check_equation = function(y,x) {
  if ( any(x>y) ) { return(FALSE) }
  if ( sum(x)>y ) { return(FALSE) }
  return(TRUE)
}




# Part 2 ####
