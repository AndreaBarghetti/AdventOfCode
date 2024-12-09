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

try_operators = function(x, y, operators) {
  # quickly avoid obvious FALSE
  if ( any(x>y) ) { return(FALSE) }
  if (sum(x[x!=1])>y)  { return(FALSE) }
  
  if (length(x)==1) {
    return(x==y)
  } else {
    for (o in operators) {
      r = do.call(o, list(x[1],x[2]))
      nx = c(r, x[-c(1:2)])
      if (try_operators(nx,y,operators)) {return(TRUE)}
    }
  }
  return(FALSE)
}

test_passed = map_lgl(input, ~{
  try_operators(.x$x, .x$y, operators)
})

map_dbl(input, ~.x$y)[test_passed] %>% sum()

# Part 2 ####
concat = function(x1,x2) {
  as.numeric( paste0(x1,x2, collapse = "")) 
}

operators = list(concat, `*`, `+`)

test_passed = map_lgl(input, ~{
  try_operators(.x$x, .x$y, operators)
})

map_dbl(input, ~.x$y)[test_passed] %>% 
  sum() %>% 
  format(scientific = F)
