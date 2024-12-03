library(tidyverse)
input <- read_lines("AoC2024/Day3/input.txt") %>% paste(collapse = "")

# Part 1 ####
get_muls = function(input) {
  str_extract_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)", simplify = T) 
}

muls = get_muls(input)

do_mul = function(mul) {
  if (mul=="") {return(0)}
  str_extract_all(mul, "\\d+", simplify = T) %>% 
    as.integer() %>% prod()
}

sum(map_int(muls, do_mul))
  
# Part 2 ####
input = paste0('do()', input, "don't()")

str_extract_all(input, "do\\(\\).*?don't\\(\\)", simplify = T) %>% 
  map(get_muls) %>% 
  map(map_int, do_mul) %>% 
  map_int(sum) %>% 
  sum()
