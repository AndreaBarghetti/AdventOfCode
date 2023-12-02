library(tidyverse)

input <- read_lines("AoC2015/Day12/input.txt")


# part 1 ####
sum_nums <- function(x) {
  str_extract_all(x, "-{0,1}\\d+", simplify = T) %>% 
    as.numeric() %>% sum()
}

res = sum_nums(input)

res

# part 2 ####
list = jsonlite::fromJSON(input %>% str_replace_all(':\\"red\\"',':\\"RED\\"'), simplifyVector = T)

rm_red = function(list) {
  
  if (!is.list(list)) {return(list)}
  
  if ('RED' %in% list) {
    return(0)
  } else {
    list = map(list, rm_red) 
  }
  return(list)
}

rm_red(list) %>% unlist() %>% as.numeric() %>% sum(na.rm = T)
