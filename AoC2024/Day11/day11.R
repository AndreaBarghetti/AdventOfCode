library(tidyverse)
input <- read_lines("AoC2024/Day11/input.txt")

stones = str_extract_all(input,"\\d+", simplify = T) %>% as.numeric()

# Part 1 ####
split_stone = function(s) {
  
  if(s==0){return(1)}
  
  c = as.character(s)
  l = str_count(c)
  if (l%%2==0 ){
    s1 = str_sub(c,1,l/2)
    s2 = str_sub(c,l/2+1,-1)
    return(as.numeric(c(s1,s2)))
    }
  
 return(s * 2024)
  
}

# Part 2 ####
