library(tidyverse)
input <- read_lines("AoC2024/Day11/input.txt")

stones = str_extract_all(input,"\\d+") %>% unlist()

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

split_stones = function(stones){
  stones = as.numeric(stones)
  map(stones, split_stone) %>% unlist() %>% 
    as.character()
}

split_stones(stones)


blink =  function(stones, n) {
  
  stoner = collections::ordered_dict()
  
  for (s in stones) {
    stoner$set(s,1)
  }
  
  for (i in 1:n){
    
    new_stoner = collections::ordered_dict()
    while (stoner$size()>0) {
      
      s = stoner$popitem()
      
      new_stones = split_stones(s$key)

      for (ns in new_stones) {
        prev = new_stoner$get(ns,default = 0)
        new_stoner$set(ns,s$value+prev)
      }
      
    }
    stoner = new_stoner
    
    # N = stoner$values() %>% unlist() %>% sum()
    # print(paste0(i," blinks: ",N))
  }
  
  return(stoner)
}

stoner = blink(stones,25)

stoner$values() %>% unlist() %>% sum()

# Part 2 ####
stoner = blink(stones,75)
