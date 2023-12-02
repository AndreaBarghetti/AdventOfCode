library(tidyverse)

input <- read_lines("AoC2023/Day1/input.txt")

# Part 1 ####
input %>% str_extract_all("\\d") %>% 
  map_int(~paste0(.x[1],rev(.x)[1]) %>% as.integer()) %>% 
  sum()

# Part 2 ####
extract_nums <- function(input) {
  
  nums = c("one"="1", "two"="2", "three"="3", 
           "four"="4", "five"="5", "six"="6", 
           "seven"="7", "eight"="8", "nine"="9")
  
  regex = "(\\d|one|two|three|four|five|six|seven|eight|nine)"
  
  n1 = input %>% str_extract(regex) %>% 
    str_replace_all(nums)
  
  n2 = input %>% 
    str_replace(paste0(".*",regex), "\\1") %>% 
    str_extract(regex) %>% 
    str_replace_all(nums)
    
  paste0(n1,n2) %>% as.integer()
  
}

extract_nums(input) %>% sum()

