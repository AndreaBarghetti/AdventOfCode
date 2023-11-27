library(tidyverse)

#Day 2 ####
input2 <- readLines("Day2/input2.txt")

# * - part 1 - *####
map_lgl(input2, function(x) {
  rule <- str_split(x,pattern = " ") %>% 
    unlist()
  
  min <- magrittr::extract(rule, 1) %>%
    str_split("-", simplify = T) %>% min()
  max <- magrittr::extract(rule, 1) %>%
    str_split("-", simplify = T) %>% max()
  letter <- magrittr::extract(rule, 2) %>% str_remove(":")
  passw <- magrittr::extract(rule, 3)
  str_count(passw, letter) %in% min:max
}) %>% unlist() %>% sum()

# * - part 2 - *####
map_lgl(input2, function(x) {
  rule <- str_split(x,pattern = " ") %>% 
    unlist()
  
  min <- magrittr::extract(rule, 1) %>%
    str_split("-", simplify = T) %>% min()
  max <- magrittr::extract(rule, 1) %>%
    str_split("-", simplify = T) %>% max()
  letter <- magrittr::extract(rule, 2) %>% str_remove(":")
  passw <- magrittr::extract(rule, 3)
  str_locate_all(passw, letter) %>% 
    data.frame() %>% 
    pull(start) %in% c(min, max) %>% 
    sum() == 1
}) %>% unlist() %>% sum()
