library(tidyverse)
stream <- read_lines('AoC2017/Day9/input.txt')

# Part 1 ----
clean_stream = function(stream) {
  stream = str_remove_all(stream, "!.")
  stream = str_remove_all(stream, '<[^>]*>')
  stream = str_remove_all(stream, '[^\\{\\}]')
  stream
}

get_score = function(stream) {
  
  stream = clean_stream(stream)
  ss = str_split(stream,'', simplify = T)
  
  lvl=0
  score=0
  
  for (i in ss) {
    if (i =='{') {lvl = lvl+1 }
    if (i =='}') {score=score+lvl; lvl = lvl-1}
  }
  score
}

get_score(stream)

# Part 2 ----
clean_stream = function(stream) {
  stream = str_remove_all(stream, "!.")
  stream
}
clean_stream(stream) %>% 
  str_extract_all('<[^>]*>', simplify = T) %>% 
  str_sub(2,-2) %>% nchar() %>% 
  sum()
