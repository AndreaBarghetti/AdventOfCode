library(tidyverse)

# Day 6 ####
input6 <- readLines("Day6/input6.txt") 

# * - part 1 - *####

answers <- input6 %>% 
  lapply(function(x) {
    if (x=="") "NEXT-GRP"
    else x
  }) %>% paste(collapse = " ") %>%
  str_split("NEXT-GRP") %>%
  unlist() %>% 
  str_remove_all("^ | $")

count_uniq <- sapply(answers, function(x) {
  x %>%
    str_remove_all(" ") %>% 
    str_split("") %>% 
    unlist() %>% 
    unique() %>%
    length()
})

sum(count_uniq)

# * - part 2 - *####
count_common <- sapply(answers, function(x) {
  x %>%
    str_split(" ") %>%
    unlist() %>%
    lapply(function(x) {
      str_split(x, "") %>% 
        unlist()
    }) %>%
    reduce(intersect) %>%
    unique() %>%
    length()
})

sum(count_common)
