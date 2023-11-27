library(tidyverse)

#Day 1 ####
input1 <- readLines("Day1/input1.txt") %>% 
  as.numeric()

# * - part 1 - *####
solution1 <- input1 %>% 
  c(.,abs(. -2020)) %>% 
  subset(duplicated(.)) %>% 
  prod()

# or
solution1 <- combn(input1, 2, FUN = prod, simplify = TRUE)[which(combn(input1, 2, FUN = sum, simplify = TRUE)==2020)]

# * - part 2 - *####
solution2 <- combn(input1, 3, FUN = prod, simplify = TRUE)[which(combn(input1, 3, FUN = sum, simplify = TRUE)==2020)]

