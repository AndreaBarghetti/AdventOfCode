library(tidyverse)

# Day 5 ####
input5 <- readLines("Day5/input5.txt")

# * - part 1 - *####
seats <- list(a=str_sub(input5, 1,7),
              b=str_sub(input5, 8,10))

seats$row <- seats$a %>% str_replace_all(c("F"="0", "B"="1")) %>%
  strtoi(base=2)
seats$col <- seats$b %>% str_replace_all(c("L"="0", "R"="1")) %>%
  strtoi(base=2)

seats$id <- seats$row*8+seats$col

max(seats$id)

# * - part 2 - *####
all_seats <- min(seats$id):max(seats$id)

all_seats %>% magrittr::extract(!all_seats %in% seats$id)
