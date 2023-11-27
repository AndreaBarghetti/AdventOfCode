library(tidyverse)

input <- readLines("Day3/input.txt")

# part1 ####
get_common <- function(sack) {
  map_chr(sack, function(sack) {
    split <- str_split(sack,"", simplify = T)
    c1 <- split[1:length(split)/2]
    c2 <- split[(length(split)/2+1):length(split)]
    common <- intersect(c1,c2)
    common
  })
}

scoring <- c(setNames(1:26,letters),setNames(27:52,LETTERS))

get_score <- function(letter) {
  recode(letter, !!!scoring)
}

get_common(input) %>%
  get_score() %>%
  sum()

# part2 ####
groups_f <- (seq_along(input)-1)%/%3
groups <- split(input, groups_f)

intersect_sack <- function(sack1,sack2) {
  s1 <- str_split(sack1,"", simplify = T)
  s2 <- str_split(sack2,"", simplify = T)
  intersect(s1,s2)
}
get_badge <- function(group) {
  group %>%
    purrr::reduce(intersect_sack)
}

map_chr(groups,get_badge) %>%
  get_score() %>%
  sum()
