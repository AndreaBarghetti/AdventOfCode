library(tidyverse)

input <- readLines("Day4/input.txt")

# part1 ####
check_within <- function(x) {
  map_lgl(x, function(x) {

    pos <- x %>% str_split("[-,]", simplify = T) %>%
      as.integer()
    overlap <- all(between(pos[1:2], pos[3],pos[4])) | all(between(pos[3:4], pos[1],pos[2]))
    overlap

  })
}
check_within(input) %>% sum()


# part2 ####
check_within2 <- function(x) {
  map_lgl(x, function(x) {
    pos <- x %>% str_split("[-,]", simplify = T) %>%
      as.integer()
    overlap <- any(between(pos[1:2], pos[3],pos[4])) | any(between(pos[3:4], pos[1],pos[2]))
    overlap
  })
}
check_within2(input) %>% sum()





