library(tidyverse)
input <- read_lines("AoC2023/Day6/input.txt")

races = list(time = str_extract_all(input[[1]], "\\d+",simplify = T) %>% as.integer(),
             distance = str_extract_all(input[[2]], "\\d+", simplify = T) %>% as.integer())

# Part 1 ####
get_dist = function(time, charge) {
  charge*time-charge^2
}
get_dists <- function(time) {
  map_dbl(1:time,  ~{
    get_dist(time, .x)
  })
}

pmap_dbl(races, function(time, distance) {
  sum(get_dists(time) > distance)
}) %>% prod()

# Part 2 ####
race <- map(races, str_c,collapse = "") %>% map(as.numeric)

polyroot(c(race$distance+1, -race$time, 1)) %>%  as.numeric() %>% diff() %>% floor()
