library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 6, .fun = read_lines)

parse_input <- function(input) {
  str_split(input, pattern = "\\)", simplify = T)
}

space_map <- parse_input(input)

# part 1 ####
get_center <- function(orbit) {
  space_map[,1][space_map[,2]==orbit]
}
get_orbits <- function(center) {
  space_map[,2][space_map[,1]==center]
}
get_nears <- function(orbit) {
  c(get_center(orbit), get_orbits(orbit))
}
dist_to_com <- function(orbit) {
  dist <- 0
  while (orbit !="COM") {
    dist = dist + 1
    orbit <- get_center(orbit) 
  }
  dist
}

count_orbits <- function(space_map) {
  centers <- unique(as.character(space_map))
  dists <- map_int(centers, dist_to_com)
  sum(dists)
}

count_orbits(space_map)

# part 2 ####

# breadth-first search
get_to_santa <- function(space_map) {
  
  centers <- unique(as.character(space_map))
  
  current <- "YOU"
  reached <- current
  dist <- 0
  
  while (!"SAN" %in% reached) {
    current <- map(current, get_nears) %>% 
      unlist() %>% unique() %>% setdiff(reached)
    reached <- c(reached,current)
    dist = dist + 1
  }
  
  dist-2
  
}

get_to_santa(space_map)

