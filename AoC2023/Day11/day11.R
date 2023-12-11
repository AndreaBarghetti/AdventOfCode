library(tidyverse)
input <- read_lines("AoC2023/Day11/input.txt")

space = str_split(input, "", simplify = T)

# Part 1 ####
get_stars_coords <- function(space, expansion=1e6) {
  
  empty_cols = apply(space, 2, function(c){
    all(c==".")
  }) %>% which()
  
  empty_rows = apply(space, 1, function(c){
    all(c==".")
  }) %>% which()
  
  stars = which(space=="#", arr.ind = T)
  
  stars[,2] <- map_int(stars[,2], ~{
    add = sum(empty_cols < .x) * (expansion-1)
    .x+add
  })
  stars[,1] <- map_int(stars[,1], ~{
    add = sum(empty_rows < .x) * (expansion-1)
    .x+add
  })
  stars
}

stars <- get_stars_coords(space, expansion = 2)

dist(stars, method = "manhattan", diag = T, upper = F) %>% 
  as.integer() %>% sum()

# Part 2 ####
stars <- get_stars_coords(space, expansion = 1e6)

dist(stars, method = "manhattan", diag = T, upper = F) %>% 
  as.integer() %>% sum()
