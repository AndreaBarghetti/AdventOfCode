library(tidyverse)

input <- read_lines("AoC2025/Day9/input.txt")

tiles = str_split(input, ',') %>% 
  map(as.integer)

# Part 1 ####
max_area = function(tiles) {
  max_area = 0 
  for (t1 in tiles) {
    for (t2 in tiles) {
      area = prod(abs(t1-t2)+1)
      if (area > max_area) {max_area = area}
    }
  }
  max_area
}
max_area(tiles)
  
# Part 2 ####
library(sf)

tiles_m = do.call('rbind', tiles)
tiles_m = rbind(tiles_m, tiles_m[1,])
polygon = st_polygon(list(tiles_m))

check_rectangle = function(t1,t2, polygon) {
  
  # rectangle
  rect = st_polygon(list(rbind(t1, c(t1[1],t2[2]), t2, c(t2[1],t1[2]), t1)))

  sf::st_within(rect, polygon, sparse = F)
  
}

max_area = function(tiles, polygon) {
  max_area = 0 
  for (t1 in tiles) {
    for (t2 in tiles) {

      if (!check_rectangle(t1, t2, polygon)) {
        next
      }
      
      area = prod(abs(t1-t2)+1)
      if (area > max_area) {max_area = area}
    }
  }
  max_area
}

max_area(tiles, polygon)
