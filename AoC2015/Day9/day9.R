library(tidyverse)

input <- read_lines("AoC2015/Day9/input.txt")


# part 1 ####

get_dists <- function(input) {
  dists = list()
  walk(input, function(input) {
    x = input %>% str_extract(c("^\\w+","(?<=\\s)[A-Z]\\w+","\\d+"))
    dist = as.integer(x[3])
    A = x[1]
    B = x[2]
    dists[[A]] <<- c(dists[[A]], setNames(dist,B))
    dists[[B]] <<- c(dists[[B]], setNames(dist,A))
    dists
  })
  dists
}
dists = get_dists(input)
places = names(dists)
all_paths = combinat::permn(places, fun = c)

follow_path = function(path, dists) {
  dist=0
  walk2(path[-length(path)],path[-1], function(from, to){
    dist <<- dist + dists[[from]][to]
  })
  dist
}
res = map_int(all_paths, follow_path, dists)

min(res)

# part 2 ####
max(res)

