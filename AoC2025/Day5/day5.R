library(tidyverse)
input <- read_lines("AoC2025/Day5/input.txt")

parse_ingredients = function(input) {
  sep = which(input=="")
  ingredients = input[(sep+1):length(input)]
  ingredients = as.numeric(ingredients)
}
parse_ranges = function(input) {
  sep = which(input=="")
  ranges = input[1:(sep-1)]
  ranges = map(ranges, ~str_split(.x, "-", simplify = T)%>%
                 as.numeric())
  ranges
}

ingredients = parse_ingredients(input)
ranges = parse_ranges(input)

# Part 1 ####
check_ingredient = function(ingredient, ranges) {
  
  for (r in ranges) {
    if (between(ingredient, r[1],r[2])) {return(T)}
  }
  return(F)
  
}
count_fresh_ingredients =  function(ingredients, ranges) {
  fresh=0
  for (i in ingredients) {
    if (check_ingredient(i, ranges)){
      fresh=fresh+1
    }
  }
  fresh
}
count_fresh_ingredients(ingredients, ranges)

# Part 2 ####
reduce_ranges = function(ranges) {
  
  mranges = ranges %>% 
    purrr::reduce(rbind)
  
  mranges = mranges[order(mranges[, 1], mranges[, 2]),]
  
  s = mranges[1,1]
  e = mranges[1,2]
  
  reduced_ranges = list(c(s,e))
  
  i=2
  while (i<nrow(mranges)) {
    cs = mranges[i,1]
    ce = mranges[i,2]
    if (cs <= s & ce > e) {e = ce} else {
      reduced_ranges = append(reduced_ranges, list(c(s,e)))
      i=i+1; next
    }
  }
  
  reduced_ranges %>% 
    purrr::reduce(rbind)
  
}

reduce_ranges(ranges) %>% 
  apply(1,diff) %>% sum() %>% 
  format(scientific=F)
  
