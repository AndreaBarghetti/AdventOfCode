library(tidyverse)

input <- read_lines("AoC2015/Day15/input.txt")

parse_properties <- function(input) {
  ingr <- str_extract(input, "^\\w+")
  values = str_extract_all(input, "-*\\d+", simplify = T)
  values = matrix(as.numeric(values), nrow(values))
  rownames(values)<- ingr
  values
}

properties <- parse_properties(input)

# part 1 ####
get_score <- function(Sp,Bu,Ch, properties) {
  
  coefs = c(Sp,Bu,Ch)
  if(sum(coefs)>100){return(0)}
  coefs = c(coefs, 100-sum(coefs))
  
  score = t(properties) %*% coefs %>% head(4) %>% pmax(0) %>% prod()
  
  score
}

#brute force try all combos
find_best = function(properties) {
  best = 0 
  for (x in 1:100) {
    for (y in 1:100) {
      for ( z in 1:100) {
        score = get_score(x,y,z, properties)
        if (score > best) {
          best = score
          best_vals = c(x,y,z)
        }
      }
    }
  }
  return(list(vals = best_vals,
              score = best))
}
find_best(properties)

# part 2 ####
get_score <- function(Sp,Bu,Ch, properties) {
  
  coefs = c(Sp,Bu,Ch)
  if(sum(coefs)>100){return(0)}
  coefs = c(coefs, 100-sum(coefs))
  
  score = t(properties) %*% coefs  %>% pmax(0) %>% head(4) %>%  prod()
  calories = t(properties) %*% coefs  %>% pmax(0) %>% pluck(5)
  if (calories != 500) {return(0)}
  score
}

find_best(properties)

