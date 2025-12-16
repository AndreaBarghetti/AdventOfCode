library(tidyverse)

input = 29000000

# part 1 ----

get_factors <- function(N) {

  limit <- floor(sqrt(N))
  
  i_sequence <- 1:limit
  
  divisors <- i_sequence[N %% i_sequence == 0]
  
  partners <- N / divisors
  
  all_factors <- unique(c(divisors, partners))
  
  return(all_factors)
}

get_presents = function(house_nr) {
  sum(get_factors(house_nr)*10)
}

find_first_house = function(limit, .f = get_presents) {

  i = 0
  presents=0
  while (presents<=limit) {
    i=i+1
    presents = do.call(.f, args = list(i))
  }
  i
    
}

find_first_house(input, get_presents)

# part 2 ----
get_presents2 = function(house_nr) {
  fctrs = get_factors(house_nr)
  sum(fctrs[house_nr/get_factors(house_nr)<=50]*11)
}

find_first_house(input, .f = get_presents2)



