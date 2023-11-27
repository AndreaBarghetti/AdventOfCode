library(tidyverse)

input15 <- c(20,0,1,11,6,3)

# * - part 1 - *####
init <- input15

i <- length(init)
while(i < 2020) {
  if (!init[i]  %in% init[-i]) {init[i+1] <- 0}
  else {init[i+1] <- match(init[i],rev(init[-i]))}
  i <- i+1
}
init[2020]

# * - part 2 - *####
init <- input15

memory_game <- function(init, rounds) {
  taken_number <- init[-length(init)]
  next_n <- init[length(init)]
  
  index_numbers <- rep(0, rounds)
  index_numbers[taken_number+1] <- seq_along(taken_number)
  
  i <- length(init)
  
  while (i < rounds) {
    
    is_new <- index_numbers[next_n+1]==0
    
    if(is_new) {
      index_numbers[next_n+1] <- i
      next_n <- 0
    }
    else {
      x <- i - index_numbers[next_n+1]
      index_numbers[next_n+1] <- i
      next_n <- x
    }
    i<-i+1
  }
  return(next_n)
}

memory_game(init = init,
            rounds=3e7)

