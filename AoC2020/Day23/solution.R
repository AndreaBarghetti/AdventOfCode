library(tidyverse)

cups <- c(1,2,3,4,8,7,5,9,6)

# * - part 1 - *####

play_cups <- function(cups, rounds) {
  
  if (!exists("current")) {current<- cups[1]}
  size <- length(cups)
  
  for (i in 1:rounds) {
    print(i)
    pos <- which(current==cups)
    expand <- rep(cups, 2)
    next_cups <- expand[(pos+1):(pos+3)]
    left <- cups[!cups %in% next_cups]
    new_cup <- max(left[left < current])
    if (new_cup == -Inf) {new_cup <- max(left)}
    new_order <- c(left[1:which(left==new_cup)], next_cups, left[(which(left==new_cup)+1):length(left)])
    # fix for when new cup is the last one
    if (any(is.na(new_order))) {
      new_order <- c(left[1:which(left==new_cup)], next_cups)
    }
    current <- rep(new_order,2)[(which(new_order==current))+1]
    cups <- new_order
  }
  return(cups)
}

movedcups = play_cups(cups = cups, rounds = 100)

movedcups[c((which(movedcups==1)+1):length(cups),1:(which(movedcups==1)-1))] %>% 
  paste0(collapse = '')

# * - part 2 - *####

