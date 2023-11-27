library(tidyverse)

input3 <- readLines("Day3/input3.txt")

# * - part 1 - *####
slope <- str_replace_all(input3, c("\\."="0", "#"="1"))

width <- slope[1] %>% nchar()
length <- length(slope)
position <- list(row=1,col=1)
trees <- 0
direction <- list(right=3, down=1)

skyfun <- function(slope, direction, print=T) {
  
  width <- slope[1] %>% nchar()
  length <- length(slope)
  position <- list(row=1,col=1)
  trees <- 0
  
  while (position$row <= length) {
    hit <- slope[position$row] %>% str_sub(start = position$col, end = position$col) %>% as.integer()
    trees <- sum(trees, hit)
    if(print) {
      print(slope[position$row])
      print(c(rep(".", position$col-1), "^   ", trees) %>% paste(collapse = ""))
    }
    position$col <- position$col+direction$right
    position$row <- position$row+direction$down
    if (position$col >width) {position$col <- position$col - width}
    if (position$row >length) {
      return(trees)
      position <- list(row=1,col=1)
      trees <- 0
      break()
    }
  }
}

skyfun(slope, direction, print = T)

# * - part 2 - *####
directions <- list(list(right=1, down=1),
                   list(right=3, down=1),
                   list(right=5, down=1),
                   list(right=7, down=1),
                   list(right=1, down=2))

lapply(directions, function(direction) {
  skyfun(slope, direction, print=F)
}) %>% unlist() %>% prod()
