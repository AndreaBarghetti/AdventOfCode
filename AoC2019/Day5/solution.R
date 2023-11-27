library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 5, .fun = read_lines)
program <- str_split(input,",", simplify = T) %>%  as.integer()

# Part 1 ####
read_instruction <- function(x) {
  (x +100000) %>% str_sub(c(5,4,3,2), c(6,4,3,2)) %>% 
    as.integer() %>% setNames(c("op", "m1","m2","m3"))
}

read_code <- function(program, input=1) {
  
  # so I don't go crazy adding +1 to interpret positions
  x = zeroindex(program)
  
  i <- 0 
  outputs <- integer(0)
  
  repeat {
    
    instruction <- read_instruction(x[i])
    
    p1 <- x[i+1]
    p2 <- x[i+2]
    p3 <- x[i+3]
    
    op <- instruction['op']
    if (instruction['m1']==0) {
      v1 <- x[p1]
    } else if (instruction['m1']==1) {
      v1 <- p1
    } else {stop("wrong mode")}
    
    if (instruction['m2']==0) {
      v2 <- x[p2]
    } else if (instruction['m2']==1) {
      v2 <- p2
    } else {stop("wrong mode")}
    
    if (instruction['m3']==0) {
      v3 <- x[p3]
    } else if (instruction['m3']==1) {
      v3 <- p3
    } else {stop("wrong mode")}
    
    if (op == 1) {
      x[p3] <- v1+v2
      i <- i + 4
    } else if (op == 2) {
      x[p3] <- v1*v2
      i <- i + 4
    } else if (op == 3) {
      x[p1] <- input
      i <- i + 2
    } else if (op == 4) {
      outputs <- c(outputs, v1)
      i <- i + 2
    } else if (op == c(99)) {
      return(outputs)
    } else {
      stop(paste(x[i], "Error: ",op,"is not a valid op" ))
    }
  }
}

output <- read_code(program, 1)


# part 2 ####
read_code2 <- function(program, input=1) {
  
  # so I don't go crazy adding +1 to interpret positions
  x = zeroindex(program)
  
  i <- 0 
  outputs <- integer(0)
  
  repeat {
    
    instruction <- read_instruction(x[i])
    
    p1 <- x[i+1]
    p2 <- x[i+2]
    p3 <- x[i+3]
    
    op <- instruction['op']
    
    if (instruction['m1']==0) {
      v1 <- x[p1]
    } else if (instruction['m1']==1) {
      v1 <- p1
    } else {stop("wrong mode")}
    
    if (instruction['m2']==0) {
      v2 <- x[p2]
    } else if (instruction['m2']==1) {
      v2 <- p2
    } else {stop("wrong mode")}
    
    if (instruction['m3']==0) {
      v3 <- x[p3]
    } else if (instruction['m3']==1) {
      v3 <- p3
    } else {stop("wrong mode")}
    
    if (op == 1) {
      x[p3] <- v1+v2
      i <- i + 4
    } else if (op == 2) {
      x[p3] <- v1*v2
      i <- i + 4
    } else if (op == 3) {
      x[p1] <- input
      i <- i + 2
    } else if (op == 4) {
      outputs <- c(outputs, v1)
      i <- i + 2
    } else if (op == 5) {
      if (v1 != 0) {i <- v2} else {i<-i+3}
    } else if (op == 6) {
      if (v1 == 0) {i <- v2} else {i<-i+3}
    } else if (op == 7) {
      if (v1 < v2) {x[p3] <- 1} else {x[p3] <- 0}
      i <- i + 4
    } else if (op == 8) {
      if (v1 == v2) {x[p3] <- 1} else {x[p3] <- 0}
      i <- i + 4
    } else if (op == c(99)) {
      return(outputs)
    } else {
      stop(paste(x[i], "Error: ",op,"is not a valid op" ))
    }
  }
}

read_code2(program,5)
