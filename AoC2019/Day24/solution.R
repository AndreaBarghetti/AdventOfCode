library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 24, .fun = read_lines)

bugs = str_split(input,"", simplify = T)
bugs = ifelse(bugs=='#',1,0)

# part 1 ####

get_next_bugs <- function(bugs) {
  H <-apply(bugs, 1, function(x){
    lead(x, default = 0) + lag(x,default = 0)
  }) %>% t()
  V <- apply(bugs, 2, function(x){
    lead(x, default = 0) + lag(x,default = 0)
  }) 
  neigh_bugs = H+V
  die = ifelse(neigh_bugs!=1,0,1)
  new = ifelse((neigh_bugs==1|neigh_bugs==2)&bugs==0,1,0)
  bugs*die+new
}

display_bugs = function(bugs) {
  apply(ifelse(bugs,"#","."),1,paste0, collapse='') %>% cat(sep = '\n')
}

cycle_bugs = function(bugs) {
  
  states = character(100000)
  
  get_state = function(bugs) {
    str_c(bugs, collapse = "")
  }
  
  i<-0
  repeat {
    i <- i + 1
    state = get_state(bugs)
    if (state %in% states) {
      break
    } else {
      states[i] <- state
    }
    bugs = get_next_bugs(bugs)
  }
  display_bugs(bugs)
  2^(which(t(bugs)==1)-1) %>% sum()
}
cycle_bugs(bugs)

   
# part 2 ####
bugs = str_split(input,"", simplify = T)
# bugs = str_split(read_lines('Day24/test_input.txt'),"", simplify = T)
bugs = ifelse(bugs=='#',1,0)

mbugs <- list(bugs)

expand_mbugs <- function(mbugs) {
  # expand inner and outer levels
  empty_bugs <- matrix(0,5,5)
  
  if (sum(mbugs[[length(mbugs)]])!=0) {
    mbugs <- c(mbugs,list(empty_bugs))
  }
  if (sum(mbugs[[1]])!=0) {
    mbugs <- c(list(empty_bugs), mbugs)
  }
  mbugs
}

get_next_bugs = function(mbugs) {
  
  mbugs <- expand_mbugs(mbugs)
  
  empty_bugs <- matrix(0,5,5)
  
  pmap(
    list(mbugs,
         lag(mbugs, default = list(empty_bugs)),
         lead(mbugs,default = list(empty_bugs))
    ),
    
    function(bugs, inner_bugs, outer_bugs) {
      
      H <- apply(bugs, 1, function(x){
        lead(x, default = 0) + lag(x,default = 0)
      }) %>% t()
      V <- apply(bugs, 2, function(x){
        lead(x, default = 0) + lag(x,default = 0)
      }) 
      neigh_bugs = (H+V)
      
      # add from inner bugs
      neigh_bugs[2,3] = neigh_bugs[2,3] + sum(inner_bugs[1,])
      neigh_bugs[3,2] = neigh_bugs[3,2] + sum(inner_bugs[,1])
      neigh_bugs[3,4] = neigh_bugs[3,4] + sum(inner_bugs[,5])
      neigh_bugs[4,3] = neigh_bugs[4,3] + sum(inner_bugs[5,])
      
      # add from outer bugs
      neigh_bugs[1,] = neigh_bugs[1,] + outer_bugs[2,3]
      neigh_bugs[,1] = neigh_bugs[,1] + outer_bugs[3,2]
      neigh_bugs[,5] = neigh_bugs[,5] + outer_bugs[3,4]
      neigh_bugs[5,] = neigh_bugs[5,] + outer_bugs[4,3]
      
      die = ifelse(neigh_bugs!=1,0,1)
      new = ifelse((neigh_bugs==1|neigh_bugs==2)&bugs==0,1,0)
      bugs = bugs*die+new
      bugs[3,3] <- 0 
      return(bugs)
    }
  )
}

count_mbugs <- function(mbugs, time) {
  for (t in 1:time) {
    cat("time:", t)
    mbugs = get_next_bugs(mbugs)
    cat(", bugs: ", map_int(mbugs, sum) %>% sum(), "\n")
  }
}

count_mbugs(mbugs,200)
