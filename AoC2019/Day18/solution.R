library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 18, .fun = read_lines)

map <- str_split(input,"", simplify = T)

display_map <- function(map) {
  map[map %in% letters] <-"Key"
  map[map %in% LETTERS] <-"Door"
  plot_matrix(map) +
    scale_fill_manual(values = c("#"="black","."="lightgray","Door"="red","Key"="orange","@"="blue"))
}

display_map(map)

# I'll start by filling with # all the dead ends in the map
# because they should never be walked and they'd only make the search harder
# I need to do this only once, so I don't care about efficiency
# this also remove doors that don't lead to anything useful
fill_dead_ends <- function(map) {
  
  moves = list(c(1,0),c(-1,0),c(0,1),c(0,-1))
  queue = list()
  
  # find first seeds
  for (r in 2:(nrow(map)-1)){
    for (c in 2:(ncol(map)-1)){
      pos <- cbind(r,c)
      value = map[pos]
      if (value=="." | (value %in% LETTERS)) {
        around_pos = map(moves, ~ pos+.x)
        around_vals = map_chr(around_pos, ~map[.x])
        if (sum(around_vals=="#")==3) {
          map[pos]<-"#"
          queue = c(queue, around_pos[around_vals!='#'])
        }
      }
    }
  }
  
  while (length(queue) > 0) {
    pos = queue[[1]]
    queue = queue[-1]
    
    value =  map[pos]
    if (value=="." | (value %in% LETTERS)) {
      around_pos = map(moves, ~pos+.x)
      around_vals = map_chr(around_pos, ~map[.x])
      if (sum(around_vals=="#")==3) {
        map[pos]<-"#"
        queue = c(queue, around_pos[around_vals!='#'])
      }
    }
    
  }
  map
}

# part 1 ####
Maze <- R6::R6Class('Maze', 
                    public = list(
                      map = NA,
                      entrance = NA,
                      keys = NA,
                      doors = NA,

                      initialize = function(map) {
                        self$map <- map
                        self$entrance <- which(map=="@", arr.ind = T)
                        keys <- map(setNames(letters,letters), function(l) {
                          which(map==l, arr.ind = T)
                        })
                        self$keys <- keys[map_lgl(keys,~length(.x)>0)]
                        self$doors <- map(setNames(LETTERS,LETTERS), function(l) {
                          which(map==l, arr.ind = T)
                        })
                      },
                      
                      use_key = function(key) {
                        door = toupper(key)
                        self$map[self$keys[[key]]] <- "."
                        self$map[self$doors[[door]]] <- "."
                        self$keys[[key]]<-NULL
                      },
                      
                      display = function() {
                        self$map %>% 
                          display_map() 
                      },
                      
                      fill_dead_ends = function() {
                        self$map <- fill_dead_ends(self$map)
                      }
                    )
)

maze <- Maze$new(map)

maze$fill_dead_ends()
maze$display()


# This function finds distances from a current position
# to all reachable FIRST next key
# return the distance matrix
get_key_dists <- function(map, pos) {
  moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1)) %>% 
    map(as.integer)
  
  # Create visited and add the starting point
  visited <- matrix(FALSE, nrow(map), ncol(map))
  visited[pos] <- TRUE
  visited[map=="#"] <- TRUE
  
  # Store the distances
  distances <- matrix(Inf, nrow(map), ncol(map))
  distances[pos] <- 0
  
  # BFS loop
  queue <- list(pos)
  
  while (length(queue) > 0) {
    pos <- queue[[1]]
    queue <- queue[-1]
    
    if (map[pos] %in% c(LETTERS,letters)) {
      next
    }
   
    # Explore neighboring cells
    for (move in moves) {
      next_pos <- pos + move
      
      # Check boundaries and walls
      if (any(next_pos<=0) | any(dim(map)-next_pos<0)) {next}
      
      if ((map[next_pos] != "#") && (!visited[next_pos])) {
        queue <- c(queue, list(next_pos))
        visited[next_pos] <- TRUE
        
        distances[next_pos] <- distances[pos] + 1
        
      }
    }
    
  }
  distances
}

# A runner explore the maze, get keys and open doors
Runner <- R6::R6Class("Runner", 
                      public = list(
                        
                        pos = NA,
                        maze=NA,
                        time=NA,
                        key_dists = NA,

                        initialize = function(maze, pos, time=0) {
                          self$maze <- maze$clone()
                          self$pos <- pos
                          self$time <- time
                        },
                        
                        locate_next_keys = function() {
                          key_distances = get_key_dists(self$maze$map, self$pos)[purrr::reduce(self$maze$keys, rbind)]
                          
                          res = map2(self$maze$keys, key_distances, function(pos,dist) {
                            list(pos=pos,dist=dist)
                          })
                          
                          self$key_dists <- res[key_distances<Inf]
                        },
                        
                        go_to_key = function(key) {
                          
                          key_dist <- self$key_dists[[key]]
                          self$time <- self$time + key_dist$dist
                          self$pos <- key_dist$pos
                          self$maze$use_key(key)
                        },
                        
                        get_state = function() {
                          label = paste( paste0(self$pos, collapse = ","),
                                         paste0(names(self$maze$keys) %>% sort(), collapse = ","),
                                         sep = ".")
                          
                          return(setNames(self$time, label))
                        },
                        
                        display = function() {
                          x= self$pos[2]
                          y= nrow(self$maze$map) - self$pos[1]+1
                          self$maze$display() +
                            annotate(geom="point", col="green",shape=18, size=3,x=x,y=y)
                        },
                        
                        print = function() {
                          cat("Runner:\n")
                          cat("time:",self$time,"keys left:", length(self$maze$keys), sep = " ")
                        }
                        
                      )
)

library(rlang)

# DFS approach
# or kind of,
# but it run for almost 1 hour to get to the right solution
# so probably not the right method
run_maze <- function(maze) {
  
  queue = list(Runner$new(maze, maze$entrance))
  
  states = rlang::env()
  
  best_time <- Inf
  
  while (length(queue) > 0) {
    
    current = queue[[1]]
    queue[1]<-NULL
    
    #check if time is over
    if(current$time >=best_time) {next}
    
    # check state
    state = current$get_state()
    if (is.null(states[[names(state)]])) {
      env_poke(env = states, nm = names(state) , value = state)
    } else {
      prev_state = states[[names(state)]]
      if (state >= prev_state) {next}
      if (state < prev_state) {
        env_poke(env = states, nm = names(state) , value = state)
      }
    }
    if (length(current$maze$keys)==0) {
      best_time <- min(best_time, current$time)
      message("New best time:", best_time)
      next
    }
    
    current$locate_next_keys()
    
    for (key in names(current$key_dists)) {
      
      new_runner <- current$clone(deep = T)
      new_runner$go_to_key(key)
      
      #this can make a big difference
      # which one is faster?
      # queue <- c(queue,list(new_runner))
      queue <- c(list(new_runner), queue)
      
    }
  }
  best_time
}

run_maze(maze)


# part 2 ####
update_map <- function(map) {
  entrance = which(map=="@",arr.ind = T)
  new <- matrix("#", 3,3)
  new[c(1,3,7,9)] <- "@"
  map[entrance[1]+(-1:1),entrance[2]+(-1:1)] <- new
  map
}
map2<-update_map(map)

maze2 <- Maze$new(map2)

maze2$fill_dead_ends()
maze2$display()


Runners <- R6::R6Class("Runners", 
                      public = list(
                        
                        pos = NA,
                        maze = NA,
                        time = NA,
                        key_dists = NA,
                        key_robot_pairs=NA,
                        
                        initialize = function(maze, pos, time=0) {
                          self$maze <- maze$clone()
                          self$pos <- pos
                          self$time <- time
                          self$assign_keys()
                        },
                        
                        assign_keys = function() {
                          
                          temp = self$maze$map %>% str_replace_all("[A-Za-z]",".") %>% 
                            matrix(nrow(self$maze$map), ncol(self$maze$map))
                          key_robot_pairs = apply(self$pos, 1, function(pos) {
                            get_key_dists(temp, matrix(pos,nrow=1))[purrr::reduce(self$maze$keys, rbind)]
                          }, simplify = T) %>% 
                            apply(1,function(x){which(x<Inf)}) %>% setNames(names(self$maze$keys))
                          self$key_robot_pairs <- key_robot_pairs
                          
                        },
                        
                        
                        locate_next_keys = function() {
                          
                          key_distances = map(1:4, function(robot) {
                            get_key_dists(self$maze$map, matrix(self$pos[robot,],1))[purrr::reduce(self$maze$keys, rbind)]
                          }) %>% pmap_dbl(min)
                          
                          res = map2(self$maze$keys, key_distances, function(pos,dist) {
                            list(pos=pos,dist=dist)
                          })
                          
                          self$key_dists <- res[key_distances<Inf]
                        },
                        
                        go_to_key = function(key) {
                          
                          robot = self$key_robot_pairs[key]
                          
                          key_dist <- self$key_dists[[key]]
                          self$time <- self$time + key_dist$dist
                          self$pos[robot,] <- key_dist$pos
                          self$maze$use_key(key)
                        },
                        
                        get_state = function() {
                          label = paste( paste0(self$pos, collapse = ","),
                                         paste0(names(self$maze$keys) %>% sort(), collapse = ","),
                                         sep = ".")
                          
                          return(setNames(self$time, label))
                        },
                        
                        display = function() {
                          x= self$pos[2]
                          y= nrow(self$maze$map) - self$pos[1]+1
                          self$maze$display() +
                            annotate(geom="point", col="green",shape=18, size=3,x=x,y=y)
                        },
                        
                        print = function() {
                          cat("Runners\n")
                          cat("time:",self$time,"\n")
                          cat("keys left:", length(self$maze$keys),"\n")
                          cat("positions:", t(self$pos),"\n")
                        }
                        
                      )
)

self <- Runners$new(maze2, maze2$entrance)

# then just replace Runner with Runners
run_maze2 <- function(maze) {
  
  queue = list(Runners$new(maze, maze$entrance))
  
  states = rlang::env()
  
  best_time <- Inf
  
  while (length(queue) > 0) {
    
    current = queue[[1]]
    queue[1]<-NULL
    
    #check if time is over
    if(current$time >=best_time) {next}
    
    # check state
    state = current$get_state()
    if (is.null(states[[names(state)]])) {
      env_poke(env = states, nm = names(state) , value = state)
    } else {
      prev_state = states[[names(state)]]
      if (state >= prev_state) {next}
      if (state < prev_state) {
        env_poke(env = states, nm = names(state) , value = state)
      }
    }
    if (length(current$maze$keys)==0) {
      best_time <- min(best_time, current$time)
      message("New best time:", best_time)
      next
    }
    
    current$locate_next_keys()
    
    for (key in names(current$key_dists)) {
      
      new_runner <- current$clone(deep = T)
      new_runner$go_to_key(key)
      
      queue <- c(list(new_runner), queue)
      
    }
  }
  best_time
}

run_maze2(maze2)
