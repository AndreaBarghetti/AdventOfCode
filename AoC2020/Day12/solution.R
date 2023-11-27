library(tidyverse)

input12 <- readLines("Day12/input12.txt")

# * - part 1 - *####
actions <- sapply(input12, function(x) {
  dir <- x %>% str_split("", n = 2)
  dir[[1]][1]
})

values <- sapply(input12, function(x) {
  dir <- x %>% str_split("", n = 2)
  dir[[1]][2]
}) %>% as.numeric()

move <- function(direction, value) {
  if (direction=="N") {y <<- y + value}
  else if (direction=="S") {y <<- y - value}
  else if (direction=="W") {x <<- x - value}
  else if (direction=="E") {x <<- x + value}
}

turn <- function(direction, value) {
  if (direction=="R") {dir <<- dir + (value/90)}
  else if (direction=="L") {dir <<- dir - (value/90)}
}

x <- 0
y <- 0
dirs <- rep(c("N","E","S","W"), 100)
dir <- 50

for (i in seq_along(actions)) {
  if (actions[i] %in% c("N","E","S","W")) {
    move(actions[i], values[i])
    
    print(paste("move", actions[i], dirs[dir],  values[i]))
  }
  
  else if (actions[i] %in% c("L","R")) {
    turn(actions[i], values[i])
    
    print(paste("turn",  dirs[dir]))
  }
  
  else if (actions[i] == "F") {
    move(dirs[dir], values[i])
    
    print(paste("move", actions[i], dirs[dir], values[i]))
  }
  
  print(paste(x,y))
}
sum(abs(x), abs(y))


# * - part 2 - *####
waypoint <- list(x=10,y=1)
x <- 0
y <- 0

move2 <- function(waypoint, value) {
  x <<- x + waypoint$x*value
  y <<- y + waypoint$y*value
}

move_waypoint <- function(direction, value) {
  x <- waypoint$x
  y <- waypoint$y
  
  if (direction=="N") {y <- y + value}
  else if (direction=="S") {y <- y - value}
  else if (direction=="W") {x <- x - value}
  else if (direction=="E") {x <- x + value}
  
  waypoint$x <<- x
  waypoint$y <<- y
}

turn2 <- function(direction, value) {
  
  x <- waypoint$x
  y <- waypoint$y
  
  turn <- value/90
  
  if (direction=="L") {turn <- 4 - turn}
  
  if (turn==1) {
    waypoint$x <<- y
    waypoint$y <<- -1*x
  }
  if (turn==2) {
    waypoint$x <<- -1*x
    waypoint$y <<- -1*y
  }
  if (turn==3) {
    waypoint$x <<- -1*y
    waypoint$y <<- x
  }
}

for (i in seq_along(values)) {
  print(input12[i])
  if (actions[i] %in% c("N","E","S","W")) {
    move_waypoint(actions[i], values[i])
    
    print(paste("move waypoint to ", paste(waypoint %>% unlist(), collapse = " ")))
  }
  
  else if (actions[i] %in% c("L","R")) {
    turn2(actions[i], values[i])
    
    print(paste("turn",actions[i],values[i], "waypoint:", paste(waypoint %>% unlist(), collapse = " ")))
  }
  
  else if (actions[i] == "F") {
    move2(waypoint, value = values[i])
    
    print(paste("move toward", paste(waypoint %>% unlist(), collapse = " "), values[i],"times" ))
  }
  
  print(paste(x,y))
}

sum(abs(x), abs(y))

