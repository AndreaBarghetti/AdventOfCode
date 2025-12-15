library(tidyverse)

input = read_lines("AoC2015/Day18/input.txt")

map = str_split(input,"", simplify = T) %>% 
  apply(2,`==`,"#")

# part 1 ----
game_of_life_step <- function(m) {
  
  if (!is.integer(m)) storage.mode(m) <- "integer"
  nr <- nrow(m); nc <- ncol(m)
  
  # Zero-pad once
  pad <- matrix(0L, nr + 2L, nc + 2L)
  pad[2L:(nr + 1L), 2L:(nc + 1L)] <- m
  
  # 8-neighbor counts via slicing (very fast)
  nbh <- pad[1L:nr,           1L:nc]           +
    pad[1L:nr,           2L:(nc + 1L)]    +
    pad[1L:nr,           3L:(nc + 2L)]    +
    pad[2L:(nr + 1L),    1L:nc]           +
    pad[2L:(nr + 1L),    3L:(nc + 2L)]    +
    pad[3L:(nr + 2L),    1L:nc]           +
    pad[3L:(nr + 2L),    2L:(nc + 1L)]    +
    pad[3L:(nr + 2L),    3L:(nc + 2L)]
  
  alive <- (m == 1L)
  # Rules: survive with 2 or 3; birth with exactly 3
  next_m <- ( (alive & (nbh == 2L | nbh == 3L)) | (!alive & nbh == 3L) )
  storage.mode(next_m) <- "integer"
  next_m
}

print_map <- function(m) {
  apply(m,1, function(x){
    cat(ifelse(x,'#','.'),'\n', sep = "")
  })
  cat('\n')
  return(invisible())
}

lights_after_steps = function(map, steps=100) {
  
  for (i in 1:steps){
    map = game_of_life_step(map)
    # print_map(map)
  }
  sum(map)
  
}

lights_after_steps(map, 100)

# part 2 ----
fix_corners = function(map) {
  dims = dim(map)
  map[rbind(c(1,1),c(1,dims[2]),c(dims[1],1),c(dims))] = TRUE
  map
}

map2 = fix_corners(map)

game_of_life_step <- function(m) {
  
  if (!is.integer(m)) storage.mode(m) <- "integer"
  nr <- nrow(m); nc <- ncol(m)
  
  # Zero-pad once
  pad <- matrix(0L, nr + 2L, nc + 2L)
  pad[2L:(nr + 1L), 2L:(nc + 1L)] <- m
  
  # 8-neighbor counts
  nbh <- 
    pad[1L:nr,           1L:nc]           +
    pad[1L:nr,           2L:(nc + 1L)]    +
    pad[1L:nr,           3L:(nc + 2L)]    +
    pad[2L:(nr + 1L),    1L:nc]           +
    pad[2L:(nr + 1L),    3L:(nc + 2L)]    +
    pad[3L:(nr + 2L),    1L:nc]           +
    pad[3L:(nr + 2L),    2L:(nc + 1L)]    +
    pad[3L:(nr + 2L),    3L:(nc + 2L)]
  
  alive <- (m == 1L)
  # Rules: survive with 2 or 3; birth with exactly 3
  next_m <- ( (alive & (nbh == 2L | nbh == 3L)) | (!alive & nbh == 3L) )
  next_m = fix_corners(next_m)
  storage.mode(next_m) <- "integer"
  next_m
}

lights_after_steps(map2, 100)

# animation ----
library(animation)

plot_map <- function(m) {
  image(
    t(m[nrow(m):1, ]),
    col = c("black", "white"),
    axes = FALSE,
    useRaster = TRUE
  )
}

save_animation <- function(map) {
  animation::saveGIF({
    for (i in 1:100) {
      map = game_of_life_step(map)
      plot_map(map)
    }
  },
  interval=.1,
  loop=T, 
  movie.name = "day18.gif",
  ani.width = 500,
  ani.height = 500,
  clean = T)
}

save_animation(map)

