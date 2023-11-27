# read line of input into a move (list)
read_input <- function(input) {
  dir <- input %>% str_extract("\\w")
  val <- input %>% str_extract("\\d+") %>% as.integer()
  move <- list(dir=dir,val=val)
  move
}

# create a rope with N knots, all starting at position 0,0
make_rope <- function(n_knots) {
  rope <- map(setNames(1:n_knots, c("H",1:(n_knots-1))),~c(x=0,y=0))
  rope
}

# measure distances between two knots
measure_hdistance <- function(k1, k2) {
  k1["x"]-k2["x"]
}
measure_vdistance <- function(k1, k2){
  k1["y"]-k2["y"]
}

measure_distance <- function(k1, k2) {
  hdistance <- measure_hdistance(k1, k2)
  vdistance <- measure_vdistance(k1, k2)
  sqrt(hdistance^2 + vdistance^2)
} #not used

# move the knot by 1 in the given direction
# return knot
move_k <- function(dir, knot) {
  if (dir=="L") {knot["x"] <- knot["x"] - 1}
  if (dir=="R") {knot["x"] <- knot["x"] + 1}
  if (dir=="U") {knot["y"] <- knot["y"] + 1}
  if (dir=="D") {knot["y"] <- knot["y"] - 1}
  knot
}

# move the tail by 1 to reach the head
# return tail
move_t <- function(Head, Tail) {
  hdistance <- measure_hdistance(Head, Tail)
  vdistance <- measure_vdistance(Head, Tail)

  if (abs(hdistance)<2 & abs(vdistance)<2) {return(Tail)}

  else if (hdistance==0) {Tail["y"] <- Tail["y"]+sign(vdistance)}

  else if (vdistance==0) {Tail["x"] <- Tail["x"]+sign(hdistance)}

  else {
    Tail["y"] <- Tail["y"]+sign(vdistance)
    Tail["x"] <- Tail["x"]+sign(hdistance)
  }
  return(Tail)
}

# move the head of the rope by 1 in the given direction, and then every following knot accordingly
# save the new position of final tail to the given enviroment
# return rope: (list of knots)
move_r <- function(dir, rope, env) {

  n_knots <- length(rope)

  # move the head
  rope[[1]] <- move_k(dir = dir, knot = rope[[1]])

  #repeat for each knot
  for (k in 2:n_knots) {
    Head = rope[[k-1]]
    Tail = rope[[k]]
    Tail = move_t(Head, Tail)
    rope[[k]] <- Tail

    if (k ==n_knots) {
      append_tail_position(tail = list(Tail=Tail), env = env)
    }
  }
  rope
}

# move entire rope according to the given move
# 1 step at the time
# save each new position of final tail to the given enviroment
# return rope
move_rope <- function(move, rope, env) {
  val = move$val

  for (i in 1:abs(val)) {
    rope <- move_r(dir = move$dir, rope = rope, env=env)
  }

  rope
}

# read and write tail positions in list in given env
get_tail_positions <- function(env) {
  rlang::env_get(nm = "tail_positions", env = env)
}
append_tail_position <- function(tail, env) {
  temp <- get_tail_positions(env = env)
  new <- c(temp, list(tail))
  rlang::env_poke(nm = "tail_positions", value = new, create = T, env = env)
}


# run full simulation
# and record tail positions to the given env
record_simulation <- function(input,
                              n_knots=2) {

  env <- rlang::env()
  rlang::env_poke(nm = "tail_positions", value = list(), create = T, env = env)

  rope <- make_rope(n_knots)

  for (x in input) {
    move <- read_input(x)
    rope <- move_rope(move = move, rope = rope, env = env)
  }

  output <- rlang::env_get(env, nm = "tail_positions") %>%
    purrr::reduce(rbind)

  output
}
