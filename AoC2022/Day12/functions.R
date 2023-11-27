# convert map to numeric
reset_map <- function(cmap) {
  map <- matrix(cmap %>% recode(!!!c(setNames(1:26, letters),"S"=1,"E"=26)), nrow = nrow(cmap))
  attributes(map)$edge <- Inf
  map
}

make_posmap <- function(map) {
  posmap <- matrix(seq_along(map), nrow = nrow(map))
  posmap
}

# get the coordinate of the next position,
# given the current position and a direction
# also needs a position map
next_pos <- function(pos,dir=c("U","D","L","R"), posmap) {
  current <- which(posmap==pos,arr.ind = TRUE)
  new <- current
  if (dir=="U") {new[1] <- new[1]-1}
  if (dir=="D") {new[1] <- new[1]+1}
  if (dir=="L") {new[2] <- new[2]-1}
  if (dir=="R") {new[2] <- new[2]+1}
  res <- tryCatch({posmap[new[1],new[2]]}, error=function(cond) {return(0)})
  res
}

# get the value of the next position,
# given the current position, a direction, and the map
get_next <- function(pos, dir, map, posmap) {
  res <- map[next_pos(pos, dir,posmap)]
  if(length(res)==0) {res <- attributes(map)$edge}
  res
}

#get_next(1,"U",map)
#get_next(5,"R",map)
#get_next(1,"U",checkmap)
#get_next(1,"R",checkmap)

# get the value of one position,
# given the position and the map
get_pos <- function(pos,map) {
  res <- map[pos]
  if(length(res)==0) {res <- attributes(map)$edge}
  res
}

# check all position around
# return all the directions it is possible to move towards
check_space <- function(pos,map,posmap) {
  dirs <- c("U","D","L","R") %>% setNames(.,.)
  current <- get_pos(pos,map)
  values <- map_dbl(dirs, function(dir) {get_next(pos, dir,map,posmap)})
  # c_values <- map_lgl(dirs, function(dir) {get_next(pos, dir, checkmap)})

  moves <- (values - current) < 2
  # avoid getting back into "a" zones
  #moves <- moves & (values !=1)

  # avoid choosing already checked map fails
  # moves <- moves & (!c_values)

  if (all(!moves)) {return(vector("character", length = 0))}

  return(names(moves[moves]))
}

# check all directions it is possible to move to
# and return their coordinates
next_pos_all <- function(pos,map,posmap) {
  dirs <- check_space(pos,map,posmap)
  map_dbl(dirs, next_pos, pos=pos,posmap=posmap)
}

get_cmap_info <- function(cmap) {
  # get start and end position
  start <- which(cmap == "S",arr.ind = FALSE)
  end <- which(cmap == "E",arr.ind = FALSE)
  map <- reset_map(cmap)
  posmap <- make_posmap(cmap)
  list(map=map,
       posmap=posmap,
       start=start,
       end=end)
}

# get a map with the min distance of every position
# from the start
# terminate scan when end position is found
get_dist_map <- function(cmap) {

  info <- get_cmap_info(cmap)

  map <- info$map
  posmap <- info$posmap
  start <- info$start
  end <- info$end

  # do a full scan first
  full_scan <- map(seq_along(map), next_pos_all, map=map, posmap=posmap)

  step <- 0
  dist_map <- ifelse(map==0,0,0)
  positions <- start
  explored <- positions

  repeat {
    #get all new possible positions
    positions <- full_scan[positions] %>% unlist() %>% unique() %>% setdiff(explored)
    # marked them as explored
    explored <- c(explored,positions)
    # increase step
    step <- step+1
    #mark all current position with current step
    dist_map[positions] <- step
    # and repeat...
    if(end %in% positions) {break}
  }
  dist_map
}

