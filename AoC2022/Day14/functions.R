get_coordinates <- function(input) {
  map(input, function(input){
    str_extract_all(input, "\\d+,\\d+") %>%
      unlist() %>%
      map(function(x) {
        split <- str_split(x,pattern = ",", simplify = T)
        c(split[1],split[2]) %>% as.integer() %>% setNames(c("x","y"))
      })
  })
}

get_rocks_ranges <- function(coordinates) {
  xrange <- coordinates %>%
    map(function(coord) {
      map(coord, ~.x["x"])
    }) %>% unlist() %>% range()
  yrange <- coordinates %>%
    map(function(coord) {
      map(coord, ~.x["y"])
    }) %>% unlist() %>% range()
  list(xrange=xrange,
       yrange=yrange)
}

draw_rocks <- function(coordinates, map) {

  walk(coordinates, function(coordinates) {

    for (i in seq_along(coordinates[-1])) {
      start <- coordinates[[i]]
      end <- coordinates[[i+1]]
      map[start[2]:end[2], start[1]:end[1]] <- T
    }

    assign("map", value = map, envir = rlang::env_parent())
  })

  return(map)
}

create_map <- function(coordinates) {

  rock_ranges <- get_rocks_ranges(coordinates)

  map <- matrix(F,
                ncol = rock_ranges$xrange[2]+2,
                nrow = rock_ranges$yrange[2])

  map <- draw_rocks(coordinates, map)

  # crop map
  map <- map[,(rock_ranges$xrange[1]-2):(rock_ranges$xrange[2]+2)]

  # add top row
  map <- rbind(rep(F,ncol(map)),map)

  # mark center

  attributes(map)$center <- 500 - rock_ranges$xrange[1]+3

  map
}

try_fall <- function(pos, map) {
  fall_check <- map[pos[2]+1,(pos[1]-1):(pos[1]+1)] %>% setNames(c("L","D","R"))
  !fall_check[c("D","L","R")]
}

fall <- function(pos,map) {

  below <- try_fall(pos,map)

  while (any(below)) {
    shift <- (below*c(0,-1,1))[below]
    pos[2] <- pos[2]+1
    pos[1] <- pos[1] + shift[1]
    below <- try_fall(pos,map)
  }

  map[pos[2],pos[1]] <- T

  return(map)
}


make_sand <- function(map) {
  x=attributes(map)$center
  y=1
  c(x,y)
}

add_sand <- function(map) {

  pos <- make_sand(map)

  if(map[pos[2],pos[1]]) {message("top full");stop("top full")}

  map <- fall(pos, map )

  return(map)

}


fill_map <- function(map) {

  tryCatch(expr = {
    repeat {
      map <- add_sand(map)
    }
  },
  error = function(cond) {
    return(map)
  })
}

expand_map <- function(map) {
  sides <- matrix(F, nrow = nrow(map), ncol = max(1,(nrow(map)+50-ncol(map)%/%2)))
  wider_map <- cbind(sides,map,sides)
  #add floor
  floor <- rbind(rep(F,ncol(wider_map)),
                 rep(T,ncol(wider_map)))
  expanded_map <- rbind(wider_map,floor)
  attributes(expanded_map)$center <- attributes(map)$center + ncol(sides)
  expanded_map
}
