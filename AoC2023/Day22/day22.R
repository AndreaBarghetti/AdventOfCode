library(tidyverse)
input <- read_lines("AoC2023/Day22/input.txt")

parse_input <- function(input) {
  
  bricks <- str_extract_all(input, '\\d+', simplify = T) %>% 
    as.integer() %>% matrix(ncol=6)
  colnames(bricks) <- c("x1","y1","z1","x2","y2","z2")
  as_tibble(bricks) %>% 
    mutate(
      name = as.character(row_number())
    ) %>% 
    arrange(z1) %>% 
    mutate(x1=x1+1, y1=y1+1, x2=x2+1 ,y2=y2+1)
}

bricks = parse_input(input)

# Part 1 ####
get_layer = function(bricks, lv) {
  floor = matrix("", 10,10)
  
  bricks_lv = bricks %>% 
    filter(map2_lgl(z1,z2, function(z1,z2) {between(lv, z1,z2)}))
  if(nrow(bricks_lv)==0) {return(floor)}
  for (i in 1:nrow(bricks_lv)) {
    b=bricks_lv[i,]
    floor[b$y1:b$y2,b$x1:b$x2] <- b$name
  }
  return(floor)
}

get_supporting_bricks <- function(bricks) {
  map(1:nrow(bricks), function(row) {
    brick = bricks[row,]
    if(brick$z1==1){return('floor')}
    below = get_layer(bricks, brick$z1-1)
    above = get_layer(bricks, brick$z1)
    below[above==brick$name] %>% unique() %>% setdiff("")
  })
}

get_supported_bricks <- function(bricks) {
  map(1:nrow(bricks), function(row) {
    brick = bricks[row,]
    below = get_layer(bricks, brick$z2)
    above = get_layer(bricks, brick$z2+1)
    above[below==brick$name] %>% unique() %>% setdiff("")
  })
}

get_floor = function(bricks, lv) {
  floor = matrix(F, 10,10)
  
  bricks_lv = bricks %>% 
    filter(z2==lv)
  if(nrow(bricks_lv)==0) {return(floor)}
  for (i in 1:nrow(bricks_lv)) {
    b=bricks_lv[i,]
    floor[b$y1:b$y2,b$x1:b$x2] <- T
  }
  return(floor)
}

is_supported = function(bricks, brick){
  if(brick$z1==1){return(T)}
  floor = get_floor(bricks,brick$z1-1)
  any(floor[brick$y1:brick$y2, brick$x1:brick$x2])
}

fall_bricks <- function(bricks) {
  for (i in 1:nrow(bricks)) {
    brick <- bricks[i,]
    if (brick$z1==1){next}
    while(!is_supported(bricks, brick)) {
      brick$z1<-brick$z1-1
      brick$z2<-brick$z2-1
    }
    bricks[i,] <- brick
  } 
  return(bricks)
}

fallen_bricks = fall_bricks(bricks)

supporting_bricks = get_supporting_bricks(fallen_bricks)

total_bricks <- nrow(fallen_bricks)
unremovable_bricks <- supporting_bricks[map_int(supporting_bricks, length)==1] %>% 
  unlist() %>% unique() %>% setdiff("floor") %>% length()
total_bricks - unremovable_bricks


# Part 2 ####
supported_bricks = get_supported_bricks(fallen_bricks)

bricks_ls <- map2(supporting_bricks, supported_bricks, function(supporting, supported) {
  list(supporting=supporting,
       supported=supported)
}) %>% setNames(fallen_bricks$name)

count_falling_bricks <- function(bricks_ls) {
  sum(map_lgl(bricks_ls, function(b) {
    length(b$supporting)==0
  }))
}

remove_brick <- function(bricks_ls, name) {
 
  queue = collections::queue()
  queue$push(name) 
  
  while(queue$size()>0){
    removed = queue$pop()
    if (length(bricks_ls[[removed]]$supported) == 0) {next}
    for (s in bricks_ls[[removed]]$supported) {
      bricks_ls[[s]]$supporting <- setdiff(bricks_ls[[s]]$supporting, removed)
      if(length(bricks_ls[[s]]$supporting)==0){queue$push(s)}
    }
  }
  bricks_ls
}

map_dbl(names(bricks_ls), function(name) {
  remove_brick(bricks_ls,name) %>% count_falling_bricks()
}) %>% sum()
