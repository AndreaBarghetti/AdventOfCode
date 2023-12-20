library(tidyverse)
input <- read_lines("AoC2023/Day16/input.txt")

contraption <- str_split(input, "", simplify = T)

# Part 1 ####
track_light <- function(contraption, start_pos=cbind(1,1), start_dir="R") {
  
  nrows = nrow(contraption)
  ncols = ncol(contraption)
  dirs = list("R"=c(0,1),"L"=c(0,-1),"D"=c(1,0),"U"=c(-1,0))
  pos = start_pos
  dir = start_dir
  pos = list(pos=pos, dir=dir)
  
  light_count = matrix(0, nrow = nrows, ncol = ncols)
  cache = rlang::env()
  
  queue = list(pos)
  
  while(length(queue)>0) {
  
    pos = queue[[1]]
    queue = queue[-1]
    
    # already done cache 
    if (!is.null(cache[[toString(pos)]])) {next} else {cache[[toString(pos)]] <- T}
    # handle out of space coords
    if (any(pos$pos<=0) | pos$pos[1] >nrows | pos$pos[2]>ncols){next}
    
    light_count[pos$pos] <- light_count[pos$pos]+1
    
    tile = contraption[pos$pos]
    
    if (tile == ".") {
      pos$pos <- pos$pos + dirs[[pos$dir]]
      queue = c(queue,list(pos))
    }
    if (tile == "-") {
      if (pos$dir %in% c("L","R")) {
        pos$pos <- pos$pos + dirs[[pos$dir]]
        queue = c(queue,list(pos))
      } else if ( pos$dir %in% c("U","D")) {
        nextpos1 = nextpos2 = pos
        nextpos1$dir <- "L"
        nextpos1$pos <- nextpos1$pos +dirs[[nextpos1$dir]]
        nextpos2$dir <- "R"
        nextpos2$pos <- nextpos2$pos +dirs[[nextpos2$dir]]
        queue = c(queue,list(nextpos1),list(nextpos2))
      }
    }
    if (tile == "|") {
      if ( pos$dir %in% c("L","R")) {
        
        nextpos1 = nextpos2 = pos
        nextpos1$dir <- "U"
        nextpos1$pos <- nextpos1$pos +dirs[[nextpos1$dir]]
        nextpos2$dir <- "D"
        nextpos2$pos <- nextpos2$pos +dirs[[nextpos2$dir]]
        queue = c(queue,list(nextpos1),list(nextpos2))
        
      } else if( pos$dir %in% c("U","D")) {
        pos$pos <- pos$pos + dirs[[pos$dir]]
        queue = c(queue,list(pos))
      }
    }
    
    if (tile == "\\") {
      pos$dir <- recode(pos$dir, "L"="U","U"="L","R"="D","D"="R")
      pos$pos <- pos$pos + dirs[[pos$dir]]
      queue = c(queue,list(pos))
    }
    if (tile == "/") {
      pos$dir <- recode(pos$dir, "L"="D","D"="L","R"="U","U"="R")
      pos$pos <- pos$pos + dirs[[pos$dir]]
      queue = c(queue,list(pos))
    }
  }
  return(light_count)
}

sum(track_light(contraption)>0)

# Part 2 ####
# not optimized. takes 10min... but works
max_score = 0
for (r in 1:nrow(contraption)){
  score = sum(track_light(contraption, start_pos = cbind(r,1), start_dir = "R")>0)
  max_score = max(max_score,score)
  score = sum(track_light(contraption, start_pos = cbind(r,ncol(contraption)), start_dir = "L")>0)
  max_score = max(max_score,score)
}
for (c in 1:ncol(contraption)){
  score = sum(track_light(contraption, start_pos = cbind(1,c), start_dir = "D")>0)
  max_score = max(max_score,score)
  score = sum(track_light(contraption, start_pos = cbind(nrow(contraption),c), start_dir = "U")>0)
  max_score = max(max_score,score)
}


