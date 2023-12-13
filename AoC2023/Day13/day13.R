library(tidyverse)

input <- read_lines("AoC2023/Day13/input.txt")

parse_input <- function(input) {
  seps = c(0,which(input==""))
  map2(seps, lead(seps,default = length(input)+1), ~{
    range = (.x+1):(.y-1)
    mirror = str_split(input[range],"", simplify = T)
    })
}

patterns <- parse_input(input)


# Part 1 ####
mat_mirror = function (x, axis = c("V", "H")) {
  match.arg(axis)
  nrow=nrow(x)
  ncol=ncol(x)
  if (axis == "V") {
    x <- t(apply(x, 1, rev))
  }
  if (axis == "H") {
    x <- apply(x, 2, rev)
  }
  return(matrix(x,nrow=nrow,ncol=ncol ))
}

score_simmetry = function(patterns) {
  
  map(patterns, function(pattern) {
    
    ncol=ncol(pattern)
    nrow=nrow(pattern)
    
    res_c = map_lgl(1:(ncol-1), function(pos) {
      ncols = min(pos,ncol-pos)
      cols1 = (1:pos)[(1:ncols)+max(0,pos-ncols)]
      cols2 = (pos+1:ncol)[1:ncols]
      side1 = matrix(pattern[,cols1], ncol = ncols)
      side2 = matrix(pattern[,cols2], ncol = ncols)
      side2 = mat_mirror(side2, axis = "V")
      
      identical(side1,side2)
    })
    
    if(sum(res_c)>0){
      res_c = (which(res_c))
    } else {res_c=0}
    
    res_r = map_lgl(1:(nrow-1), function(pos) {
      nrows = min(pos,nrow-pos)
      rows1 = (1:pos)[(1:nrows)+max(0,pos-nrows)]
      rows2 = (pos+1:nrow)[1:nrows]
      side1 = matrix(pattern[rows1,], nrow = nrows)
      side2 = matrix(pattern[rows2,], nrow = nrows)
      side2 = mat_mirror(side2, axis = "H")
      
      identical(side1,side2)
    })
    
    if(sum(res_r)>0){
      res_r = which(res_r)
    } else {res_r=0}
    
    
    return(list(c=res_c,r=res_r))
    
  })

}

scores = score_simmetry(patterns) 

sum(map_int(scores, ~sum(.x$c+.x$r*100)))

# Part 2 ####

# brute force
fix_smudge <- function(pattern) {
  score = score_simmetry(list(pattern))[[1]]
  for (r in 1:nrow(pattern)) {
    for (c in 1:ncol(pattern)) {
      pattern[r,c] <- ifelse(pattern[r,c]==".","#",".")
      alt_score = score_simmetry(list(pattern))[[1]]
      diff = map2(alt_score,score,setdiff) %>% unlist()
      if (sum(diff)>0) {
        return(map2(alt_score,score,setdiff)) 
      } else {pattern[r,c] <- ifelse(pattern[r,c]==".","#",".")}
    }
  }
  stop("no simmetry")
}

scores2 <- map(patterns,fix_smudge)

sum(map_int(scores2, ~{
  r = ifelse(length(.x$r)>0,.x$r*100,0)
  c = ifelse(length(.x$c)>0,.x$c,0)
  r+c
}))

