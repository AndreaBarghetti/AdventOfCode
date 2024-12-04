library(tidyverse)
input <- read_lines("AoC2024/Day4/input.txt")

mat = str_split(input,"", simplify = T)

# Part 1 ####
Xpos = which(mat=='X', arr.ind = T)

get_xmas = function(mat, xpos) {
  
  dirs = list(
    R = cbind(0,0:3),
    L = cbind(0,0:-3),
    U = cbind(0:-3,0),
    D = cbind(0:3,0),
    UR = cbind(0:-3,0:3),
    UL = cbind(0:-3,0:-3),
    DR = cbind(0:3,0:3),
    DL = cbind(0:3,0:-3)
  )
  # remove out of bounds
  if (xpos[1]<=3) {dirs[c('U','UR','UL')] = NULL}
  if (xpos[1]>=nrow(mat)-2) {dirs[c('D','DR','DL')] = NULL}
  if (xpos[2]<=3) {dirs[c('L','UL','DL')] = NULL}
  if (xpos[2]>=ncol(mat)-2) {dirs[c('R','UR','DR')] = NULL}
  
  sum(map_chr(dirs, ~{
    mat[sweep(.x,2,xpos,`+`)] %>% str_c(collapse = '')
  }) == 'XMAS')
  
}


apply(Xpos, 1, function(xpos) {
  get_xmas(mat, xpos)
}) %>% sum()


# Part 2 ####
Apos = which(mat=='A', arr.ind = T)

# remove out of bounds
Apos = Apos[Apos[,1]>1 & Apos[,1]<nrow(mat) & Apos[,2]>1  & Apos[,2]<ncol(mat),]

get_xmas2 = function(mat, apos) {

  diags = list(
    rbind(c(-1,-1),c(1,1)),
    rbind(c(1,-1),c(-1,1))
  )
  
  all(map_chr(diags, ~{
    mat[sweep(.x,2,apos,`+`)] %>% str_c(collapse = '')
  }) %in% c('MS','SM'))
  
}

apply(Apos, 1, function(pos) {
  get_xmas2(mat, pos)
}) %>% sum()
