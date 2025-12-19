library(tidyverse)

# pkgs=c(1:5,7:11) # TEST
pkgs = read_lines('AoC2015/Day24/input.txt') %>% 
  as.integer()

# part 1 ----
library(lpSolve)

get_pkg_ids = function(pkgs, N=0, grps=3, num.bin.solns=1) {

  L = length(pkgs)
  const.mat1 = matrix(0, nrow = grps, ncol = grps*L)
  for (i in 1:grps) {
    const.mat1[i,((i-1)*L+1):(i*L)]=pkgs
  }
  
  const.rhs1 = rep(sum(pkgs)/grps,grps)
  
  e = vector(mode = 'integer', length = grps*L)
  const.mat2 = map(1:L, function(i) {
    e[i+(0:(grps-1))*L]=1
    e
  }) %>% do.call(rbind,.)
  
  const.rhs2 = rep(1,L)
  
  const.mat = rbind(const.mat1,const.mat2)
  const.rhs = c(const.rhs1, const.rhs2)
  const.dir <- c(rep("=", nrow(const.mat)))
  
  objective.in = rep(c(1,rep(0,grps-1)),each=L)
  
  if(N>0) {
    const.mat = rbind(const.mat, c(rep(1,L),rep(0,(grps-1)*L)))
    const.rhs = c(const.rhs, N)
    const.dir <- c(const.dir, "=")
  }
  
  res <- lp(direction = "min", 
            objective.in = objective.in, 
            const.mat = const.mat, 
            const.dir = const.dir, 
            const.rhs = const.rhs, 
            all.bin = TRUE,
            num.bin.solns = num.bin.solns)
  
  res$solution
}

# tested manually how many solutions to return before having duplicates: 5
get_best_qe = function(pkgs, grps, num.bin.solns=5) {
  
  s1 = get_pkg_ids(pkgs, grps = grps)
  N = sum(s1[1:length(pkgs)])
  sols = get_pkg_ids(pkgs, N = N, grps = grps, num.bin.solns = num.bin.solns)
  sols=sols[1:(length(sols)-1)]
  msols = matrix(sols, nrow = num.bin.solns, byrow = T)
  msols=msols[,1:length(pkgs)] %>% unique()
  
  apply(msols,1,function(x) {
    
    prod(pkgs[x==1])
    
  }) %>% min()
  
}

get_best_qe(pkgs, grps = 3, num.bin.solns = 5)

# part 2 ----
get_best_qe(pkgs, grps = 4, num.bin.solns = 5)
