library(tidyverse)

input <- read_lines("AoC2025/Day2/test")

ranges = str_extract_all(input, "\\d+-\\d+", simplify = T) %>%
  map(str_split,"-", simplify=T)

# Part 1 ####
find_reps = function(range) {
  # browser()
  start = range[1]; end = range[2]
  ls = nchar(start); le = nchar(end)
  if (ls==1) {start='10'; return(find_reps(c(start,end)))}
  if (ls%%2==1) {start = format(10^(ls), scientific = F)}
  if (le%%2==1) {end = format(10^(le-1)-1,  scientific = F)}
  if (start>end) {stop('invalid range')}
  ls = nchar(start); le = nchar(end)
  s1 = str_sub(start,1, ls/2) %>% as.numeric()
  s2 = str_sub(start,ls/2+1,-1) %>% as.numeric()
  e1 = str_sub(end,1, le/2) %>% as.numeric()
  e2 = str_sub(end,le/2+1,-1) %>% as.numeric()
  test = paste0(s1,s1) %>% as.numeric()
  if (test<as.numeric(start)) {s1=s1+1}
  test = paste0(s1,s1) %>% as.numeric()
  invalids=c()
  while(test<=as.numeric(end)) {
    invalids=c(invalids,test)
    s1=s1+1
    test = paste0(s1,s1) %>% as.numeric()
  }
  invalids
}
map(ranges, find_reps) %>% unlist() %>% sum() %>% format(scientific = F)

# Part 2 ####
w_search = function(p, range) {
  start = as.numeric(range[1]); end = as.numeric(range[2])
  w = p
  res = list()
  while(as.numeric(w)<=end) {
    w=paste0(w,p)
    if (as.numeric(w)>=start & as.numeric(w)<=end) {
      # message(w)
      res = append(res, w)
    }
  }
  res
}
factors <- function(n) {
  small <- which(n %% 1:floor(sqrt(n)) == 0)
  large <- n / small
  facs <- unique(c(small, large)) %>% sort()
  setdiff(facs, n)
}
find_reps2 = function(range) {
  start = range[1]; end = range[2]
  ls = nchar(start); le = nchar(end)
  result = list()
  fs = map(ls:le, factors) %>% unlist() %>% unique()
  for(f in fs) {
    min = paste0('1', paste0(rep('0',f-1), collapse = '')) %>% as.integer()
    max = paste0(rep('9',f),collapse = '') %>% as.integer()
    for (i in min:max) {
      result = append(result, w_search(i, range))
    }
  }
  result = result %>% unlist(recursive = T) %>% unique()
  result
}
map_dbl(ranges, ~ {
  find_reps2(.x) %>% as.numeric() %>% sum()
}) %>%
  sum()
