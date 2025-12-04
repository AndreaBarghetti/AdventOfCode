library(tidyverse)
banks <- read_lines("AoC2025/Day3/input.txt")
# Part 1 ####
max_bank = function(bank) {
  bank = str_split(bank, '',simplify = T) %>%
    as.integer()
  p1 = bank[1:(length(bank)-1)] %>%
    which.max()
  n2 = bank[(p1+1):length(bank)] %>%
    max()
  as.integer(paste0(bank[p1],n2))
}
map_dbl(banks,max_bank) %>% sum()
# Part 2 ####
max_bank2 = function(bank, n=12) {
  bank = str_split(bank, '',simplify = T) %>%
    as.integer()
  if (n==1) {return(max(bank))}
  p1 = bank[1:(length(bank)-n+1)] %>%
    which.max()
  nbank = bank[(p1+1):length(bank)]
  res = c(bank[p1], max_bank2(nbank, n = n-1))
  res = paste0(res, collapse = '') %>% as.numeric()
  return(res)
}
map_dbl(banks,max_bank2) %>% sum() %>%
  format(scientific = F)
