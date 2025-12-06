library(tidyverse)

input <- read_lines("AoC2025/Day6/input.txt")

parse_numbers = function(input) {
  
  nums = input[1:(length(input)-1)] %>% 
    str_extract_all("\\d+", simplify = T) %>%
    apply(2,as.numeric, simplify = F)
  rev(nums)

}

parse_operators = function(input) {
  op = input[length(input)] %>% 
    str_extract_all("\\*|\\+", simplify = T)
  rev(op)
}

# Part 1 ####
get_grandtotal = function(numbers, operators) {

  gt = map2_dbl(numbers, operators, function(n, o) {
    switch(o, 
                `+` = sum(n), 
                `*` = prod(n))
    
  }) %>% sum()
  
  gt
}

numbers = parse_numbers(input)
operators = parse_operators(input)

get_grandtotal(numbers, operators) %>% 
  format(scientific = F)

# Part 2 ####
parse_numbers2 = function(input) {
  
  nums = input[1:(length(input)-1)]
  nums = str_split(nums,'', simplify = F)
  l=length(nums[[1]])
  Ns = c()
  numbers = list()
  for (i in l:1) {
    
    chr = map_chr(nums,i)
    if (all(chr==" ")) {
      numbers = append(numbers,list(Ns))
      Ns=c()
      next
    }
    Ns = c(Ns,paste0(chr, collapse = '') %>% as.integer())
  }
  numbers = append(numbers,list(Ns))
  numbers
}

numbers = parse_numbers2(input)
operators = parse_operators(input)

get_grandtotal(numbers, operators) %>% 
  format(scientific = F)
