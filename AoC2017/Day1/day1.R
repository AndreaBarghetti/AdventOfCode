library(tidyverse)
input <- read_lines('AoC2017/Day1/input.txt')

digits = str_split(input,"", simplify = T) %>% as.integer()

# Part 1 ----
sum(digits[digits==lead(digits, default = digits[1])])

# Part 2 ----
count_digits = function(digits) {
  L=length(digits)
  digits2 = digits[c((L/2+1):L,1:(L/2))]
  sum(digits[digits==digits2])
}

count_digits(digits)

