library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 4, .fun = read_lines)
input <- str_split(input,"-", simpl=T) %>% as.integer()
input <- input[1]:input[2]

# Part 1 ####

# never decrease L to R
is_increasing <- function(pw) {
  str_split(pw, simplify = T,"") %>%
    apply(1, function(x) {
      all(x==sort(x))
    })
}

check_rules <- function(pw,L,R) {
  R1 = nchar(pw) == 6
  R2 = between(pw, L, R)
  R3 = str_detect(pw,'(\\d)\\1')
  R4 = is_increasing(pw)
  R1&R2&R3&R4
}

valid <- check_rules(input, min(input), max(input))

sum(valid)

# Part 2 ####
R5 <- function(pw) {
  x <- pw %>% str_extract_all("(\\d)\\1+", simplify = T) %>% 
    nchar() %>% 
    apply(1, function(x) {2 %in% x})
  x
}
valid <- valid&R5(input)

sum(valid)
