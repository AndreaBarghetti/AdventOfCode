library(tidyverse)

input <- readLines("day8/input.txt")

# learned this solution from
# https://emilhvitfeldt.github.io/rstats-adventofcode/

# part 1 ####
mchar <- function(input) {
  purrr::map_int(input, function(x) {
    nchar(eval(parse(text = x)), type = "bytes")
  })
}

sum(nchar(input)-mchar(input))

# part 2 ####
mchar2 <- function(input) {
  stringi::stri_escape_unicode(input) %>% nchar + 2
}

sum(mchar2(input)-nchar(input))
