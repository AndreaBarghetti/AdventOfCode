library(tidyverse)

input <- readLines("Day11/input.txt")

source("Day11/functions.R")
raw_monkeys <- split(input[input!=""], cumsum(input=="")[input!=""])
round_order <- names(raw_monkeys)

# part1 ####
.monkeys_env <- list2env(map(raw_monkeys,read_monkey), envir = rlang::current_env())

play_rounds(20, round_order)

get_result()

# part2 ####
check_divs <- rlang::env_get_list(env = .monkeys_env, nms = round_order) %>%
  map(~.x$test) %>%
  map_chr(str_extract, "\\d+") %>%
  as.integer()

mcd <- prod(check_divs)

calm_down <- function(worry_lv) {
  if(worry_lv > mcd) {
    return(worry_lv %% mcd)
  } else if (worry_lv%%mcd==0) {
    return(mcd)
  } else {
    return(worry_lv)
  }
}

.monkeys_env <- list2env(map(raw_monkeys,read_monkey), envir = rlang::current_env())
play_rounds(10, round_order)

get_result()

