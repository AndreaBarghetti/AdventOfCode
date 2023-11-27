library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 21, .fun = read_lines)
program <- input %>% str_split(",", simplify = T) %>% as.numeric()

source('Day21/intcode_computer.R')


# part 1 ####
droid <- Droid$new(program)

droid$run(save = F, print = T)

droid$mrun(save = T, print = T,
"NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
"
)

rev(droid$computer$outputs)[1]


# part 2 ####
droid <- Droid$new(program)

droid$run(save = F, print = T)

droid$mrun(save = T, print = T,
"NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT I T
NOT T T
OR F T
AND E T
OR H T
AND T J
RUN
"
)

rev(droid$computer$outputs)[1]


###
hole_types = map(1:4, ~ c("_","#")) %>% 
  purrr::reduce(crossing, .name_repair = "universal") %>% 
  apply(1, paste0, collapse = "")
