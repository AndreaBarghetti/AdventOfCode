library(tidyverse)

source("day17/functions.R")

rocks <-  parse_rocks("Day17/rocks.txt")
winds <- parse_winds("Day17/input.txt")
# winds <- parse_winds("Day17/example.txt")

# part 1 ####

rock_count <- 0
wind_i <- 1
tower <- init_tower()

tower <- fall_n_rocks(tower, 2022, max = 100)

nrow(tower)+attr(tower, "trimmed")-1

# part 2 ####
heights <-c()

rock_count <- 0
wind_i <- 1
tower <- init_tower()
tower <- fall_n_rocks2(tower, 2022, max = 100)

increases <- diff(c(0,heights))

period <- find_period2(increases, 200)

# find a patter in increases
# plot(seq_along(heights), heights, type="s")
# plot(seq_along(increases), increases, type="s")

start_loop <- period["start"]+1
end_loop <- sum(period)
loop_length <- period["period"]

loop_increase <- sum(increases[(start_loop):(end_loop)])

N_loops <- (1000000000000-(start_loop-1))%/%loop_length
lefts <- (1000000000000-(start_loop-1))%%loop_length

format(
  (N_loops*loop_increase) +
    heights[start_loop-1] +
    sum(increases[start_loop:end_loop][1:lefts]),
  scientific = F
  )


# not great, as I had to figure out visually if there was a periodicity in the increases
# and check what was the period size in order to set the period search min_len
