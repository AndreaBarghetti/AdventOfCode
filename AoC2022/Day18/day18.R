library(tidyverse)

source("Day18/functions.R")

rock <- parse_input("Day18/input.txt")

# part 1 ####
total_rock_surface <- get_surface_area(rock)

# part 2 ####

# all empty space coordinates
space <- rock %>%
  map(range) %>%
  imap(~{
    df <- tibble(val=.x[1]:.x[2])
    colnames(df)<-.y
    df
    }) %>%
  purrr::reduce(crossing) %>%
  anti_join(rock)

# start water in a corner
water <- tibble(x=1,y=1,z=1)
# spread wherever possible
water <- spread_water_all(water, rock)

# empty rock is whatever left
empty_rock <- space %>%
  anti_join(water)

# rock/water surface is total rock surface minus empty rock surface
empty_rock_surface <- get_surface_area(empty_rock)

total_rock_surface-empty_rock_surface
