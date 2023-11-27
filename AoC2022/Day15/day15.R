library(tidyverse)

input <- read_lines("Day15/input.txt")
source("Day15/functions.R")

# part 1 ####
coordinates <- read_input(input)

coord_df <- get_coord_df(coordinates)

set_y <- 2000000

covered_positions_x <- scan_row(set_y = set_y,coord_df)

range(covered_positions_x) %>% diff()

# part 2 ####
target_range=c(0,4000000)

sensors_df <- coord_df %>%
  filter(center=="sensor")

sensors_plus <- pmap(sensors_df, function(x,y,dist,...) {
  c(x,y,dist+1)
})

intersactions_df <- imap_dfr(sensors_plus, function(sensor1,i) {
  map_dfr(sensors_plus[-i], function(sensor2){
    get_intersections(sensor1,sensor2)
  })
})

positions_to_check <- intersactions_df %>%
  group_by(x,y) %>%
  count() %>%
  ungroup() %>%
  filter(n>=4)

signal <- positions_to_check %>%
  mutate(i=row_number()) %>%
  group_by(i) %>%
  mutate(free= !is_covered(c(x,y),sensors_df)) %>%
  filter(free) %>%
  group_by(i) %>%
  filter(between(x,target_range[1],target_range[2]),
         between(y,target_range[1],target_range[2]))

format(signal$x*4000000+signal$y ,scientific = F)

# Visualization ####

plot_space(coord_df, target_range, center = "beacon")
plot_space(coord_df, target_range, center = "sensor")
