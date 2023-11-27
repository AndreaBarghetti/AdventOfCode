library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 19, .fun = read_lines)
program <- input %>% str_split(",", simplify = T) %>% as.numeric()

source('Day19/intcode_computer.R')


# part 1 ####
drone <- Drone$new(program)
# drone$map <-map
drone$scan_map()

sum(drone$map==1)

# part 2 ####
drone$map %>% 
  plot_matrix()

search_space <- function(drone) {
  pos <-c(100,0)
  repeat {
    # message(pos[1],",",pos[2])
    suppressMessages({
      if (drone$test(pos)==0) {
        pos <- pos + c(-1,1)
      } else {
        check = drone$test(pos+c(-99,99))==1
        if(check) {return(pos)}
        pos <- pos + c(1,0)
      }
    })
  }
}

C1 = search_space(drone)
res = C1 + c(-99,0)

res[1]*10000+res[2]
