library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 8, .fun = read_lines)
input <- input %>% str_split("", simpl=T) %>% as.integer()

# part 1 ####

width = 25
length = 6
layers = length(input)/width/length
image <- array(input, dim = c(width,length, layers))

n_zeros <- apply(image,3,function(l) {
  sum(l==0)
})

min0_l <- image[,,which(n_zeros==min(n_zeros))]
sum(min0_l==1)*sum(min0_l==2)

# part 2 ####
first_pixel <- function(x) {
  bw <- x %in% 0:1
  x[bw][1]
}

image %>% 
  apply(c(2,1),first_pixel) %>%
  plot_matrix()
