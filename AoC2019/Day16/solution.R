library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 16, .fun = read_lines)

signal = as.integer(str_split(input,"", simplify=T))

#test
# signal = as.integer(str_split('03036732577212944063491565474664',"", simplify=T))

right_digit <- function(x) {
  as.character(x) %>% str_sub(-1,-1) %>% 
    as.integer()
}

get_signal_mat <- function(signal) {
  matrix(signal, nrow = length(signal),ncol=length(signal), byrow = T)
}

get_phase_mat <- function(signal, signal_offset = c(0,1,0,-1)) {
  apply(matrix(seq_along(signal)),1, function(i) {
    circular(rep(signal_offset, each=i))[seq_along(signal)+1]
  }) %>% t()
}

step_phase <- function(signal, phase_mat) {
  
  signal_mat <- get_signal_mat(signal)
  
  rowSums(signal_mat*phase_mat) %>% 
    right_digit()
  
}

phase_x <- function(signal, phases, signal_offset = c(0,1,0,-1)) {
  
  phase_mat <- get_phase_mat(signal, signal_offset)
  
  for (phase in seq_along(integer(phases))) {
    signal <- step_phase(signal, phase_mat)
  }
  signal
}

# part 1 ####
str_c(phase_x(signal, 100)[1:8], collapse = "")

# part 2 ####
message_offset <- str_c(signal[1:7], collapse = "") %>% as.integer()

cut_signal <- circular(signal)[(message_offset+1):(length(signal)*10000)] %>% 
  as.numeric()

step_phase2 <- function(signal) {
  rev(cumsum(rev(signal))) %>% 
    right_digit()
}

phase_x2 <- function(signal, phases) {
  
  for (phase in 1:phases) {
    message(phase)
    signal <- step_phase2(signal)
  }
  signal
}

str_c(phase_x2(cut_signal, 100)[1:8], collapse = "")


# see why this works:

# the message offset is way past the the half of the signal
message_offset/(length(signal)*10000)

# for each each phase
# the second half of the signal is only affected by the N last elements
# as all other are 0:
# the last element is always the same,
# the second last is the sum of the last 2, 
# the 3rd last is the sum of the last 3 etc...

get_phase_mat(1:50, signal_offset = c(0,1,0,-1)) %>% 
  mat_to_tibble() %>% 
  ggplot2::ggplot(ggplot2::aes(x = x, y = y)) + 
  ggplot2::geom_tile(ggplot2::aes(fill = as.factor(value)), show.legend = T) + 
  ggplot2::theme_void() + 
  ggplot2::coord_equal() +
  scale_fill_viridis_d()
  
