library(tidyverse)
input <- read_lines("AoC2023/Day6/input.txt")

races = list(time = str_extract_all(input[[1]], "\\d+",simplify = T) %>% as.integer(),
             distance = str_extract_all(input[[2]], "\\d+", simplify = T) %>% as.integer())

# Part 1 ####
get_dist = function(time, charge) {
  charge*time-charge^2
}
get_dists <- function(time) {
  map_dbl(1:time,  ~{
    get_dist(time, .x)
  })
}

pmap_dbl(races, function(time, distance) {
  sum(get_dists(time) > distance)
}) %>% prod()

# Part 2 ####
race <- map(races, str_c,collapse = "") %>% map(as.numeric)

polyroot(c(race$distance+1, -race$time, 1)) %>%  as.numeric() %>% diff() %>% floor()


# Vis ####
func = function(x) {
  x=x*1000*60*60
  (35937366*x-x^2)/(1000000*1E6)
}
ggplot(data=NULL)+
  geom_function(fun = func, xlim = c(0, 35937366/(1000*60*60))) +
  theme_bw()+
  labs(x="Charge time (h)",y="Distance (Mkm)")+
  geom_hline(yintercept = race$distance/(1000000*1E6), col="red")
