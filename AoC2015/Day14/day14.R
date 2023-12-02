library(tidyverse)

input <- read_lines("AoC2015/Day14/input.txt")

parse_input <- function(input) {
  r = input %>% str_extract("^\\w+")
  nums = input %>% str_extract_all("\\d+", simplify = T)
  speed = nums[,1] %>% as.numeric()
  run_time = nums[,2] %>% as.numeric()
  rest_time = nums[,3] %>% as.numeric()
  tibble(raindeer=r, speed=speed, run_time=run_time, rest_time=rest_time)
}

profiles <- parse_input(input) %>% 
  mutate(cycle_time = run_time+ rest_time,
         dist_per_cycle = speed*run_time)

# part 1 ####
race_time = 2503

profiles %>% 
  mutate(full_cycles = race_time%/%cycle_time,
         extra_time = race_time%%cycle_time,
         dist_run = full_cycles * dist_per_cycle,
         dist_run = dist_run + map2_dbl(extra_time, run_time,min)*speed) %>% 
  arrange(desc(dist_run)) %>% 
  select(raindeer, dist_run)


# part 2 ####
do_race = function(profiles, race_time) {
  
  running = pmap(profiles, function(...) {
    k = list(...)
    rep(c(rep(1, k$run_time),rep(0, k$rest_time)), (race_time%/%k$cycle_time+1))[1:race_time]
  }) %>% setNames(profiles$raindeer) %>% purrr::reduce(rbind)
  
  dists = apply(running,2,function(x) {x*profiles$speed})
  dists = apply(dists, 1, cumsum) %>% t()
  apply(dists,2,function(x) {
    profiles$raindeer[which(x==max(x))]
  }) %>% unlist() %>% table() %>% sort(decreasing = T)
}

do_race(profiles, race_time)[1]
