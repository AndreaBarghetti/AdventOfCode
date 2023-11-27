library(tidyverse)

input <- read_lines("Day16/input.txt")
source("Day16/third_attempt.R")
valves <- read_input(input)
names(valves) <- map_chr(valves, ~.x$valve)


# make graph of distances
source("Day16/graph_funs.R")
graph_df <- make_graph_df(valves)

#careonly about valves with pressure
valve_pressures <- valves %>% map_dbl(~.x$pressure)
valve_pressures <- valve_pressures[valve_pressures>0]
valve_names <- names(valve_pressures)

# run DepthFirst Search
DFS <- function(current, score=0, time=30, valve_pressures) {

  # get bincode of current state
  # bincode <- state_to_bin(valve_pressures,current)

  # get an distinc rapresentation of current state
  bincode <- str_c(c(current, time, valve_pressures), collapse = "-")

  # if this state already appeared:
  past_score <- read_past_state(bincode)
  if (!is.null(past_score)) {
    return(max(score, past_score))
  }

  # if it's the end, return score
  if ((time <=0) | all(valve_pressures==0)) {
    write_past_state(score,bincode)
    return(score)
  }

  # otherwise
  # get all options (what to open next)
  valves <- get_options(valve_pressures)
  # valves <- get_options2(valve_pressures, current, time)

  if (length(valves)!=0) {
    # and iterate over all options
    score <- map_dbl(valves, function(valve) {
      dist <- get_dist(current, valve)
      time <- max(0,time-dist-1)
      score <- score + valve_pressures[valve]*time
      valve_pressures[valve] <- 0
      DFS(current = valve,
          score = score,
          time = time,
          valve_pressures = valve_pressures)
    }) %>% max()
  }

  write_past_state(score,bincode)
  return(score)
}

past_states <- list()
DFS("AA", score = 0, time = 30, valve_pressures)

#should return score of 20
get_options2(valve_pressures, "AA", time = 3)
get_dist(start = "AA", names(valve_pressures))

#try avoid going valves if there is no time to open them
get_options2 <- function(valve_pressures, current, time) {
  options <- names(valve_pressures)[valve_pressures>0]
  dists <- get_dist(current,options)
  options<-options[(time-dists-1)>0]
  options
}

past_states
which(past_states %>% map_dbl(~.x)==1647) %>% unname() %>% dput()
past_states[c(127L, 128L, 129L, 140L, 141L, 360L, 401L, 402L)]
names(past_states)


letters %>% setNames(., 1:26)



### random stuff #####

library(GGally)
library(network)
library(sna)
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)
network.vertex.names(net) = letters[1:10]


pressure_df <- map_dbl(valves, ~.x$pressure) %>% tibble(valve=names(.), pressure=., )

df <- graph_df %>%
  left_join(pressure_df, by=c("end"="valve"))

test <- df %>%
  group_by(start, end) %>%
  filter(end %in% valves[[start]]$tunnels) %>%
  pull(pressure)

net <- df %>%
  group_by(start, end) %>%
  filter(end %in% valves[[start]]$tunnels) %>%
  ungroup() %>%
  spread(end,dist) %>%
  select(-pressure) %>%
  select_if(is.numeric) %>%
  as.matrix() %>% ifelse(is.na(.),0,.) %>%
  network(loops = F, directed = F)


ggnet2(net, show.legend=F, node.color = unique(test))
