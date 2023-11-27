library(tidyverse)

input <- read_lines("Day16/example.txt")
source("Day16/functions.R")
source("Day16/functions2.R")
valves <- read_input(input)
names(valves) <- map_chr(valves, ~.x$valve)

# part 1 ####
status <- list(current = "AA",
               time = 30,
               pressure_released = 0,
               open_valves = valves %>% map_lgl(~.x$open),
               valve_pressure = valves %>% map_dbl(~.x$pressure),
               completed = F)
status$relevant_valves <- valves %>% map_lgl(~!.x$open)
status$bin_state <- get_bin_state(status)

states <- list(status)
past_states <- tibble()
completed_states <- c()
while (length(states)>0) {
  states <- multiply_states(states, graph_df)
  completed <- states %>% map_lgl(~all(.x$open_valves))
  timed_out <- states %>% map_lgl(~.x$time<=0)
  completed_states <- c(completed_states,states[completed|timed_out])
  states <- states[!(completed|timed_out)]

  new_states <- tibble(binstate=states %>% map_dbl(~.x$bin_state),
                       score=states %>% map_dbl(~.x$pressure_released))

  past_states <- bind_rows(past_states, new_states) %>%
    group_by(binstate) %>%
    filter(score == max(score))

  keep <- new_states %>%
    mutate(x = row_number()) %>%
    semi_join(past_states, by = c("binstate", "score")) %>%
    pull(x)
  states<-states[keep]
  print(length(states))
}

completed_states %>%
  map_dbl(~.x$pressure_released) %>% max()
