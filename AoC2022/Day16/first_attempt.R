library(tidyverse)

input <- read_lines("Day16/example.txt")
source("Day16/functions.R")
valves <- read_input(input)
names(valves) <- map_chr(valves, ~.x$valve)

# open all valves with 0 pressure
valves <- map(valves, function(valve) {
  if (valve$pressure==0) {valve$open <- T}
  valve
})


### older solution ####
source("Day16/functions.R")

status <- list(current = "AA",
               time = 30,
               pressure_released = 0,
               open_valves = valves %>% map_lgl(~.x$open),
               valve_pressure = valves %>% map_dbl(~.x$pressure),
               completed = F)
status$relevant_valves <- valves %>% map_lgl(~!.x$open)
status$bin_state <- get_bin_state(status)


#there are this many possible states
2^sum(!status$open_valves) * length(valves)

# this takes a while to run, but eventually gets the correct answer
completed <- run_all(valves, status)

completed %>%
  map_dbl(~.x$pressure_released) %>% max()

# part 2 ####
status <- list(current = c("AA","AA"),
               time = 26,
               pressure_released = 0,
               open_valves = valves %>% map_lgl(~.x$open),
               valve_pressure = valves %>% map_dbl(~.x$pressure),
               completed = F)
status$relevant_valves <- valves %>% map_lgl(~!.x$open)
status$bin_state <- get_bin_state(status)



move_to_valve <- function(status, to_valve, player) {

  if(is.null(status)) {return()}
  if(status$time == 0 ) {return()}

  if(player==1) {status$time <- status$time-1}
  status$current[player] <- to_valve
  status$current <- sort(status$current)
  status
}

open_valve <- function(status, player) {

  if (is.null(status)) {return()}
  if (status$time == 0) {return()}

  # if it's closed, open it
  if (!status$open_valves[[status$current[player]]]) {
    if(player==1) {status$time <- status$time-1}
    status$open_valves[[status$current[player]]] <- T
    status$bin_state <- get_bin_state(status)

    # if it was the last, status completed = T
    if (all(status$open_valves)) {
      status$completed <- T
    }

    #update pressure_released (at time 0)
    status$pressure_released <- status$pressure_released + status$time * status$valve_pressure[[status$current[player]]]
  }

  status
}

get_options <- function(status, valves, player) {
  if(is.null(status)) {return()}
  valves[[status$current[player]]]$tunnels
}

move_and_open_all <- function(status, valves) {

  if(is.null(status)) {return()}
  # unless it's all open already
  if (status$completed) {return(list())}
  if (status$time <= 0) {return(list())}

  future <- list()
  for (player in 1:2) {
    options <- get_options(status,valves, player)
    with_open <- list(open_valve(status, player))
    with_move <- map(options, function(to_valve){
      move_to_valve(status, to_valve, player)
    })
    future <- c(future, with_open, with_move)
  }
  unique(future)
}

get_bin_state <- function(status) {
  open_valves <- status$open_valves
  bin_str <- open_valves[status$relevant_valves]%>% ifelse(0,1) %>% str_c(collapse = "")
  bin_state <- strtoi(bin_str, base = 2)


  occupied_valves1 <- names(status$open_valves) %in% status$current[1]
  occupied_valves2 <-  names(status$open_valves) %in% status$current[2]
  bin_str1 <- occupied_valves1 %>% ifelse(0,1) %>% str_c(collapse = "")
  bin_str2 <- occupied_valves2 %>% ifelse(0,1) %>% str_c(collapse = "")
  bin_state1 <- strtoi(bin_str1, base = 2)
  bin_state2 <- strtoi(bin_str2, base = 2)

  sum(bin_state,bin_state1,bin_state2)
}

run_all <- function(valves, status) {
  multi_status <- move_and_open_all(status, valves)

  time_out <- multi_status %>% map_dbl(~.x$time) <= 0
  completed <- list()
  states <- tibble()

  repeat {

    multi_status <- map(multi_status, move_and_open_all, valves=valves) %>%
      unlist(recursive = F) %>%
      unique()
    multi_status <- multi_status[!map_lgl(multi_status,is.null)]
    # check which status is completed all valve open
    # or run out of time
    time_out <- multi_status %>% map_dbl(~.x$time)<=0
    all_opened <- multi_status %>% map_lgl(~.x$completed)


    completed <- c(completed,multi_status[all_opened|time_out])
    multi_status <- multi_status[!(all_opened|time_out)]

    # filter out unneccessary completeded suboptimal runs
    # this is likely unnecessary step
    scores <- completed %>% map_dbl(~.x$pressure_released)
    completed <- suppressWarnings(completed[scores == max(scores)])

    # stop is there are no more status to run
    xlen = length(multi_status)
    if (xlen==0) {break}
    print(xlen)

    # filter out hopeless statuses
    # if two status have same current position
    # and same open valves (could improve to say: same open valves or subgroup (worse))
    # keep only the one with higher score
    new_states <- tibble(score = multi_status %>% map_dbl(~.x$pressure_released),
                         bin_state = multi_status %>% map_dbl(~get_bin_state(.x)))

    states <- bind_rows(states, new_states) %>%
      group_by(bin_state) %>%
      mutate(keep = score >= max(score))

    from <- nrow(states)-nrow(new_states)+1
    to <- nrow(states)
    multi_status <- multi_status[states$keep[from:to]]

    states <- states %>%
      filter(keep)
  }
  completed
}

completed <- run_all(valves, status)

status$relevant_valves <- valves %>% map_lgl(~!.x$open)
status$bin_state <- get_bin_state(status)
