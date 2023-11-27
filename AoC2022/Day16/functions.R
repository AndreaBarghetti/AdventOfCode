read_input <- function(input) {

  map(input, function(input) {
    valves <- input %>% str_extract_all("[A-Z]{2}", simplify = T)
    pressure <-  input %>% str_extract_all("[0-9]+", simplify = T) %>% as.numeric()
    valve <- valves[1]
    tunnels <- valves[-1]
    list(valve=valve,
         pressure=pressure,
         tunnels=tunnels,
         open = F)
  })
}

move_to_valve <- function(status, to_valve) {
  if(is.null(status)) {return()}
  if(status$time == 0 ) {return()}

  status$time <- status$time-1
  # status$previous <- str_c(name=status$current, open=status$valves[[status$current]]$open)
  status$current <- to_valve
  status
}

open_valve <- function(status) {

  if(is.null(status)) {return()}
  if (status$time <= 0) {return()}

  # if it's closed, open it
  if (!status$open_valves[[status$current]]) {
    status$time <- status$time-1
    status$open_valves[[status$current]] <- T
    status$bin_state <- get_bin_state(status)

    # if it was the last, status completed = T
    if (all(status$open_valves)) {
      status$completed <- T
    }

    #update pressure_released (at time 0)
    status$pressure_released <- status$pressure_released + status$time * status$valve_pressure[[status$current]]
  }

  status
}

move_and_open <- function(status, to_valve) {
  if(is.null(status)) {return()}
  status <- move_to_valve(status, to_valve)
  status <- open_valve(status)
  status
}

# status <- move_and_open(status, "DD")
# status$valves[options] %>% map_chr(~.x$valve)
# status$previous

get_options <- function(status, valves) {
  if(is.null(status)) {return()}
  valves[[status$current]]$tunnels
}

move_and_open_all <- function(status, valves) {

  if(is.null(status)) {return()}
  # unless it's all open already
  if (status$completed) {return(list())}
  if (status$time <= 0) {return(list())}

  options <- get_options(status,valves)

  # avoid going back where you came from
  # check_prev <- str_c(options,status$valves[options] %>% map_chr(~as.character(.x$open)))
  # options <- options[!check_prev==status$previous]


  with_open <- map(options, function(to_valve){
    move_and_open(status, to_valve)
  })
  without_open <- map(options, function(to_valve){
    move_to_valve(status, to_valve)
  })
  c(with_open, without_open)
}

# get_potential <- function(status) {
#   # open_valves <- !status$open_valves
#   potential <- sum(status$valve_pressure * status$time * (!status$open_valves))
#   potential
# }

get_bin_state <- function(status) {
  open_valves <- status$open_valves
  bin_str <- open_valves[status$relevant_valves]%>% ifelse(0,1) %>% str_c(collapse = "")
  bin_state <- strtoi(bin_str, base = 2)
  bin_state
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
    new_states <- tibble(current = multi_status %>% map_chr(~.x$current),
                         score = multi_status %>% map_dbl(~.x$pressure_released),
                         bin_state = multi_status %>% map_dbl(~get_bin_state(.x)))

    states <- bind_rows(states, new_states) %>%
      group_by(current,bin_state) %>%
      mutate(keep = score >= max(score))

    from <- nrow(states)-nrow(new_states)+1
    to <- nrow(states)
    multi_status <- multi_status[states$keep[from:to]]

    states <- states %>%
      filter(keep)
  }

  completed
}


get_score <- function(path) {
  starts = lag(path,default = "AA")[-(length(path)+1)]
  dists <- map2_dbl(starts,path, function(start,end) {get_dist(start = start, end = end, graph_df)}) %>% setNames(path)
  dist_open <- dists+1
  time_left <- 30-cumsum(dist_open)
  time_left <- ifelse(time_left>0,time_left,0)
  res <- sum(time_left*valve_pressures[path])
  res
}
# get_score(c("DD", "BB", "JJ", "HH","EE","CC"))
