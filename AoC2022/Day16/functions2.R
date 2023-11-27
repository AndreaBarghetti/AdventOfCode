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

get_bin_state <- function(status) {
  values <- !status$open_valves + (names(status$open_valves) %in% status$current)
  bin_str <- str_c(values, collapse = "")
  bin_state <- strtoi(bin_str, base = 4)
  bin_state
}

move_to_valve <- function(status, to_valve, graph_df) {

  if(status$time <= 0 ) {return()}

  dist <- get_dist(status$current, to_valve, graph_df)

  status$time <- status$time-dist
  status$current <- to_valve
  status$bin_state <- get_bin_state(status)
  status
}

get_options <- function(status, graph_df) {

  if(is.null(status)) {vector(mode = "character", length = 0)}

  names(status$open_valves)[!status$open_valves] %>% setdiff(status$current)
}

open_valve <- function(status) {

  # if out of time, do nothing
  if (status$time <= 0) {return(status)}
  # if it's already open it, do nothing
  if (status$open_valves[[status$current]]) {return(status)}

  # if it's closed, open it
  status$time <- status$time-1
  status$open_valves[[status$current]] <- T
  status$bin_state <- get_bin_state(status)

  # if it was the last, status completed = T
  if (all(status$open_valves)) {
    status$completed <- T
  }

  #update pressure_released (at time 0)
  status$pressure_released <- status$pressure_released + status$time * status$valve_pressure[[status$current]]

  status
}

multiply_status <- function(status,graph_df) {

  # add open valve if possible
  if (!status$open_valves[status$current]) {
    with_open <- list(open_valve(status))
    return(with_open)
  }
  else {
    move_options <- get_options(status, graph_df)
    with_move <- map(move_options, move_to_valve, status = status, graph_df = graph_df)
    return(with_move)
  }
}

multiply_states <- function(states, graph_df) {
  map(states, function(status) {
    multiply_status(status,graph_df)
  }) %>% unlist(recursive = F)
}
