read_input <- function(input) {

  map(input, function(input) {
    valves <- input %>% str_extract_all("[A-Z]{2}", simplify = T)
    pressure <-  input %>% str_extract_all("[0-9]+", simplify = T) %>% as.numeric()
    valve <- valves[1]
    tunnels <- valves[-1]
    list(valve=valve,
         pressure=pressure,
         tunnels=tunnels)
  })
}

get_options <- function(valve_pressures, current, time) {
  names(valve_pressures)[valve_pressures>0]
}



# state_to_bin <- function(valve_pressures, current) {
#   str <- str_c((valve_pressures>0) + names(valve_pressures) %in% current,collapse="")
#   val <- strtoi(str, base = 4)
#   val
# }

# read_past_state <- function(time, valve, bincode) {
#   past_states <- get(envir = rlang::global_env(), x = "past_states")
#   past_states[[as.character(time)]][[valve]][[as.character(bincode)]]
# }
# write_past_state <- function(x, time, valve, bincode) {
#   past_states <- get(envir = rlang::global_env(), x = "past_states")
#   past_states[[as.character(time)]][[valve]][[as.character(bincode)]] <- x
#   assign(x = "past_states", value = past_states, envir = rlang::global_env())
# }
read_past_state <- function(bincode) {
  past_states <- get(envir = rlang::global_env(), x = "past_states")
  past_states[[bincode]]
}
write_past_state <- function(x, bincode) {
  past_states <- get(envir = rlang::global_env(), x = "past_states")
  past_states[[bincode]] <- x
  assign(x = "past_states", value = past_states, envir = rlang::global_env())
}

