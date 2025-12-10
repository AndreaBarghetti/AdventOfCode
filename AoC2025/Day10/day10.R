library(tidyverse)
library(collections)

input <- read_lines("AoC2025/Day10/input.txt")
input <- read_lines("AoC2025/Day10/test")

parse_machines = function(input) {
  map(input, function(l) {
    lights = str_match(l, "\\[(.+)\\]")[2]
    lights = as.logical(str_split(lights,"",simplify = T)=="#")
    
    buttons = str_match_all(l, "\\(.*\\)") %>% str_split(" ", simplify = T) %>% 
      str_extract_all("\\d+") %>% map(as.integer)
    # make them binary for simplicity
    buttons = map(buttons, function(b) {
      button = vector(length = length(lights))
      button[b+1] = T
      button
    })
    
    joltage_requirements = str_match(l, "\\{(.+)\\}")[2] %>% str_extract_all('\\d+', simplify = T) %>% 
      as.integer()
    
    list(target_lights = lights,
         buttons = buttons,
         joltage_requirements = joltage_requirements)
  })
}

machines = parse_machines(input)

# Part 1 ####
get_state = function(lights) {
  paste0(as.integer(lights), collapse = '')
}

start_machine = function(machine) {
  
  target_lights = machine$target_lights
  buttons = machine$buttons
  
  state_cache <- new.env(hash = TRUE, parent = emptyenv())
  
  lights = vector(length = length(target_lights))
  counter = 0
  
  q = queue()
  
  q$push(list(lights=lights, counter = counter))
  
  repeat {
    
    state = q$pop()
    
    if (all(state$lights == target_lights)) { return(state$counter) }
    
    assign(get_state(state$lights) , state$counter, envir = state_cache)
    
    for (b in buttons) {
      
      nx_lights = xor(state$lights,b)

      if (!exists(get_state(nx_lights), envir = state_cache, inherits = FALSE)) {
        
        q$push(list(lights = nx_lights,
               counter = state$counter + 1 ))
        
      }
      
    }
    
  }
  
}

counters_pt1 = map_dbl(machines, start_machine)
sum(counters_pt1)

# Part 2 ####
configure_joltage = function(machine) {
  
  target_joltage = machine$joltage_requirements
  buttons = machine$buttons
  # buttons = buttons[order(map_int(buttons,sum), decreasing = T)]
  
  state_cache <- collections::dict()
  
  joltage = vector(mode='integer', length = length(target_joltage))
  counter = 0
  min_counter = Inf
  
  q = stack()
  
  q$push(list(joltage=joltage, counter = counter))
  
  while(q$size()>0) {
    
    state = q$pop()
    
    # if counter > min_counter, discard
    if (state$counter >= min_counter) {next}
    
    # if target is exceeded , discard
    if (any(state$joltage > target_joltage)) {next}
    
    # if target is reached, update min_counter
    if (all(state$joltage == target_joltage)) { min_counter = min(min_counter, state$counter); next }
    
    # if same state is already reached: 
    # - if counter is worse: discard
    # - if counter is better: update
    # otherwise assign new state
    hash = get_state(state$joltage)
    
    if ( state_cache$has(hash) ) {
      if (state$counter < state_cache$get(hash)) {
        state_cache$set(hash, state$counter)
      } else {next}
    } else {
      state_cache$set(hash, state$counter)
    }
    
    for (b in buttons) {
      # b=buttons[[1]]
      
      # for ( n in c(1,10,100)) {
      #   q$push(list(joltage = state$joltage + b*n,
      #               counter = state$counter + n ))
      # }
      
      q$push(list(joltage = state$joltage + b,
                  counter = state$counter + 1 ))
      
    }
    
  }
  min_counter
}

# system.time({
#   replicate(n = 100, configure_joltage(machines[[1]]))
# })
configure_joltage(machines[[1]])

counters_pt2 = map_dbl(machines, configure_joltage)

sum(counters_pt2)

sum(machines[[1]]$joltage_requirements)
machines[[1]]$buttons %>% map_int(sum)
