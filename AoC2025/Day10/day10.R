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
    
    target_joltage = str_match(l, "\\{(.+)\\}")[2] %>% str_extract_all('\\d+', simplify = T) %>% 
      as.integer()
    
    list(target_lights = lights,
         buttons = buttons,
         target_joltage = target_joltage)
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
  
  lights = vector(length = length(target_lights))
  counter = 0
  
  q = priority_queue()
  
  bpressed = vector(length = length(buttons))
  
  q$push(list(lights=lights, bpressed = bpressed, counter=counter), priority = -counter)
  
  repeat {
    
    state = q$pop()
    
    if (all(state$lights == target_lights)) { return(state$counter) }
    
    for (i in seq_along(buttons[!state$bpressed])) {
      
      b = buttons[!state$bpressed][[i]]
      bpressed = state$bpressed
      bpressed[i] = T
      counter = state$counter + 1
      
      nx_lights = xor(state$lights,b)

      q$push(list(lights = nx_lights, 
                  counter= counter,
                  bpressed = bpressed), priority = -counter)
      
    }
    
  }
  
}

counters_pt1 = map_dbl(machines, start_machine)
sum(counters_pt1)

# Part 2 ####
library(lpSolve)

# I need to solve this system of equations
# with constrains (integers >= 0)
# and minimizing sum of parameters
print_system = function(machine) {
  
  buttons = machine$buttons
  target_joltage = machine$target_joltage
  
  L = paste0(imap_chr(buttons, ~paste0(sum(.x),"*",letters[.y])), collapse = ' + ')
  R = sum(target_joltage)
  for (i in seq_along(target_joltage)) {
    r = target_joltage[i]
    l = paste0(paste0(letters[which(map_lgl(buttons,i))]), collapse = " + ")
    cat(paste(l,'=',r),'\n')
  }
  cat(paste(L,'=',R),'\n')
}
print_system(machine)

configure_joltage = function(machine) {
  
  target_joltage = machine$target_joltage
  buttons = machine$buttons
  
  num_vars = length(buttons)
  
  eq_system = map(seq_along(target_joltage), ~{
    l = map_lgl(buttons,.x)
    l
  }) %>% do.call(rbind,.)
  
  l = map_int(buttons, ~sum(.x))
  eq_system = rbind(eq_system, l)
  
  # objective function: minimize sum of all variables
  objective.in <- rep(1, num_vars)
  
  # constraints
  constraints <- matrix(0, nrow = num_vars, ncol = num_vars)
  diag(constraints)<-1
  
  # constraint directions
  const.dir <- c(rep("=", nrow(eq_system)), rep(">=", num_vars))
  
  const.mat = rbind(eq_system, constraints)
  
  const.rhs = c(target_joltage, sum(target_joltage), rep(0,num_vars))

  res <- lp("min", objective.in, const.mat, const.dir, const.rhs, all.int = TRUE)
  
  return(res$objval)
}

counters_pt2 = map_dbl(machines, configure_joltage)
sum(counters_pt2)
