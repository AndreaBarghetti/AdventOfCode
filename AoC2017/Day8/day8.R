library(tidyverse)
input <- read_lines('AoC2017/Day8/input.txt')

program = input %>% str_match_all('\\S+') %>% 
  map(function(x) {
    list(
      reg = x[1],
      verb = ifelse(x[2]=='inc','+','-'),
      val = as.numeric(x[3]),
      c_reg = x[5],
      op = x[6],
      c_val = as.numeric(x[7])
    )
  })

# Part 1 + 2----
library(collections)

run_program = function(program) {
  
  register = dict()
  max_val = 0
  
  for (instruction in program) {
      
      if (!register$has(instruction$reg)) { register$set(instruction$reg, 0) }
      if (!register$has(instruction$c_reg)) { register$set(instruction$c_reg, 0) }
      
      condition = do.call(instruction$op, list(register$get(instruction$c_reg), instruction$c_val))
      
      if (condition) { 
        reg_val = do.call(instruction$verb, list(register$get(instruction$reg), instruction$val))
        max_val = max(max_val, reg_val)
        register$set(instruction$reg, reg_val)
      }
  }

  message('max value: ', max_val)
  
  return(register)
}

register = run_program(program)

register$as_list() %>% unlist() %>% max()
