library(tidyverse)
input <- read_lines("AoC2024/Day5/input.txt")

parse_input = function(input) {
  
  sep = which(input=="")
  
  rules = input[1:(sep-1)]
  updates = input[-(1:sep)]
  
  parse = function(line, sep) {
    str_split(line,sep,simplify = T) %>% as.integer()
  }
  
  rules = map(rules, parse, "\\|")
    
  updates = map(updates, parse, ",")
  
  return(list(
    rules = rules,
    updates = updates
  ))
}

input = parse_input(input)

rules = input$rules
updates =  input$updates

# Part 1 ####

apply_rule = function(rule, update) {
  
  if (!all(rule %in% update)) {return(TRUE)}
  return(which(rule[1]==update) < which(rule[2]==update))
  
}

apply_rules = function(update, rules) {
  all(map_lgl(rules, apply_rule, update))
}

valid_updates = map_lgl(updates, apply_rules, rules)

mid = function(x) {
  l = length(x)
  x[l%/%2+1]
}

sum(map_int(updates[valid_updates], mid))


# Part 2 ####
invalid_updates = updates[!valid_updates]

fix_rule = function(update, rule) {
  fixed_update = update
  fixed_update[update==rule[1]] = rule[2]
  fixed_update[update==rule[2]] = rule[1]
  fixed_update
} 

fix_update = function(update, rules) {
  
  rules_to_fix = !map_lgl(rules, apply_rule, update)
  
  if(!any(rules_to_fix)) {
    return(update)
  } else {
    
    update = fix_rule(update, rules[rules_to_fix][[1]])
    
  }
  
  fix_update(update,rules)

}

updates_fixed = map(invalid_updates, fix_update, rules) 

map_int(updates_fixed, mid) %>% sum()

