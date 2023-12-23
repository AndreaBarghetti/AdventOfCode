library(tidyverse)
input <- read_lines("AoC2023/Day19/input.txt")

parse_input <- function(input) {
 bl = which(input=="")
 
 instr = input[1:(bl-1)]
 parts = input[(bl+1):length(input)]
 
 parts_ls = map(parts, function(p) {
   str_extract_all(p, "\\d+") %>% unlist() %>%  
     as.integer() %>% 
     setNames(c("x","m","a","s"))
 })
 
 instr_names <- str_extract(instr, "^\\w+")
 instr_ls = map(instr, function(line) {
   
   rule = str_replace(line, "^.+\\{(.+)\\}","\\1")
   rules = str_split(rule, ",") %>% unlist()
 }) %>% setNames(instr_names)
 
 return(list(parts = parts_ls, 
             rules = instr_ls))
}

pinput = parse_input(input)
rules = pinput$rules
parts = pinput$parts

# Part 1 ####
apply_rule = function(part, rule_name, rules) {
  
  if(rule_name=="A") {return(T)}
  if(rule_name=="R") {return(F)}
  rule=rules[[rule_name]]
  
  for (r in rule) {
    
    if (!str_detect(r,":")) {
      return(apply_rule(part, r, rules))
    }
    
    p = str_extract(r, "^[xmas]")
    op = str_extract(r, "[<>]")
    val = str_extract(r, "\\d+") %>% as.numeric()
    to = str_extract(r, "\\w+$")
    cond = do.call(op, args = list(part[[p]], val))
    
    if(cond) {
      return(apply_rule(part, to, rules))
    } else {next}
  }
}

map_dbl(parts, function(part) {
  pass = apply_rule(part, 'in', rules)
  if (pass) {
    sum(part)
  } else{0}
}) %>% sum()


# Part 2 ####
