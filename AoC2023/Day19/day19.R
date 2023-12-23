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
p_range = list(x=c(1,4000), m=c(1,4000), a=c(1,4000), s=c(1,4000))

apply_rule2 = function(p_range, rule_name, rules) {
  
  if(rule_name=="A") {return(prod((map_int(p_range,diff)+1)))}
  if(rule_name=="R") {return(0)}
  
  rule=rules[[rule_name]]
  
  for (r in rule) {
    
    if (!str_detect(r,":")) {
      return(apply_rule2(p_range, r, rules))
    }
    
    p = str_extract(r, "^[xmas]")
    op = str_extract(r, "[<>]")
    val = str_extract(r, "\\d+") %>% as.numeric()
    to = str_extract(r, "\\w+$")
    
    if (val<=p_range[[p]][1] | val>=p_range[[p]][2]) {
      
      cond = do.call(op, args = list(p_range[[p]][1], val))
      
      if(cond) {
        return(apply_rule2(p_range, to, rules))
      } else {next}
      
    } else {
      p_range1 <- p_range2 <- p_range
      if (op=="<") {
        p_range1[[p]][2] <- val-1
        p_range2[[p]][1] <- val
      } else {
        p_range1[[p]][2] <- val
        p_range2[[p]][1] <- val+1
      }
      
      return(apply_rule2(p_range1, rule_name, rules) + apply_rule2(p_range2, rule_name, rules))
    }

  }
  
}

apply_rule2(p_range, "in", rules) %>% 
  format(scientific = F)
