library(tidyverse)

input = read_lines('AoC2015/Day23/input.txt')

parse_instruction = function(instruction) {
  if (str_detect(instruction, '[\\+-]\\d+')) {
    val = str_extract(instruction, '[\\+-]\\d+') %>% as.integer()
    instruction = str_remove(instruction, ',.+')
  } else {val = NA}
  if (str_detect(instruction, ' [ab]$')) {
    reg = str_extract(instruction, '[ab]$')
  } else {reg= NA}
  cmd = str_extract(instruction, '^\\w+')
  return(list(cmd=cmd, r=reg, v=val))
}

program = map(input,parse_instruction)


# part 1 ----
run_program = function(program, reg=c(a=0,b=0)) {
  i=1
  l=length(program)
  
  while (i <= 46) {
    instr = program[[i]]
    cmd = instr[['cmd']]
    v = instr[['v']]
    r = instr[['r']]
    
    if (cmd == 'inc') {reg[r] = reg[r]+1}
    if (cmd == 'hlf') {reg[r] = reg[r]/2}
    if (cmd == 'tpl') {reg[r] = reg[r]*3}
    if (cmd == 'jmp') { i = i + v; next }
    if (cmd == 'jie') {if (reg[r]%%2==0) { i = i + v; next} }
    if (cmd == 'jio') {if (reg[r]==1) { i = i + v; next} }
    i=i+1
  }
  reg
}
run_program(program, reg = c(a=1,b=0))



