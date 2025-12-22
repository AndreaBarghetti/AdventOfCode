library(tidyverse)
input <- read_lines('AoC2017/Day5/input.txt')

program = as.integer(input)

# Part 1 ----
test_program = function(program) {
  L = length(program)
  i = 1
  count=0
  while(i<=L & i>=0) {
    count=count+1
    j = program[i]
    program[i] =  program[i] + 1
    i = i+j
  }
  count
}
test_program(program)

# Part 2 ----
test_program2 = function(program) {
  L = length(program)
  i = 1
  count=0
  while(i<=L & i>=0) {
    count=count+1
    j = program[i]
    if (j >= 3) {program[i] =  program[i] - 1} else {program[i] =  program[i] + 1}

    i = i+j
  }
  count
}

test_program2(program)
