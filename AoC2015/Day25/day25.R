library(tidyverse)

input = read_lines('AoC2015/Day25/input.txt')

row = 2978
column = 3083

first_code = 20151125

# part 1 ----

get_coord = function(row, column) {
  r=1
  c=1
  nr=1
  i=1
  while (r != row | c != column) {
    i=i+1
    if(r==1){nr=nr+1;r=nr;c=1} else { r=r-1; c=c+1}
  }
  i
}
N = get_coord(row, column)

get_next_code = function(code, N=1) {
  
  for (i in 1:N) {
    code =  (code * 252533) %% 33554393
  }
  
  code
  
}

get_next_code(first_code, N-1)
