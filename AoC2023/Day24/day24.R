library(tidyverse)
input <- read_lines("AoC2023/Day24/test.txt")

hail2 = input %>% 
  str_extract_all("[-]*\\d+") %>% 
  map(as.numeric)

# test_area = c(7,27)
test_area = c(200000000000000, 400000000000000)

# Part 1 ####
hail = map(hail2,~.x[c(1,2,4,5)])

line = function(h) {
  A = h[4]
  B = -h[3]
  C = A*h[1] + B*h[2]
  c(A=A,B=B,C=C)
}

line_intersect <- function(l1, l2) {
  
  det <- l1['A'] * l2['B'] - l2['A'] * l1['B']
  
  # If the determinant is zero, lines are parallel or coincident
  if (det == 0) {
    return(NULL)
  }
  
  # find the intersection point
  x <- (l2['B'] * l1['C'] - l1['B'] * l2['C']) / det
  y <- (l1['A'] * l2['C'] - l2['A'] * l1['C']) / det
  return(c("x" = unname(x), "y" = unname(y)))
}

# get all intersactions
intersactions <- map(seq_along(hail[-1]), ~{
  h1 = hail[[.x]]
  l1 = line(h1)
  
  hails = hail[(.x+1):length(hail)]
  
  intersections <- map(hails, function(h2){
    l2 = line(h2)
    int = line_intersect(l1,l2)
    if (is.null(int)){return(NULL)}
    # if it happened in the past:
    in_fut = c(
      sign(int[1]-h1[1])==sign(h1[3]),
      sign(int[2]-h1[2])==sign(h1[4]),
      sign(int[1]-h2[1])==sign(h2[3]),
      sign(int[2]-h2[2])==sign(h2[4])
      ) %>%
      all()
    if(!in_fut){return(NULL)}
    return(int)
  })
  intersections
}) %>% unlist(recursive = F)

# count intersactions in area
map_lgl(intersactions, ~ {
  if (is.null(.x)) {return(F)}
  all(between(.x, test_area[1],test_area[2]))
}) %>% sum()


# Part 2 ####
# NOT WORKING

library(symengine)

use_vars(xr, yr, zr, vxr, vyr, vzr)

eq_system <- map(hail2, function(h) {
  
  eq1 <- (xr-h[1]) * (h[5] - vyr) - (yr - h[2]) * (h[4] - vxr)
  eq2 <- (yr-h[2]) * (h[6] - vzr) - (zr - h[3]) * (h[5] - vyr)
  c(eq1, eq2)
  
}) %>% purrr::reduce(c)

eq_system[[2]]

solve(eq_system, c(xr, yr, zr, vxr, vyr, vzr))



# symengine test

# Load symengine library
library(symengine)

# Define symbolic variables
use_vars(x,y,z)
# Define the equations
eq1 <- x + y + z - 6
eq2 <- 2 * x - 3 * y + z + 1
eq3 <- x + y + 2 * z - 9

# Solve the system of equations
solution <- solve(c(eq1, eq2, eq3), c(x, y, z))

# Print the solution
print(solution)
