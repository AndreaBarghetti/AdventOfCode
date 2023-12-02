library(tidyverse)

input <- read_lines("AoC2015/Day15/input.txt")

parse_input <- function(input) {
  ingr <- str_extract(input, "^\\w+")
  values = str_extract_all(input, "-*\\d+", simplify = T)
  values = matrix(as.numeric(values), nrow(values))
  rownames(values)<- ingr
  values
}

properties <- parse_input(input)

get_score <- function(Sp,Bu,Ch) {
  
  coefs = c(Sp,Bu,Ch)
  if(sum(coefs)>100){return(0)}
  coefs = c(coefs, 100-sum(coefs))
  
  score = (properties *c(coefs)) %>% apply(2,sum) %>% sapply(max,0) %>% prod()

  score
}

res <- optim(par = c(10,10,10), fn = function(x) {get_score(x[1], x[2], x[3])}, lower=c(0,0,0), upper = c(100,100,100),
      method="L-BFGS-B", hessian = TRUE)
