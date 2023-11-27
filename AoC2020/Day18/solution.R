library(tidyverse)

# Day 18: Operation Order ####
input18 <- read_lines("Day18/input18.txt")

math <- sapply(USE.NAMES = F, input18, str_remove_all, " ")


# * - part 1 - *####

# if there is a bracket extract it, solve it and replace it
# if there are no bracket, solve first 2 and replace results
# if there is only one number left, return result
string = "1+1*4"
solve_first2 <- function(string) {
  paste0(string %>% str_match("[0-9]+[\\+\\*][0-9]+") %>% pluck(1) %>% parse(text = .) %>% eval(),
         string %>% str_remove("[0-9]+[\\+\\*][0-9]+"))
}
solve_first2(string)

solve_all <- function(string) {
  while( str_detect(string, "[\\+\\*]")) {
    string <- solve_first2(string)
  }
  return(string)
}

solve_all(string)

string = "(2*3)+(1*4+(2*3))"
extract_bracket <- function(string) {
  (string %>% str_match(.,"\\(([0-9\\*\\+]+)\\)"))[[2]]
}
extract_bracket(string)

extract_inner_bracket <- function(string) {
  while( str_detect(string, "[\\(\\)]")) {
    string <- extract_bracket(string)
  }
  return(string)
}
extract_inner_bracket(string)

solve_inner_bracket <- function(string) {
  string %>% str_replace_all(
    paste0("\\(",
           extract_inner_bracket(string) %>%
             str_replace_all(c("\\*" = "\\\\*", 
                               "\\+" = "\\\\+")),
           "\\)"),
    solve_all(extract_inner_bracket(string))
  )
}

solve_inner_bracket(string)

solve_math <- function(string) {
  while(str_detect(string, "[\\(\\)]")) {
    string <- solve_inner_bracket(string)
  }
  return(string %>% solve_all)
}

solve_math(string)

# options(digits=16)
sapply(math, solve_math) %>% as.numeric() %>% sum() %>% 
  format(scient=F)


# * - part 2 - *####
string = "1+1*4+1"
string = "2*4+1"

solve_first_add <- function(string) {
  string %>% str_replace(
    str_match(string, "[0-9]+[\\+][0-9]+") %>% pluck(1)%>%
      str_replace("\\+","\\\\+"),
    str_match(string, "[0-9]+[\\+][0-9]+") %>% pluck(1) %>% 
      parse(text = .) %>% eval() %>% as.character())
}
solve_first_add(string)

solve_all <- function(string) {
  while( str_detect(string, "[\\+]")) {
    string <- solve_first_add(string)
  }
  while(str_detect(string, "[\\*]")) {
    string <- solve_first2(string)
  }
  return(string)
}

sapply(math, solve_math) %>% as.numeric() %>% sum()%>% 
  format(scient=F)

