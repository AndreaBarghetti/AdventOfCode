library(tidyverse)

input = "cqjxjnds"

# part 1 ####

check_pw_num <- function(pw_num) {
  
  rule1 = any(pw_num %in% c(9,12))
  if (rule1) {return(F)}
  
  rule2 = {any(pw_num == lead(pw_num, 1)-1 & pw_num == lead(pw_num, 2)-2, na.rm = T)}
  if (!rule2) {return(F)}
  
  rule3 = {
    pw = paste0(letters[pw_num], collapse = "")
    str_detect(pw, "[a-z]*([a-z])\\1[a-z]*(?!\\1)([a-z])\\2")
  }
  if (!rule3) {return(F)}
  
  return(TRUE)
}

next_pw_num <- function(pw_num, i) {
  
  if(pw_num[i]<26) {
    pw_num[i] <- pw_num[i]+1
    return(pw_num)
  } else {
    pw_num[i] <- 1
    return(next_pw_num(pw_num, i-1))
  }
  
}


get_new_pw <- function(pw) {
  
  p_w = str_split(pw, "", simpl=T)
  pw_num = unname(setNames(1:26, letters)[p_w])
  
  repeat {
    pw_num <- next_pw_num(pw_num,length(pw_num))
    if (check_pw_num(pw_num)) {break}
  }
  
  paste(letters[pw_num], collapse = "")
}

new_pw = get_new_pw(input)

# part 2 ####
new_pw = get_new_pw(new_pw)
