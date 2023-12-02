library(tidyverse)

input = "1113122113"

# part 1 ####
look_and_say = function(x) {
  splits = str_extract_all(x, "(.)\\1*", simplify = T)
  what = str_sub(splits,1,1)
  times = nchar(splits) %>% as.character()
  paste0(times, what) %>% paste0(collapse = "")
}

look_and_sayX <- function(x,n) {
  for (i in 1:n) {
    x = look_and_say(x)
  }
  x
}

look_and_sayX(input,40) %>% nchar()


# part 2 ####
look_and_sayX(input,50) %>% nchar()
