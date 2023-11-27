library(tidyverse)

input10 <- readLines("Day10/input10.txt")

# * - part 1 - *####
nums <- input10 %>% as.numeric()

diff <- tibble(a=c(0,nums) %>% sort()) %>%
  mutate(b=lead(a,1, default = max(a)+3),
         dif=b-a) %>%
  pull(dif) %>%
  table() %>% prod()

diff

# * - part 2 - *####
ns <- tibble(a=c(0,nums) %>% sort()) %>% 
  mutate(b=lead(a,1, default = max(a)+3),
         dif=b-a) %>%
  pull(dif) %>%
  paste0(collapse = "") %>%
  str_remove("[3]+$") %>%
  str_remove("^3") %>%
  str_split("[3]+") %>%
  sapply(function(x) (nchar(x)-1))

combos_fun <- function(vector) {
  sapply(vector, function(n) {
    if (n < 3) return( 2^n)
    else (y <- 2^n - sum(2^((n-3):0)))
  })
}

#options(digits=16)
combos_fun(ns) %>% purrr::reduce(prod) %>% 
  format(scientific = F)

