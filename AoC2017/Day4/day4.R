library(tidyverse)
input <- read_lines('AoC2017/Day4/input.txt')

passphrases = input %>% str_split(" ")

# Part 1 ----
validate_passphrase = function(passphrase) {
  !any(passphrase %>% table()>1)
}

sum(map_lgl(passphrases, validate_passphrase))

# Part 2 ----
sort_letters = function(passphrase) {
  map_chr(passphrase, function(word) {
    str_split(word,"", simplify = T) %>% sort() %>% paste0(collapse = "")
  })
}

map(passphrases, sort_letters) %>% 
  map_lgl(validate_passphrase) %>% 
  sum()
