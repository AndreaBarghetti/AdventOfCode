library(tidyverse)
input <- read_lines("AoC2023/Day4/input.txt")

parse_input <- function(input) {
  
  res = str_split(input, ":|\\|") %>% 
    map(str_extract_all, "\\d+") %>% 
    map(map,as.integer) %>% 
    map(setNames, c("Card","winning", "nums"))
  
  res
  
}

cards = parse_input(input)

# Part 1 ####
wins = map_int(cards, ~{
  sum(.x$nums %in% .x$winning)
})
sum(ifelse(wins>0,2^(wins-1),0))

# Part 2 ####
cards = rep(1, length(wins))

for (i in seq_along(cards)) {
  if(wins[i]==0) {next}
  cards[(i+1):(i+wins[i])] <- cards[(i+1):(i+wins[i])] + cards[i]
}
sum(cards)

