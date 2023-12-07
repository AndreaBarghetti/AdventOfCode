library(tidyverse)
input <- read_lines("AoC2023/Day7/input.txt")

hands <- map_dfr(input, function(line) {
  hand <- str_extract(line, "^\\S+")
  bid <- str_extract(line,"\\S+$") %>% as.integer()
  list(hand=hand, bid=bid)
})

# Part 1 ####
get_hand_type <- function(count) {
  if (5 %in% count) {return(7)}
  if (4 %in% count) {return(6)}
  if ((3 %in% count) & (2 %in% count)) {return(5)}
  if (3 %in% count) {return(4)}
  if (sum(count==2)==2) {return(3)}
  if (2 %in% count) {return(2)}
  if (1 %in% count) {return(1)}
  return(0)
}
get_hand_strength <- function(hands) {
  
  map_dbl(hands, function(hand) {
    cards = str_split(hand,"", simplify = T)
    count = table(cards)
    type = get_hand_type(count)
    values = recode(cards, "A"="14", "K"="13","Q"="12","J"="11","T"="10") %>% as.integer()
    strength = (15^5)*type + sum((15^(4:0))*values)
    strength
  })
}

hands = hands %>% 
  mutate(strength= get_hand_strength(hand)) %>% 
  arrange(strength) %>% 
  mutate(rank=row_number(),
         win = rank * bid)

sum(hands$win)

# Part 2 ####
get_hand_strength2 <- function(hands) {
  
  map_dbl(hands, function(hand) {
    
    Js= str_count(hand, "J")
    if(Js==5) {return((15^5)*7)}
    cards = str_split(hand,"", simplify = T)
    count = table(cards)
    if(Js>0) {
      count['J']<-0
      count <- sort(count,decreasing = T)
      count[1] <- count[1]+Js
    }
    type = get_hand_type(count)
    values = recode(cards, "A"="14", "K"="13","Q"="12","J"="0","T"="10") %>% as.integer()
    strength = (15^5)*type + sum((15^(4:0))*values)
    strength
  })
}

hands2 = hands %>% 
  mutate(strength= get_hand_strength2(hand)) %>% 
  arrange(strength) %>% 
  mutate(rank=row_number(),
         win = rank * bid)

sum(hands2$win)


