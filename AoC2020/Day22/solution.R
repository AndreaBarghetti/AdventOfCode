library(tidyverse)

input <- read_lines("AoC2020/Day22/input.txt")

# * - part 1 - *####
deck1 <- input[(which(input=="Player 1:")+1):(which(input=="")-1)] %>% as.numeric()
deck2 <- input[(which(input=="Player 2:")+1):length(input)] %>% as.numeric()

play_combat <- function(deck1, deck2) {
  while(length(deck1)!=0 & length(deck2)!=0) {
    
    c1 <- deck1[1]
    c2 <- deck2[1]
    get <- c(c1, c2) %>% sort(decreasing = T)
    deck1 <- deck1[-1]
    deck2 <- deck2[-1]
    
    if (c1>c2) {deck1 <- append(deck1, get)}
    else if (c1<c2) {deck2 <- append(deck2, get)}
    else {stop("there was a tie!")}
  }
  winner <- c(deck1, deck2)
  return(winner)
}
end <- play_combat(deck1, deck2)
sum(end*seq_along(end) %>% sort(decreasing = T))

# * - part 2 - *####
library(collections)

play_rcombat = function(deck1, deck2) {

  game = list(deck1 = deck1,
              deck2 = deck2,
              hist = collections::dict(),
              winner=0)
  
  while (game$winner==0) {
    
    # avoid inf recursion
    status=list(game$deck1,game$deck2)
    
    if (game$hist$has(status)) {
      message('inf rec avoided!!!')
      game$winner=1
      return(game)
    } else {
      game$hist$set(status,T)
    }
    
    next_round = play_round(game)
    game$deck1 = next_round$deck1
    game$deck2 = next_round$deck2

    if (length(game$deck1)<=0) {game$winner=2}
    if (length(game$deck2)<=0) {game$winner=1}
  }
  game
}

play_round = function(game) {
  
  deck1 = game$deck1
  deck2 = game$deck2
  
  if (length(deck1)==0 | length(deck2)==0) {return(game)}

  # deal cards
  c1 = deck1[1]; deck1=deck1[-1]
  c2 = deck2[1]; deck2=deck2[-1]
  
  # no recursive combat:
  if (c1>length(deck1) | c2>length(deck2) ) {
    winner = ifelse(c1>c2,1,2)
  } else {
    # recursive combat:
    winner = play_rcombat(deck1[1:c1], deck2[1:c2])$winner
  }
  
  if (winner==1) {
    bottom_cards = c(c1,c2)
    deck1 = c(deck1, bottom_cards)
  } else {
    bottom_cards = c(c2,c1)
    deck2 = c(deck2, bottom_cards)
  }
  
  game$deck1 = deck1
  game$deck2 = deck2
  
  return(game)
}

endgame = play_rcombat(deck1,deck2)
deck = endgame[[endgame$winner]]
sum(deck * length(deck):1)
