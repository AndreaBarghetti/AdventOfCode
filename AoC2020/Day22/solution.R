library(tidyverse)


input22 <- read_lines("Day22/input22.txt")

# * - part 1 - *####
deck1 <- input22[2:26] %>% as.numeric()
deck2 <- input22[29:53] %>% as.numeric()

play_game <- function(deck1, deck2) {
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
end <- play_game(deck1, deck2)
sum(end*seq_along(end) %>% sort(decreasing = T))

# * - part 2 - *####
deck1 <- c(9,2,6,3,1)
deck2 <- c(5,8,4,7,10)

deck1 <- c(2,43,19)
deck2 <- c(3,2,29,14)

maxdepth <- 0
depth <- 0

play_rgame <- function(deck1, deck2, subgame=F) {
  
  if (!exists("i")) {i <- 0}
  
  if (!exists("past_decks1")) {past_decks1 <- list()}
  if (!exists("past_decks2")) {past_decks2 <- list()}
  
  while(length(deck1)!=0 & length(deck2)!=0) {
    
    i<-i+1
    
    cat(paste("round",i, "\n"))
    cat("deck1:", deck1, "\ndeck2:",deck2, "\n")
    
    if (any(map2_lgl(past_decks1,past_decks2, function(d1,d2) identical(d1,deck1) & identical(d2,deck2)))) {
      win <- "deck1"
      assign("win", "deck1", pos = parent.frame(n = 1))
      print("looped")
      break
    }
    
    past_decks1[[length(past_decks1)+1]] <- deck1
    past_decks2[[length(past_decks2)+1]] <- deck2
    
    c1 <- deck1[1]
    c2 <- deck2[1]
    
    if (c1>c2) {
      win <- "deck1"
    }
    else if (c2>c1) {
      win <- "deck2"
    }
    
    else {stop("tie?")}
    
    cat(paste("t0:", win, "\n"))
    
    if (c1<=(length(deck1)-1) & c2 <= (length(deck2)-1)) {
      # subgame
      assign("depth", depth+1, envir = .GlobalEnv)
      assign("maxdepth", max(depth, maxdepth), envir = .GlobalEnv)
      play_rgame(deck1[2:(c1+1)], deck2[2:(c2+1)], subgame = T)
    }
    
    cat(paste("t1:", win, "\n"))
    
    deck1 <- deck1[-1]
    deck2 <- deck2[-1]
    
    if (win=="deck2") {
      get <- c(c2, c1)
      deck2 <- append(deck2, get)
    }
    
    else if (win=="deck1") {
      get <- c(c1, c2)
      deck1 <- append(deck1, get)
    }
    
    else {stop("no winner ?")}
    
  }
  
  if (length(deck1)==0) {
    cat("deck1 is 0 \n")
    assign("win", "deck2", pos = parent.frame(n = 1))
  }
  
  if (length(deck2)==0) {
    cat("deck2 is 0 \n")
    assign("win", "deck1", pos = parent.frame(n = 1))
  }
  
  print(paste(win, "won"))
  
  assign("depth", depth-1, envir = .GlobalEnv)
  
  if (!subgame) {
    winner <- c(deck1, deck2)
    return(winner)
  }
}

maxdepth
depth

end <- play_rgame(deck1, deck2)
sum(end*seq_along(end) %>% sort(decreasing = T))
