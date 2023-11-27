library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 22, .fun = read_lines)

parse_input <- function(input) {
  input = str_replace_all(input, " (?!-*\\d)","_")
  map(input, function(input) {
    fun = str_extract(input, "[a-z_]+")
    if (fun == "cut"){fun="cut_deck"}
    arg = str_extract(input, "-*\\d+") %>% as.integer()
    list(fun=fun, arg=arg)
  })
}

instructions <- parse_input(input)

# part 1 ####
Card = R6::R6Class('Card',
                   public = list(
                     card = NA,
                     pos = NA,
                     deck_size = NA,
                     
                     initialize = function(card, deck_size) {
                       self$card <- card
                       self$pos <- card
                       self$deck_size <- deck_size
                     },
                     
                     deal_with_increment = function(n) {
                       self$pos <- (self$pos * n)%%self$deck_size
                     },
                     
                     cut_deck = function(n) {
                       self$pos <- (self$pos - n)%%self$deck_size 
                     },
                     
                     deal_into_new_stack = function(...) {
                         self$pos <- (self$deck_size -1 -self$pos) 
                     },
                     
                     shuffle = function(instructions) {
                       walk(instructions, ~{
                         self[[.x$fun]](.x$arg)
                       })
                     }
                     
                   )
)

card = Card$new(card=2019,10007)
card$shuffle(instructions)
card$pos

# part 2 ####

# I read this solution from internet
# first explanations from this guy: https://mislavzanic.com/blog/advent-of-code-2019-22
# and more help from this guys https://github.com/mpjdem/adventofcode2019
# and this https://codeforces.com/blog/entry/72593
# yet I don't really understand it entirely 

Card2 = R6::R6Class('Card2',
                   public = list(
                     card = NA,
                     deck_size = NA,
                     a = 1,
                     b = 0,
                     
                     initialize = function(card, deck_size) {
                       self$card <- card
                       self$deck_size <- as.numeric(deck_size)
                     },
                     
                     deal_with_increment = function(n) {
                         p <- gmp::powm(n, self$deck_size - 2, self$deck_size)
                         self$a <- self$a * p
                         self$b <- self$b * p
                     },
                     
                     cut_deck = function(n) {
                         self$b <- self$b + n
                     },
                     
                     deal_into_new_stack = function(...) {
                         self$b <- self$b + 1
                         self$a <- self$a * -1
                         self$b <- self$b * -1 
                     },
                     
                     unshuffle = function(instructions) {
                       
                       walk(rev(instructions), ~{
                         self[[.x$fun]](.x$arg)
                         self$a <- self$a%%self$deck_size
                         self$b <- self$b%%self$deck_size
                       })
                       
                     }
                     
                   )
)

###
card = Card2$new(card=2020,119315717514047)
reps = 101741582076661
card$unshuffle(instructions)

((gmp::powm(card$a, reps, card$deck_size) * card$card) +
    (card$b * (gmp::powm(card$a, reps, card$deck_size) + card$deck_size - 1) *
       (gmp::powm(card$a - 1, card$deck_size - 2, card$deck_size)))
) %% card$deck_size

