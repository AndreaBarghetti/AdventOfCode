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

Deck <- R6::R6Class("Deck",
                    public = list(
                      deck = NA,
                      size = NA,
                      
                      initialize = function(size) {
                        self$size = size
                        self$deck = 0:(size-1)
                        invisible(self)
                      },
                      
                      deal_with_increment = function(n) {
                        seq = (seq(from=1,by=n, length.out=self$size)-1)%%self$size+1
                        self$deck <- self$deck[order(seq)]
                        invisible(self)
                      },
                      
                      cut_deck = function(n) {
                        if (n<0) {n <- self$size+n}
                        if(n==0) {invisible(NULL)}
                        top = 1:n
                        bottom = (n+1):self$size
                        self$deck <- self$deck[c(bottom, top)]
                        invisible(self)
                      },
                      
                      deal_into_new_stack = function(...) {
                        self$deck <- rev(self$deck)
                        invisible(self)
                      },
                      
                      print = function(){
                        cat(head(self$deck,5),"...",tail(self$deck,5))
                      }
                    )
)

deck <- Deck$new(10007)

walk(instructions, ~{
  deck[[.x$fun]](.x$arg)
})

which(deck$deck==2019)-1
