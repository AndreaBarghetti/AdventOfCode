library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 13, .fun = read_lines)
program <- input %>% str_split(",", simplify = T) %>% as.numeric()

# part 1 ####
infinite <- function(x, default) {
  obj <- x
  class(obj) <- c("infinite", class(obj))
  attr(obj, "default") <- default
  return(obj)
}

`[.infinite` <- function(x, i) {
  
  if (i > length(x)) {
    res <- attr(x, "default")
  } else { res <- NextMethod("[") }
  if (i <= 0) {
    return(attr(x, "default"))}
  if (is.na(res)) {
    return(attr(x, "default"))}
  if (is_empty(res)) {
    return(attr(x, "default"))}
  return(res)
}

Computer <- R6::R6Class("Computer",
                        
                        public = list(
                          i = NA,
                          status = NA,
                          program = NA,
                          rel_base = NA,
                          outputs = NA,
                          
                          initialize = function(program) {
                            self$program <- zeroindex(infinite(as.numeric(program),0))
                            self$i <- 0
                            self$status <- "running"
                            self$rel_base <- 0
                            self$outputs <- list()
                          },
                          
                          
                          read_instruction = function(instr) {
                            (instr +100000) %>% str_sub(c(5,4,3,2), c(6,4,3,2)) %>% 
                              as.integer() %>% setNames(c("op", "m1","m2","m3"))
                          },
                          
                          get_param = function(mode, n) {
                            param <- self$program[self$i+n]
                            if (mode == 2) {
                              param <- param + self$rel_base
                            }
                            param
                          },
                          
                          get_value = function(mode,param) {
                            
                            if (mode==0) {
                              value <- self$program[param]
                            } else if (mode==1) {
                              value <- param
                            } else if(mode==2) {
                              value <- self$program[param]
                            } else {stop("wrong mode")}
                            
                            return(value)
                          },
                          
                          read_code = function(input=NULL) {
                            
                            instruction <- self$read_instruction(self$program[self$i])
                            
                            op <- instruction['op']
                            
                            p1 <- self$get_param(instruction['m1'],1)
                            p2 <- self$get_param(instruction['m2'],2)
                            p3 <- self$get_param(instruction['m3'],3)
                            
                            v1 <- self$get_value(instruction['m1'], p1)
                            v2 <- self$get_value(instruction['m2'], p2)
                            v3 <- self$get_value(instruction['m3'], p3)
                            
                            # operations 
                            if (op == 1) {
                              self$program[p3] <- v1+v2
                              self$i <- self$i + 4
                            } else if (op == 2) {
                              self$program[p3] <- v1*v2
                              self$i <- self$i + 4
                            } else if (op == 3) {
                              if(is.null(input) | is_empty(input)) {
                                self$status <- "waiting"
                                message("input required")
                              } else {
                                self$status <- "running"
                                self$program[p1] <- input
                                # input <- input[-1]
                                self$i <- self$i + 2
                              }
                              
                            } else if (op == 4) {
                              self$i <- self$i + 2
                              self$outputs <- c(self$outputs, as.numeric(v1))
                              return(as.numeric(v1))
                            } else if (op == 5) {
                              if (v1 != 0) {self$i <- v2} else {self$i <- self$i + 3}
                            } else if (op == 6) {
                              if (v1 == 0) {self$i <- v2} else {self$i <- self$i + 3}
                            } else if (op == 7) {
                              if (v1 < v2) {self$program[p3] <- 1} else {self$program[p3] <- 0}
                              self$i <- self$i + 4
                            } else if (op == 8) {
                              if (v1 == v2) {self$program[p3] <- 1} else {self$program[p3] <- 0}
                              self$i <- self$i + 4
                            } else if (op == 9) {
                              self$rel_base <- self$rel_base + v1
                              self$i <- self$i + 2
                            } else if (op == c(99)) {
                              self$status <- "finished"
                              message("program run succesfully")
                              return(0)
                            } else {
                              stop(paste(self$program[self$i], "Error: ",op,"is not a valid op" ))
                            }
                          },
                          
                          run = function(input=NULL) {
                            self$status <- "running"
                            while (self$status == "running") {
                              self$read_code(input)
                              input=input[-1]
                              }
                          },
                          
                          print = function() {
                            cat("i:", self$i, "; ",
                                "status:", self$status, "\n")
                          }
                          
                        )
)

comp <- Computer$new(program)
comp$run()

sum(instr[,3] == 2)

# part 2 ####
Game <- R6::R6Class("Game",
                    public = list(
                      
                      computer=NA,
                      
                      initialize = function(program) {
                        self$computer <- Computer$new(program)
                      },
                      
                      run = function(input=NULL) {
                        self$computer$run(input)
                      },
                      
                      display = function() {
                        instr = self$computer$outputs %>% as.integer() %>% 
                          matrix(ncol=3,byrow = T ) %>% 
                          `colnames<-`(c("x","y","value")) %>% 
                          as_tibble()
                        
                        score = instr %>%
                          filter(x==-1,
                                 y==0) %>%
                          pull(value) %>% rev() %>% `[`(1)
                        
                        instr <- instr %>% 
                          filter(!(x==-1 &  y==0))
                        
                        as_tibble(instr) %>%
                          mutate(y=y*-1) %>% 
                          ggplot2::ggplot(ggplot2::aes(x = x, y = y)) + 
                          ggplot2::geom_tile(ggplot2::aes(fill = as.factor(value)), show.legend = F) + 
                          ggplot2::theme_void() + 
                          ggplot2::coord_equal() +
                          ggtitle(str_c("SCORE: ", score)) +
                          scale_fill_manual(values=c("gray","black","blue","black","red","yellow"))
                        
                      } 
                    )
)

game <- Game$new(replace(program,1,2))

game$run()
game$display()

go_there = function(game) {
  game$computer$outputs %>% as.integer() %>% 
    matrix(ncol=3,byrow = T ) %>% 
    `colnames<-`(c("x","y","value")) %>% 
    as_tibble() %>% 
    filter(value %in% 3:4) %>% 
    group_by(value) %>% 
    slice_tail(n=1) %>%
    ungroup() %>% 
    arrange(value) %>% 
    pull(x) %>% diff() %>% 
    sign()
}

for (i in 1:1000) {
  suppressMessages({
    game$run(go_there(game))
  })
}
game$display()




