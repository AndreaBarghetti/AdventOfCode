library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 2, .fun = read_lines)
program <- input %>% str_split(",", simplify = T) %>% as.integer()

# Part 1 ####
Computer <- R6::R6Class("Computer",
                        
                        public = list(
                          i = NA,
                          status = NA,
                          program = NA,
                          
                          initialize = function(program) {
                            self$program <- zeroindex(as.numeric(program))
                            self$i <- 0
                            self$status <- "init"
                          },
                          
                          
                          get_value = function(param) {
                            
                            value <- self$program[param]
                            
                            return(value)
                          },
                          
                          read_code = function(input=NULL) {
                            
                            repeat {
                              
                              op <- self$program[self$i]
                              
                              p1 <- self$program[self$i+1]
                              p2 <- self$program[self$i+2]
                              p3 <- self$program[self$i+3]
                              
                              v1 <- self$get_value(p1)
                              v2 <- self$get_value(p2)
                              v3 <- self$get_value(p3)
                              
                              # operations 
                              
                              if (op == 1) {
                                self$program[p3] <- v1+v2
                                self$i <- self$i + 4
                              } else if (op == 2) {
                                self$program[p3] <- v1*v2
                                self$i <- self$i + 4
                              } else if (op == c(99)) {
                                # message("program run succesfully")
                                self$status <- "finished"
                                return(self$program[0])
                              } else {
                                stop(paste(self$program[self$i], "Error: ",op,"is not a valid op" ))
                              }
                            }
                          },
                          
                          run = function(input=NULL) {
                            self$status <- "running"
                            if(self$status == "finished") {return(self$program[0])}
                            self$read_code(input)
                          },
                          
                          print = function() {
                            cat("i:", self$i, "; ",
                                "status:", self$status, "\n")
                          }
                          
                        ))

comp <- Computer$new(replace(program, 2:3,c(12,2)))
comp$run()


# Part 2 ####
run_program <- function(program, noun, verb) {
  program = replace(program, 2:3,c(noun,verb))
  comp <- Computer$new(program)
  comp$run()
}

find_nv <- function(program, exp_output) {
  for (n in 0:99) {
    for(v in 0:99) {
      if (run_program(program,n,v)==exp_output) {
        return(list(n=n,v=v, result = 100*n+v))
      }
    }
  }
}

find_nv(program,19690720)
