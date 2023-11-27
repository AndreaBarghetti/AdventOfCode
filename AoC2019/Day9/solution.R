library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 9, .fun = read_lines)

program <- input %>% str_split(",", simplify = T) %>% as.integer()

# part 1 ####
infinite <- function(x, default) {
  obj <- x
  class(obj) <- c("infinite", class(obj))
  attr(obj, "default") <- default
  return(obj)
}

# return default when trying to read a non existing index
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

computer <- R6::R6Class("computer",
                         
                         public = list(
                           i = NA,
                           status = NA,
                           program = NA,
                           rel_base = NA,
                           outputs = NA,
                           
                           initialize = function(program) {
                             self$program <- zeroindex(infinite(as.numeric(program),0))
                             self$i <- 0
                             self$status <- "init"
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
                             
                             repeat {

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
                                 self$program[p1] <- input[1]
                                 input <- input[-1]
                                 self$i <- self$i + 2
                               } else if (op == 4) {
                                 self$i <- self$i + 2
                                 self$outputs <- c(self$outputs, as.numeric(v1))
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
                                 message("program run succesfully")
                                 self$status <- "finished"
                                 return(self$outputs)
                               } else {
                                 stop(paste(self$program[self$i], "Error: ",op,"is not a valid op" ))
                               }
                             }
                           },
                           
                           run = function(input) {
                             if(self$status == "finished") {return(self$outputs)}
                             self$read_code(input)
                           },
                           
                           print = function() {
                             cat("i:", self$i, "; ",
                                 "status:", self$status, "\n")
                           }
                           
                         ))


comp <- computer$new(program)
comp$run(1)

# part 2 ####
comp <- computer$new(program)
comp$run(2)
