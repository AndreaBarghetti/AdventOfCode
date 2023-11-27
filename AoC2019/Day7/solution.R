library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 7, .fun = read_lines)

program <- input %>% str_split(",", simplify = T) %>% as.integer()

# part 1 ####
phase_sets <- combinat::permn(0:4)

read_instruction <- function(x) {
  (x +100000) %>% str_sub(c(5,4,3,2), c(6,4,3,2)) %>% 
    as.integer() %>% setNames(c("op", "m1","m2","m3"))
}

amplifier <- R6::R6Class("amplifier",
                         
                         public = list(
                           i = NA,
                           status = NA,
                           program = NA,
                           
                           initialize = function(program) {
                             self$program <- zeroindex(program)
                             self$i <- 0
                             self$status <- "init"
                           },
                           
                           read_code = function(input) {
                             
                             x = self$program
                             
                             i <- self$i
                             
                             repeat {
                               
                               instruction <- read_instruction(x[i])
                               
                               p1 <- x[i+1]
                               p2 <- x[i+2]
                               p3 <- x[i+3]
                               
                               op <- instruction['op']
                               
                               if (instruction['m1']==0) {
                                 v1 <- x[p1]
                               } else if (instruction['m1']==1) {
                                 v1 <- p1
                               } else {stop("wrong mode")}
                               
                               if (instruction['m2']==0) {
                                 v2 <- x[p2]
                               } else if (instruction['m2']==1) {
                                 v2 <- p2
                               } else {stop("wrong mode")}
                               
                               if (instruction['m3']==0) {
                                 v3 <- x[p3]
                               } else if (instruction['m3']==1) {
                                 v3 <- p3
                               } else {stop("wrong mode")}
                               
                               if (op == 1) {
                                 x[p3] <- v1+v2
                                 i <- i + 4
                               } else if (op == 2) {
                                 x[p3] <- v1*v2
                                 i <- i + 4
                               } else if (op == 3) {
                                 x[p1] <- input[1]
                                 input <- input[-1]
                                 i <- i + 2
                               } else if (op == 4) {
                                 i <- i + 2
                                 self$i <- i
                                 return(as.integer(v1))
                               } else if (op == 5) {
                                 if (v1 != 0) {i <- v2} else {i<-i+3}
                               } else if (op == 6) {
                                 if (v1 == 0) {i <- v2} else {i<-i+3}
                               } else if (op == 7) {
                                 if (v1 < v2) {x[p3] <- 1} else {x[p3] <- 0}
                                 i <- i + 4
                               } else if (op == 8) {
                                 if (v1 == v2) {x[p3] <- 1} else {x[p3] <- 0}
                                 i <- i + 4
                               } else if (op == c(99)) {
                                 message("program run succesfully")
                                 self$status <- "finished"
                                 self$i <- i
                                 return(input)
                                 break
                               } else {
                                 stop(paste(x[i], "Error: ",op,"is not a valid op" ))
                               }
                             }
                           },
                           
                           run = function(input) {
                             self$status <- "running"
                             if(self$status == "finished") {return(input)}
                             self$read_code(input)
                           },
                           
                           print = function() {
                             cat("i:", self$i, "; ",
                                 "status:", self$status,";",
                                 "output:", self$output, "\n")
                           }
                           
                         ))

run_amplifiers <- function(program, phase_set) {
  amp1 <- amplifier$new(program)
  amp2 <- amplifier$new(program)
  amp3 <- amplifier$new(program)
  amp4 <- amplifier$new(program)
  amp5 <- amplifier$new(program)
  
  out <- 0
  out <- amp1$run(c(phase_set[1],out))
  out <- amp2$run(c(phase_set[2],out))
  out <- amp3$run(c(phase_set[3],out))
  out <- amp4$run(c(phase_set[4],out))
  out <- amp5$run(c(phase_set[5],out))
  
  return(out)
}

test=c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
run_amplifiers(test, c(4,3,2,1,0)) == 43210

results <- map_dbl(phase_sets, function(phase_set) {
  run_amplifiers(program, phase_set)
})

max(results)

# part 2 ####
phase_sets2 <- combinat::permn(5:9)

run_amplifiers2 <- function(program, phase_set) {
  amp1 <- amplifier$new(program)
  amp2 <- amplifier$new(program)
  amp3 <- amplifier$new(program)
  amp4 <- amplifier$new(program)
  amp5 <- amplifier$new(program)
  
  out <- 0
  out <- amp1$run(c(phase_set[1],out))
  out <- amp2$run(c(phase_set[2],out))
  out <- amp3$run(c(phase_set[3],out))
  out <- amp4$run(c(phase_set[4],out))
  out <- amp5$run(c(phase_set[5],out))
  
  while((amp1$status != "finished")) {
    out <- amp1$run(out)
    out <- amp2$run(out)
    out <- amp3$run(out)
    out <- amp4$run(out)
    out <- amp5$run(out)
  }
  return(out)
}

results <- map_dbl(phase_sets2, function(phase_set) {
  run_amplifiers2(program, phase_set)
})
max(results)
