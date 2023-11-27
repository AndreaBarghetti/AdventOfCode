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
                          i = 0,
                          status = NA,
                          program = NA,
                          rel_base = 0,
                          outputs = list(),
                          time=0,
                          output_times=list(),
                          
                          initialize = function(program) {
                            self$program <- zeroindex(infinite(as.numeric(program),0))
                            self$status <- "running"
                          },
                          
                          empty_outputs = function() {
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
                          
                          step = function(input=NULL, save=T, print=F) {
                            
                            self$time <- self$time+1
                            
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
                                # message("input required")
                              } else {
                                self$status <- "running"
                                self$program[p1] <- input
                                self$i <- self$i + 2
                              }
                              
                            } else if (op == 4) {
                              self$i <- self$i + 2
                              out = as.numeric(v1)
                              if (save) {
                                self$outputs <- c(self$outputs, out)
                                self$output_times <- c(self$output_times, self$time)
                              }
                              if (print) {
                                cat(out,"\n")
                              }
                              return(out)
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
                              # return(0)
                            } else {
                              stop(paste(self$program[self$i], "Error: ",op,"is not a valid op" ))
                            }
                          },
                          
                          # to run until new input is required
                          run = function(input=NULL,save=T, print=F) {
                            self$status <- "running"
                            
                            while (self$status == "running") {
                              out = self$step(input, save=save, print=print)
                              input<-NULL
                            }
                          },
                          
                          print = function() {
                            cat("i:", self$i, "; ",
                                "status:", self$status, "\n")
                          }
                          
                        )
)


NTComputer <- R6::R6Class("NTComputer",
                     public = list(
                       
                       computer=NA,
                       time=0,
                       
                       initialize = function(program, address) {
                         computer = Computer$new(program)
                         computer$run(address)
                         self$computer <- computer
                       }
                       
                     )
)
