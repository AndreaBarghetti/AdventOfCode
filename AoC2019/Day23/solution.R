library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 23, .fun = read_lines)
program <- input %>% str_split(",", simplify = T) %>% as.numeric()

source('Day23/intcode_computer.R')

Network = R6::R6Class('Network',
                      public = list(
                        
                        computers = list(),
                        queues = list(),

                        initialize = function(program, n) {
                          for (i in 1:n) {
                            self$computers[[i]] <- Computer$new(program)
                            self$computers[[i]]$run(i-1)
                          }
                          names(self$computers) <- as.character(0:(n-1))
                          
                          self$queues <- map(1:n,~list())
                          names(self$queues) <- as.character(0:(n-1))
                          
                        },
                        
                        run = function() {
                          walk(self$queues, function(q){
                            order = order(q %>% map_int(~.x$time))
                            q <- q[order]
                          })

                          inputs = map(self$queues, ~{
                            input = ifelse(length(.x)!=0,.x[[1]],-1)
                          })

                          iwalk(inputs, function(input, comp) {
                            if (length(input[[1]]) == 1) {
                              self$computers[[comp]]$run(input)
                            } else {
                              self$queues[[comp]] <- self$queues[[comp]][-1]
                              self$computers[[comp]]$run(input[[1]][1])
                              self$computers[[comp]]$run(input[[1]][2])
                            }
                            
                            out = self$computers[[comp]]$outputs
                            times = self$computers[[comp]]$output_times
                            self$computers[[comp]]$outputs <- list()
                            self$computers[[comp]]$output_times <- list()
                            
                            while (length(out)>0) {
                              to = as.character(out[[1]])
                              time = times[[1]]
                              if(out[[1]] == 255){
                                message("address 255. Y:",out[[3]])
                                stop()
                                }
                              packet = list(values = unlist(out[2:3]), time=time)
                              out = out[-c(1:3)]
                              time = time[-c(1:3)]
                              self$queues[[to]] <- c(self$queues[[to]],list(packet))
                            }
                          })
                        }
                      )
)

network = Network$new(program, 50)
repeat{network$run()}

# part 2 ####
Network = R6::R6Class('Network',
                      public = list(
                        
                        computers = list(),
                        queues = list(),
                        NAT = list(last_sent=list(), packet=list()),

                        initialize = function(program, n) {
                          for (i in 1:n) {
                            self$computers[[i]] <- Computer$new(program)
                            self$computers[[i]]$run(i-1)
                          }
                          names(self$computers) <- as.character(0:(n-1))
                          
                          self$queues <- map(1:n,~list())
                          names(self$queues) <- as.character(0:(n-1))
                          
                        },
                        
                        run = function() {
                          
                          if(all(map_int(self$queues, length)==0)) {
                            message('Idle')
                              self$queues[['0']] <- self$NAT$packet
                              
                              if (!is_empty(self$NAT$last_sent)) {
                                
                                if(identical(self$NAT$last_sent[[1]]$values, self$NAT$packet[[1]]$values)) {
                                  message("RESULT: ", self$NAT$packet[[1]]$values[2])
                                  stop()
                                }
                              }
                              
                              self$NAT$last_sent <- self$NAT$packet
                          }
                          
                          walk(self$queues, function(q){
                            order = order(q %>% map_int(~.x$time))
                            q <- q[order]
                          })
                          
                          inputs = map(self$queues, ~{
                            input = ifelse(length(.x)!=0,.x[[1]],-1)
                          })
                          
                          iwalk(inputs, function(input, comp) {
                            if (length(input[[1]]) == 1) {
                              self$computers[[comp]]$run(input)
                            } else {
                              self$queues[[comp]] <- self$queues[[comp]][-1]
                              self$computers[[comp]]$run(input[[1]][1])
                              self$computers[[comp]]$run(input[[1]][2])
                            }
                            
                            out = self$computers[[comp]]$outputs
                            times = self$computers[[comp]]$output_times
                            self$computers[[comp]]$outputs <- list()
                            self$computers[[comp]]$output_times <- list()
                            
                            while (length(out)>0) {
                              to = as.character(out[[1]])
                              time = times[[1]]
                              packet = list(values = unlist(out[2:3]), time=time)
                              
                              #NAT
                              if(to == '255'){
                                message("address 255. Y:",out[[3]])
                                self$NAT$packet <- list(packet)
                              } else {
                                self$queues[[to]] <- c(self$queues[[to]],list(packet))
                              }
                              out = out[-c(1:3)]
                              time = time[-c(1:3)]

                            }
                          })
                        }
                      )
)

self = Network$new(program, 50)
repeat{self$run()}

