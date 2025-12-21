library(R6)
library(tidyverse)
codes <- read_lines("AoC2024/Day21/input.txt")

Door = R6::R6Class("Door", 
                   public = list(
                     
                     code = NA,
                     pos = NA,
                     
                     initialize = function(pad){
                       self$pad = pad
                       self$pos = which(pad=='A', arr.ind = T)
                     },
                     
                     test_pos = function(){}
                   ))

RobotArm = R6::R6Class("RobotArm", 
                       public = list(
                         
                         pad = NA,
                         pos = NA,
                         
                         initialize = function(pad){
                           self$pad = pad
                           self$pos = which(pad=='A', arr.ind = T)
                         },
                         
                         test_pos = function(){
                           if (any(self$pos<=0) | any(self$pos>dim(self$pad))) {return(F)}
                           if (is.na(self$pad[self$pos])) {return(F)}
                           return(T)
                         },
                         
                         activate = function() {
                           if (self$test_pos()){
                             return(self$pad[self$pos]) 
                           } else {return("#")}
                         },
                         
                         move = function(dir) {
                           if (dir=='A') { return(self$activate()) }
                           if (dir=='<') { self$pos=self$pos+c(0,-1) }
                           if (dir=='>') { self$pos=self$pos+c(0,1) }
                           if (dir=='^') { self$pos=self$pos+c(-1,0) }
                           if (dir=='v') { self$pos=self$pos+c(1,0) }
                         }
                         
                       ))

num_keypad = RobotArm$new(matrix(c("7","4","1",NA,"8","5","2","0","9","6","3","A"), nrow = 4))

num_keypad$move('A')
num_keypad$pos
num_keypad$pad
# Part 1 ####

# Part 2 ####
