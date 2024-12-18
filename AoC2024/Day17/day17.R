library(tidyverse)
input <- read_lines("AoC2024/Day17/test2.txt")

parse = function(input) {
  parts = str_extract_all(input, "\\d+")
  parts = parts[c(1,2,3,5)] %>% setNames(c('A','B','C','program'))
  parts = map(parts, as.integer)
  parts
}

settings = parse(input)

# Part 1 ####
library(R6)
Computer = R6::R6Class('Computer', 
                       public = list(
                         
                         pointer = 1,
                         A=NA,
                         B=NA,
                         C=NA,
                         program=NA,
                         output = integer(0),
                         
                         initialize = function(A,B,C,program) {
                           self$A = A
                           self$B = B
                           self$C = C
                           self$program = program

                         },
                         
                         combo = function(operand) {
                           if (between(operand,0,3)){return(operand)}
                           if (operand ==4){return(self$A)}
                           if (operand ==5){return(self$B)}
                           if (operand ==6){return(self$C)}
                           if (operand ==7){stop("Operand: 7!!!")}
                           stop(paste0("Operand:",operand, "!?"))
                         },
                         
                         move_p = function() {
                           self$pointer = self$pointer + 2
                         },
                         
                         #opcodes
                         opcodes = c('adv', 'bxl','bst','jnz','bxc','out', 'bdv','cdv'),

                         adv = function(operand) { 
                           self$A = as.integer(self$A / 2^self$combo(operand) )
                           self$move_p()
                           },
                         bdv = function(operand) { 
                           self$B = as.integer(self$A / 2^self$combo(operand) )
                           self$move_p()
                         },
                         cdv = function(operand) { 
                           self$C = as.integer(self$A / 2^self$combo(operand) )
                           self$move_p()
                         },
                         bxl = function(operand) {
                           self$B = bitwXor(self$B, operand)
                           self$move_p()
                         },
                         bst = function(operand) {
                           self$B = self$combo(operand) %% 8
                           self$move_p()
                         },
                         jnz = function(operand) {
                           if (self$A != 0){
                             self$pointer = operand+1
                           } else { self$move_p() }
                         },
                         bxc = function(operand) {
                           self$B = bitwXor(self$B, self$C)
                           self$move_p()
                         },
                         out = function(operand) {
                           o = as.integer(self$combo(operand) %% 8)
                           # cat(o)
                           # cat(',')
                           self$move_p()
                           self$output = c(self$output,o)
                         },
                         
                         do = function() {
                           opcode = self$program[self$pointer]
                           operand = self$program[self$pointer+1]
                           f = self$opcodes[opcode+1]
                           self[[f]](operand)
                         },
                         
                         run = function() {
                           while(self$pointer < length(self$program)) {
                             self$do()
                           }
                           cat('\n')
                           return(paste(self$output, collapse = ","))
                         },
                         
                         print = function() {
                           cat(paste0('pointer: ',self$pointer," "))
                           cat(paste0(self$opcodes[self$program[self$pointer]],"(",self$program[self$pointer+1],")\n"))
                           cat(paste0('A: ',self$A,", "))
                           cat(paste0('B: ',self$B,", "))
                           cat(paste0('C: ',self$B))
                         }
                       )
)

computer = Computer$new(settings$A,
                        settings$B,
                        settings$C,
                        settings$program)

computer$run()

# Part 2 ####





