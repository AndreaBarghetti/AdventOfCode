library(tidyverse)

input8 <- readLines("Day8/input8.txt")

# * - part 1 - *####
code <- lapply(input8, function(x) {
  out <- x %>%
    str_split(" ")
})

instructions <- sapply(code, function(x) {x[[1]][1]})
arguments <- sapply(code, function(x) {x[[1]][2] %>% as.numeric()})

i <- 1
accumulator <- 0
ilist <- vector(mode = "numeric")

run_program <- function(instructions, arguments) {
  while(!i %in% ilist) {
    
    ilist <- append(ilist, i)
    instruction <- instructions[i]
    
    if(i > length(instructions)) {return(paste("terminated with accumulator = ", accumulator))}
    
    if(instruction == "acc") {
      accumulator <- accumulator + arguments[i]
      i <- i + 1
      next()
    }
    
    else if(instruction == "jmp") {
      i <- i + arguments[i]
      next()
    }
    
    else if(instruction == "nop") {
      i <- i+1
      next()
    }
  }
  return(paste("endless loop started with accumulator=", accumulator))
}

run_program(instructions, arguments)

# * - part 2 - *####
try_fix_at <- grep(instructions, pattern = "nop|jmp")

for(pos in try_fix_at) {
  new_instructions <- instructions
  new_instructions[pos] <- new_instructions[pos] %>%
    recode("nop"="jmp", "jmp"="nop")
  
  output <- run_program(new_instructions, arguments)
  
  if (str_detect(output, "terminated")) {
    print(paste(output))
    break()
  }
}
