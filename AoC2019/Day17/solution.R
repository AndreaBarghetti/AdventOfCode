library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 17, .fun = read_lines)
program <- input %>% str_split(",", simplify = T) %>% as.numeric()

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
                                self$i <- self$i + 2
                              }
                              
                            } else if (op == 4) {
                              self$i <- self$i + 2
                              out = as.numeric(v1)
                              if (save) {
                                self$outputs <- c(self$outputs, out)
                              }
                              if (print) {
                                cat(intToUtf8(out))
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
                              out = self$step(input,save=save, print=print)
                              input<-NULL
                            }
                          },
                          
                          print = function() {
                            cat("i:", self$i, "; ",
                                "status:", self$status, "\n")
                          }
                          
                        )
)

read_camera <- function(computer) {
  
  image = capture.output(
    computer$outputs %>%
      as.integer() %>%
      intToUtf8() %>% cat()
  )
  
  # image <- image[-length(image)]
  image
}

img_to_mat <- function(image) {
  str_split(image,"", simplify = T)
} 

mark_intersections <- function(mat) {
  
  for (x in 2:(ncol(mat)-1)) {
    for (y in 2:(nrow(mat)-1)) {
      C = mat[y,x]=='#'
      U = mat[y-1,x]=='#'
      L = mat[y,x-1]=='#'
      R = mat[y,x+1]=='#'
      D = mat[y+1,x]=='#'
      
      if(all(c(C,U,L,R,D))){mat[y,x]<-'O'}
    }
  }
  
  mat
}

count_intersection <- function(mat) {
 
  count = 0
  
  for (x in 2:(ncol(mat)-1)) {
    for (y in 2:(nrow(mat)-1)) {
      if(mat[y,x]=='O') { count <- count +(x-1)*(y-1)}
    }
  }
  
  count
}

# part 1 ####
computer <- Computer$new(program)
computer$run()

(image<-read_camera(computer))

img_to_mat(image) %>% 
mark_intersections() %>% 
count_intersection()

img_to_mat(image) %>% 
  mark_intersections() %>% 
  plot_matrix() +
  ggplot2::geom_tile(data=. %>% filter(value=="#"),
                     col='black',fill='transparent',
                     show.legend = F)


# part 2 ####

# get the directions
Vcleaner <- R6::R6Class('Vcleaner', 
                        public = list(
                          map = NA,
                          pos = NA,
                          dirs = circular(c("U","R","D","L")),
                          dir=1,
                          actions=character(0),
                          steps=integer(0),
                          result=NA,
                          
                          initialize = function(image){
                            
                            map <- img_to_mat(image) %>% mat_expand(n=1,fill=".")
                            self$map <- map
                            self$pos = which(map=="^", arr.ind = T)
                            
                          },
                          
                          look = function(side=c("S","L","R")) {
                            side = match.arg(side)
                            if (side=="S") {dir = self$dirs[self$dir]} else {
                              dir = self$dirs[self$dir+ifelse(side=="L",-1,1)]
                            }
                            
                            if (dir == "U") {pos <- self$pos + c(-1,0)}
                            if (dir == "D") {pos <- self$pos + c(1,0)}
                            if (dir == "L") {pos <- self$pos + c(0,-1)}
                            if (dir == "R") {pos <- self$pos + c(0,+1)}
                            self$map[pos]
                          },
                          
                          turn = function(side){
                            if (side=="L"){self$dir=self$dir-1}
                            if (side=="R"){self$dir=self$dir+1}
                            self$steps <- 0
                          },
                          
                          move_fw = function() {
                            dir=self$dirs[self$dir]
                            if (dir == "U") {self$pos <- self$pos + c(-1,0)}
                            if (dir == "D") {self$pos <- self$pos + c(1,0)}
                            if (dir == "L") {self$pos <- self$pos + c(0,-1)}
                            if (dir == "R") {self$pos <- self$pos + c(0,1)}
                            self$steps <- self$steps + 1
                          },
                          
                          move = function(){
                            
                            if (self$look()=="#"){
                              self$move_fw()
                            } else if (self$look("L")=="#"){
                              self$actions <- c(self$actions, self$steps, "L")
                              self$turn("L")
                            } else if (self$look('R')=="#") {
                              self$actions <- c(self$actions, self$steps, "R")
                              self$turn("R")
                            } else {
                              self$actions <- c(self$actions, self$steps)
                              return(T)
                              }
                            return(F)
                          },
                          
                          run = function() {
                            while (!self$move()) {
                              
                            }
                            message("finished")
                            self$result <- self$actions
                            return(self$result)
                          }
                          
                        )
)

vcleaner <- Vcleaner$new(image)

directions <- vcleaner$run()

directions

# split according to ABC rules
split_double_digit <- function(x){
  map_chr(x, function(x){
    if(nchar(x)==1){return(as.character(x))}
    int =as.integer(x)
    paste0(split_double_digit(int%/%2),
           split_double_digit(int%/%2+int%%2), "")
  })
}

make_program <- function(directions) {
  
  directions <- split_double_digit(directions) %>% 
    paste0(collapse = "")
  
  ABC_pattern = "^(.{3,10})\\1*(.{3,10})(\\1|\\2)*(.{3,10})(\\1|\\2|\\4)*$"
  
  # split double digits
  directions = str_replace_all(directions, c("10"="55","12"="66"))
  
  get_ABC <- function(directions, ABC_pattern) {
    if (!str_detect(directions, ABC_pattern)) {return()}
    ABC <- str_replace(directions, ABC_pattern, "\\1 \\2 \\4") %>% 
      str_split(" ") %>% unlist()
    if (sum(str_count(directions, ABC))>10) {return()}
    
    main = str_locate_all(directions, ABC) %>% 
      setNames(c('A','B','C')) %>%
      map_dfr(as_tibble, .id = "p") %>% 
      arrange(start) %>% 
      pull(p) %>% 
      paste(collapse = ",") %>% 
      paste0("\n")
    
    ABC <- ABC %>% str_split("") %>% 
      map(paste0, collapse = ",") %>% 
      map(paste0, "\n") %>% 
      setNames(c("A","B","C"))
    
    c(main=main, ABC)
  }
  
  get_ABC(directions,ABC_pattern)
}

robot_inputs <- make_program(directions)


# operate the V cleaner
Robot <- R6::R6Class("Robot", 
                     public = list(
                       
                       computer=NA,
                       
                       initialize = function(program) {
                         self$computer = Computer$new(program)
                       },
                       
                       run = function(input=NULL) {
                         
                         if (!is.null(input)) {
                           input <- utf8ToInt(input)
                         }
                         self$computer$run(input)
                       },
                       
                       mrun = function(input, save=T, print=F) {
                         input = utf8ToInt(input)
                         
                         for (inp in input) {
                           self$computer$run(inp, print = print, save=save)
                         }
                         
                       },
                       
                       print = function() {
                         print(read_camera(self$computer))
                       }
                       
                     )
)

robot <- Robot$new(replace(program,1,2))
robot$run()

# run it
robot$mrun(robot_inputs$main, save = F, print = T)
robot$mrun(robot_inputs$A, save = F, print = T)
robot$mrun(robot_inputs$B, save = F, print = T)
robot$mrun(robot_inputs$C, save = F, print = T)
robot$mrun("n\n", save = T, print = F)

rev(robot$computer$outputs)[1]
